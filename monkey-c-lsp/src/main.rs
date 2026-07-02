//! A Language Server for Monkey C, exposing the workspace's parser, linter, and formatter over LSP.
//!
//! The server keeps the full text of every open document in memory (full-sync) and re-analyses on
//! open/change, publishing diagnostics. It answers `textDocument/formatting` by re-rendering the
//! document through the formatter.

// `lsp_types::Uri` carries an internal lazy-parse cache (interior mutability) that doesn't affect
// its `Hash`/`Eq`, so it is a safe `HashMap` key despite `clippy::mutable_key_type` flagging every
// map that uses it.
#![allow(clippy::mutable_key_type)]

mod analysis;
mod position;

use std::collections::HashMap;
use std::error::Error;

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
    PublishDiagnostics,
};
use lsp_types::request::{CodeActionRequest, Formatting};
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOptions, CodeActionOrCommand, CodeActionParams,
    CodeActionProviderCapability, DidCloseTextDocumentParams, DocumentFormattingParams, OneOf,
    Position, PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextEdit, Uri, WorkspaceEdit,
};

use crate::position::PositionMapper;

/// Open documents keyed by URI, holding their current full text.
type Documents = HashMap<Uri, String>;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let (connection, io_threads) = Connection::stdio();

    let capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        document_formatting_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
            code_action_kinds: Some(vec![
                CodeActionKind::QUICKFIX,
                CodeActionKind::SOURCE_FIX_ALL,
            ]),
            ..Default::default()
        })),
        ..Default::default()
    })?;

    let _init_params = connection.initialize(capabilities)?;
    run(&connection)?;

    // `connection` must be dropped before joining: its `sender` keeps the writer thread's channel
    // open, so `join` would otherwise block forever.
    drop(connection);
    io_threads.join()?;

    Ok(())
}

fn run(connection: &Connection) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut documents = Documents::new();

    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                if connection.handle_shutdown(&request)? {
                    return Ok(());
                }

                handle_request(connection, &documents, request)?;
            }
            Message::Notification(notification) => {
                handle_notification(connection, &mut documents, notification)?;
            }
            Message::Response(_) => {}
        }
    }

    Ok(())
}

fn handle_request(
    connection: &Connection,
    documents: &Documents,
    request: Request,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let request = match cast_request::<Formatting>(request) {
        Ok((id, params)) => {
            let edits = format(documents, &params);
            respond(connection, id, serde_json::to_value(edits)?)?;
            return Ok(());
        }
        Err(request) => request,
    };

    let request = match cast_request::<CodeActionRequest>(request) {
        Ok((id, params)) => {
            let actions = code_actions(documents, &params);
            respond(connection, id, serde_json::to_value(actions)?)?;
            return Ok(());
        }
        Err(request) => request,
    };

    // Unknown request: reply with an empty success so the client isn't left waiting. Specific
    // method handling is added per request type above.
    respond(connection, request.id, serde_json::Value::Null)?;

    Ok(())
}

fn handle_notification(
    connection: &Connection,
    documents: &mut Documents,
    notification: Notification,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let notification = match cast_notification::<DidOpenTextDocument>(notification) {
        Ok(params) => {
            let uri = params.text_document.uri;
            documents.insert(uri.clone(), params.text_document.text);
            publish(connection, documents, &uri)?;
            return Ok(());
        }
        Err(notification) => notification,
    };

    let notification = match cast_notification::<DidChangeTextDocument>(notification) {
        Ok(params) => {
            // Full sync: the last change carries the whole new text.
            if let Some(change) = params.content_changes.into_iter().next_back() {
                let uri = params.text_document.uri;
                documents.insert(uri.clone(), change.text);
                publish(connection, documents, &uri)?;
            }
            return Ok(());
        }
        Err(notification) => notification,
    };

    if let Ok(params) = cast_notification::<DidCloseTextDocument>(notification) {
        let DidCloseTextDocumentParams { text_document } = params;
        documents.remove(&text_document.uri);
        // Clear diagnostics for the now-closed document.
        send_diagnostics(connection, text_document.uri, Vec::new())?;
    }

    Ok(())
}

/// Analyse the document at `uri` and publish its diagnostics.
fn publish(
    connection: &Connection,
    documents: &Documents,
    uri: &Uri,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let Some(text) = documents.get(uri) else {
        return Ok(());
    };

    let diagnostics = analysis::diagnostics(text);
    send_diagnostics(connection, uri.clone(), diagnostics)
}

fn send_diagnostics(
    connection: &Connection,
    uri: Uri,
    diagnostics: Vec<lsp_types::Diagnostic>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };
    connection.sender.send(Message::Notification(Notification {
        method: PublishDiagnostics::METHOD.to_string(),
        params: serde_json::to_value(params)?,
    }))?;

    Ok(())
}

/// The formatting edits for a document: a single edit replacing the whole document with its
/// formatted text. Returns no edits when the document is unknown, doesn't parse, or is already
/// formatted.
fn format(documents: &Documents, params: &DocumentFormattingParams) -> Vec<TextEdit> {
    let uri = &params.text_document.uri;
    let Some(text) = documents.get(uri) else {
        return Vec::new();
    };

    let Some(formatted) = analysis::format(text) else {
        return Vec::new();
    };

    if formatted == *text {
        return Vec::new();
    }

    let mapper = PositionMapper::new(text);
    vec![TextEdit {
        range: Range {
            start: Position::new(0, 0),
            end: mapper.position(text.len()),
        },
        new_text: formatted,
    }]
}

/// Code actions for a document. Offers, subject to the client's `context.only` filter:
///
/// - one `quickfix` per fixable lint finding overlapping the requested range, each carrying just
///   that finding's fix and the diagnostic it resolves; and
/// - a single `source.fixAll` action bundling every fixable finding, which an editor can run on
///   save to apply all lint fixes at once.
fn code_actions(documents: &Documents, params: &CodeActionParams) -> Vec<CodeActionOrCommand> {
    let uri = &params.text_document.uri;
    let Some(text) = documents.get(uri) else {
        return Vec::new();
    };

    let mapper = PositionMapper::new(text);
    let only = &params.context.only;
    let findings = analysis::lints(text);
    let mut actions = Vec::new();

    if kind_permitted(only, &CodeActionKind::QUICKFIX) {
        for finding in &findings {
            let Some(fix) = &finding.fix else {
                continue;
            };
            if !finding_requested(mapper.range(finding.span), params) {
                continue;
            }

            let edits = fix
                .edits
                .iter()
                .map(|edit| TextEdit {
                    range: mapper.range(edit.span),
                    new_text: edit.replacement.clone(),
                })
                .collect();

            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: format!("Fix: {}", finding.message),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![analysis::lint_diagnostic(&mapper, finding)]),
                edit: Some(workspace_edit(uri, edits)),
                ..Default::default()
            }));
        }
    }

    if kind_permitted(only, &CodeActionKind::SOURCE_FIX_ALL)
        && let Some(action) = fix_all_action(uri, text, &mapper, &findings)
    {
        actions.push(action);
    }

    actions
}

/// A `source.fixAll` action applying every fixable finding at once, or `None` when nothing is
/// fixable. Delegates to the linter's `apply_fixes` (which drops overlapping fixes so the result is
/// well-formed) and emits it as a single whole-document edit — nested fixes that were skipped
/// resolve on the next run, matching the CLI's `--fix`.
fn fix_all_action(
    uri: &Uri,
    text: &str,
    mapper: &PositionMapper,
    findings: &[monkey_c_linter::Diagnostic],
) -> Option<CodeActionOrCommand> {
    let fixes: Vec<_> = findings.iter().filter_map(|f| f.fix.clone()).collect();
    if fixes.is_empty() {
        return None;
    }

    let fixed = monkey_c_linter::apply_fixes(text, fixes);
    if fixed == text {
        return None;
    }

    let whole_document = TextEdit {
        range: Range {
            start: Position::new(0, 0),
            end: mapper.position(text.len()),
        },
        new_text: fixed,
    };

    Some(CodeActionOrCommand::CodeAction(CodeAction {
        title: "Fix all auto-fixable lint problems".to_string(),
        kind: Some(CodeActionKind::SOURCE_FIX_ALL),
        edit: Some(workspace_edit(uri, vec![whole_document])),
        ..Default::default()
    }))
}

fn workspace_edit(uri: &Uri, edits: Vec<TextEdit>) -> WorkspaceEdit {
    WorkspaceEdit {
        changes: Some(HashMap::from([(uri.clone(), edits)])),
        ..Default::default()
    }
}

/// Whether the client's `context.only` filter admits actions of `kind`. Absent `only` means "any
/// kind". A requested kind admits `kind` when it is equal to it or a parent of it (`source` admits
/// `source.fixAll`); the empty string is the LSP wildcard.
fn kind_permitted(only: &Option<Vec<CodeActionKind>>, kind: &CodeActionKind) -> bool {
    let Some(only) = only else {
        return true;
    };

    only.iter().any(|requested| {
        let requested = requested.as_str();
        requested.is_empty()
            || kind.as_str() == requested
            || kind.as_str().starts_with(&format!("{requested}."))
    })
}

/// Whether a finding at `finding` is in scope for the request. Editors send the cursor as a
/// zero-width range that only overlaps small findings when the caret sits exactly on them, so also
/// accept a finding that overlaps any diagnostic the client passed in `context.diagnostics` (which
/// nvim populates with the whole line's diagnostics).
fn finding_requested(finding: Range, params: &CodeActionParams) -> bool {
    ranges_overlap(finding, params.range)
        || params
            .context
            .diagnostics
            .iter()
            .any(|diagnostic| ranges_overlap(finding, diagnostic.range))
}

/// Whether two LSP ranges overlap or touch — a zero-width cursor range inside a diagnostic counts
/// as overlapping.
fn ranges_overlap(a: Range, b: Range) -> bool {
    !(before(a.end, b.start) || before(b.end, a.start))
}

fn before(a: Position, b: Position) -> bool {
    (a.line, a.character) < (b.line, b.character)
}

fn respond(
    connection: &Connection,
    id: RequestId,
    result: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let response = Response {
        id,
        result: Some(result),
        error: None,
    };
    connection.sender.send(Message::Response(response))?;

    Ok(())
}

fn cast_request<R>(request: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
{
    match request.extract::<R::Params>(R::METHOD) {
        Ok(value) => Ok(value),
        Err(ExtractError::MethodMismatch(request)) => Err(request),
        Err(ExtractError::JsonError { method, error }) => {
            panic!("malformed {method} request: {error}");
        }
    }
}

fn cast_notification<N>(notification: Notification) -> Result<N::Params, Notification>
where
    N: lsp_types::notification::Notification,
{
    match notification.extract::<N::Params>(N::METHOD) {
        Ok(params) => Ok(params),
        Err(ExtractError::MethodMismatch(notification)) => Err(notification),
        Err(ExtractError::JsonError { method, error }) => {
            panic!("malformed {method} notification: {error}");
        }
    }
}
