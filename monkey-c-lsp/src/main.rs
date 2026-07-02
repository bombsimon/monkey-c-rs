//! A Language Server for Monkey C, exposing the workspace's parser, linter, and
//! formatter over LSP.
//!
//! The server keeps the full text of every open document in memory (full-sync)
//! and re-analyses on open/change, publishing diagnostics. It answers
//! `textDocument/formatting` by re-rendering through the formatter and
//! `textDocument/codeAction` with the linter's fixes.

// `gen_lsp_types::Uri` carries an internal lazy-parse cache (interior
// mutability) that doesn't affect its `Hash`/`Eq`, so it is a safe `HashMap`
// key despite `clippy::mutable_key_type` flagging every map that uses it.
#![allow(clippy::mutable_key_type)]

mod analysis;
mod position;

use std::collections::HashMap;
use std::error::Error;

use gen_lsp_types::{
    CodeAction, CodeActionKind, CodeActionOptions, CodeActionParams, CodeActionProvider,
    CodeActionRequest, CodeActionResponse, Diagnostic, DidChangeTextDocumentNotification,
    DidCloseTextDocumentNotification, DidCloseTextDocumentParams, DidOpenTextDocumentNotification,
    DocumentFormattingParams, DocumentFormattingProvider, DocumentFormattingRequest,
    LspNotificationMethod, LspRequestMethod, Notification as _, Position,
    PublishDiagnosticsNotification, PublishDiagnosticsParams, Range, Request as _,
    ServerCapabilities, TextDocumentContentChangeEvent, TextDocumentSync, TextDocumentSyncKind,
    TextEdit, Uri, WorkspaceEdit,
};
use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};

use crate::position::PositionMapper;

/// Open documents keyed by URI, holding their current full text.
type Documents = HashMap<Uri, String>;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let (connection, io_threads) = Connection::stdio();

    let capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSync::Kind(TextDocumentSyncKind::Full)),
        document_formatting_provider: Some(DocumentFormattingProvider::Bool(true)),
        code_action_provider: Some(CodeActionProvider::CodeActionOptions(CodeActionOptions {
            code_action_kinds: Some(vec![CodeActionKind::QuickFix, CodeActionKind::SourceFixAll]),
            ..Default::default()
        })),
        ..Default::default()
    })?;

    let _init_params = connection.initialize(capabilities)?;
    run(&connection)?;

    // `connection` must be dropped before joining: its `sender` keeps the writer
    // thread's channel open, so `join` would otherwise block forever.
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
    let id = request.id.clone();
    let method = LspRequestMethod::from(request.method.as_str());

    if method == DocumentFormattingRequest::METHOD {
        let params: DocumentFormattingParams = serde_json::from_value(request.params)?;
        let edits = format(documents, &params);
        respond(connection, id, serde_json::to_value(edits)?)?;
    } else if method == CodeActionRequest::METHOD {
        let params: CodeActionParams = serde_json::from_value(request.params)?;
        let actions = code_actions(documents, &params);
        respond(connection, id, serde_json::to_value(actions)?)?;
    } else {
        // Unknown request: reply with an empty success so the client isn't left
        // waiting. Specific method handling is added above.
        respond(connection, id, serde_json::Value::Null)?;
    }

    Ok(())
}

fn handle_notification(
    connection: &Connection,
    documents: &mut Documents,
    notification: Notification,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let method = LspNotificationMethod::from(notification.method.as_str());

    if method == DidOpenTextDocumentNotification::METHOD {
        let params: gen_lsp_types::DidOpenTextDocumentParams =
            serde_json::from_value(notification.params)?;
        let uri = params.text_document.uri;
        documents.insert(uri.clone(), params.text_document.text);
        publish(connection, documents, &uri)?;
    } else if method == DidChangeTextDocumentNotification::METHOD {
        let params: gen_lsp_types::DidChangeTextDocumentParams =
            serde_json::from_value(notification.params)?;
        // Full sync: the last change carries the whole new text.
        if let Some(change) = params.content_changes.into_iter().next_back() {
            let uri = params.text_document.text_document_identifier.uri;
            documents.insert(uri.clone(), content_change_text(change));
            publish(connection, documents, &uri)?;
        }
    } else if method == DidCloseTextDocumentNotification::METHOD {
        let params: DidCloseTextDocumentParams = serde_json::from_value(notification.params)?;
        documents.remove(&params.text_document.uri);
        // Clear diagnostics for the now-closed document.
        send_diagnostics(connection, params.text_document.uri, Vec::new())?;
    }

    Ok(())
}

/// The new full text carried by a content change. With full sync the client
/// always sends the whole document, but both shapes are handled.
fn content_change_text(change: TextDocumentContentChangeEvent) -> String {
    match change {
        TextDocumentContentChangeEvent::TextDocumentContentChangeWholeDocument(whole) => whole.text,
        TextDocumentContentChangeEvent::TextDocumentContentChangePartial(partial) => partial.text,
    }
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
    diagnostics: Vec<Diagnostic>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };
    let notification = Notification::new(PublishDiagnosticsNotification::METHOD.into(), params);
    connection
        .sender
        .send(Message::Notification(notification))?;

    Ok(())
}

/// The formatting edits for a document: a single edit replacing the whole
/// document with its formatted text. Returns no edits when the document is
/// unknown, doesn't parse, or is already formatted.
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

/// Code actions for a document. Offers, subject to the client's `context.only`
/// filter:
///
/// - one `quickfix` per fixable lint finding overlapping the requested range,
///   each carrying just that finding's fix and the diagnostic it resolves; and
/// - a single `source.fixAll` action bundling every fixable finding, which an
///   editor can run on save to apply all lint fixes at once.
fn code_actions(documents: &Documents, params: &CodeActionParams) -> Vec<CodeActionResponse> {
    let uri = &params.text_document.uri;
    let Some(text) = documents.get(uri) else {
        return Vec::new();
    };

    let mapper = PositionMapper::new(text);
    let only = &params.context.only;
    let findings = analysis::lints(text);
    let mut actions = Vec::new();

    if kind_permitted(only, &CodeActionKind::QuickFix) {
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

            actions.push(CodeActionResponse::CodeAction(CodeAction {
                title: format!("Fix: {}", finding.message),
                kind: Some(CodeActionKind::QuickFix),
                diagnostics: Some(vec![analysis::lint_diagnostic(&mapper, finding)]),
                edit: Some(workspace_edit(uri, edits)),
                ..Default::default()
            }));
        }
    }

    if kind_permitted(only, &CodeActionKind::SourceFixAll)
        && let Some(action) = fix_all_action(uri, text, &mapper, &findings)
    {
        actions.push(action);
    }

    actions
}

/// A `source.fixAll` action applying every fixable finding at once, or `None`
/// when nothing is fixable. Delegates to the linter's `apply_fixes` (which
/// drops overlapping fixes so the result is well-formed) and emits it as a
/// single whole-document edit — nested fixes that were skipped resolve on the
/// next run, matching the CLI's `--fix`.
fn fix_all_action(
    uri: &Uri,
    text: &str,
    mapper: &PositionMapper,
    findings: &[monkey_c_linter::Diagnostic],
) -> Option<CodeActionResponse> {
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

    Some(CodeActionResponse::CodeAction(CodeAction {
        title: "Fix all auto-fixable lint problems".to_string(),
        kind: Some(CodeActionKind::SourceFixAll),
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

/// Whether a finding at `finding` is in scope for the request. Editors send the
/// cursor as a zero-width range that only overlaps small findings when the
/// caret sits exactly on them, so also accept a finding that overlaps any
/// diagnostic the client passed in `context.diagnostics` (which nvim populates
/// with the whole line's diagnostics).
fn finding_requested(finding: Range, params: &CodeActionParams) -> bool {
    ranges_overlap(finding, params.range)
        || params
            .context
            .diagnostics
            .iter()
            .any(|diagnostic| ranges_overlap(finding, diagnostic.range))
}

/// Whether the client's `context.only` filter admits actions of `kind`. Absent
/// `only` means "any kind". A requested kind admits `kind` when it equals it or
/// is a parent of it (`source` admits `source.fixAll`); the empty kind is the
/// LSP wildcard. Comparison is on the wire strings so sub-kinds work without
/// enumerating the enum.
fn kind_permitted(only: &Option<Vec<CodeActionKind>>, kind: &CodeActionKind) -> bool {
    let Some(only) = only else {
        return true;
    };

    let kind = kind_wire(kind);
    only.iter().any(|requested| {
        let requested = kind_wire(requested);
        requested.is_empty() || kind == requested || kind.starts_with(&format!("{requested}."))
    })
}

/// The on-the-wire string for a code action kind (e.g. `source.fixAll`).
fn kind_wire(kind: &CodeActionKind) -> String {
    serde_json::to_value(kind)
        .ok()
        .and_then(|value| value.as_str().map(str::to_owned))
        .unwrap_or_default()
}

/// Whether two LSP ranges overlap or touch — a zero-width cursor range inside a
/// diagnostic counts as overlapping.
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
