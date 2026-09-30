//! Column alignment of trailing comments, applied to the rendered text as a final pass.

use crate::doc::display_width;

/// Column-align trailing `//` and `/*` comments across consecutive lines.
pub(crate) fn align_trailing_comments(text: &str) -> String {
    let lines: Vec<&str> = text.split('\n').collect();

    // Track block-comment spans so interior lines are not misidentified as
    // having trailing `//` comments (e.g. `https://` in a doc-comment URL).
    let mut in_block = false;
    let analyzed: Vec<Option<(usize, usize, usize)>> = lines
        .iter()
        .map(|l| {
            if in_block {
                if l.contains("*/") {
                    in_block = false;
                }
                return None;
            }
            let result = analyze_trailing(l);
            // Case 1: trailing `/* ... */` opener on a code line (analyze_trailing
            // returned Some with a `/*` comment that doesn't close on this line).
            if let Some((_, _, cs)) = result {
                let slice = &l.as_bytes()[cs..];
                if slice.len() >= 2 && slice[1] == b'*' && !l[cs..].contains("*/") {
                    in_block = true;
                    return None;
                }
            }

            // Case 2: `/*` occupies the whole line with no code before it.
            // analyze_trailing returns None in this case (code_end <= indent),
            // so we detect it by inspecting the trimmed line directly.  This
            // is the common `/** doc comment */` pattern.
            let trimmed = l.trim_start();
            if trimmed.starts_with("/*") && !trimmed.contains("*/") {
                in_block = true;
            }

            result
        })
        .collect();

    let mut out: Vec<String> = lines.iter().map(|l| (*l).to_string()).collect();

    let mut i = 0;
    while i < analyzed.len() {
        let Some((indent, _, _)) = analyzed[i] else {
            i += 1;
            continue;
        };

        let mut j = i;
        while j < analyzed.len()
            && matches!(analyzed[j], Some((ind, _, _)) if ind == indent
                || (ind > indent && is_binary_chain_continuation(lines[j])))
        {
            j += 1;
        }

        if j - i >= 2 {
            let max_code = (i..j)
                .filter_map(|k| {
                    analyzed[k].map(|(_, code_end, _)| display_width(&lines[k][..code_end]))
                })
                .max()
                .unwrap_or(0);

            for k in i..j {
                let Some((_, code_end, comment_start)) = analyzed[k] else {
                    continue;
                };
                let line = lines[k];
                let code = &line[..code_end];
                let comment = &line[comment_start..];
                let pad = max_code - display_width(code);
                out[k] = format!("{code}{} {comment}", " ".repeat(pad));
            }
        }

        i = j;
    }

    out.join("\n")
}

fn is_binary_chain_continuation(line: &str) -> bool {
    const OPS: &[&str] = &[
        "==",
        "!=",
        "<=",
        ">=",
        "<<",
        ">>",
        "&&",
        "||",
        "and",
        "or",
        "instanceof",
        "has",
        "+",
        "-",
        "*",
        "/",
        "%",
        "<",
        ">",
        "&",
        "|",
        "^",
    ];

    let trimmed = line.trim_start();
    OPS.iter()
        .any(|op| trimmed.starts_with(op) && trimmed[op.len()..].starts_with(' '))
}

fn analyze_trailing(line: &str) -> Option<(usize, usize, usize)> {
    let bytes = line.as_bytes();
    let indent = bytes.iter().take_while(|b| **b == b' ').count();

    let mut i = indent;
    let mut in_string = false;
    let mut in_char = false;
    let mut comment_start: Option<usize> = None;

    while i < bytes.len() {
        let c = bytes[i];
        if in_string {
            if c == b'\\' && i + 1 < bytes.len() {
                i += 2;
                continue;
            }

            if c == b'"' {
                in_string = false;
            }
        } else if in_char {
            if c == b'\\' && i + 1 < bytes.len() {
                i += 2;
                continue;
            }

            if c == b'\'' {
                in_char = false;
            }
        } else if c == b'"' {
            in_string = true;
        } else if c == b'\'' {
            in_char = true;
        } else if c == b'/' && i + 1 < bytes.len() && matches!(bytes[i + 1], b'/' | b'*') {
            // A block comment with code after it on the same line is inline, not trailing.
            if bytes[i + 1] == b'*'
                && let Some(close) = line[i + 2..].find("*/")
            {
                let after = i + 2 + close + 2;
                if !line[after..].trim().is_empty() {
                    i = after;
                    continue;
                }
            }

            comment_start = Some(i);
            break;
        }

        i += 1;
    }

    let comment_start = comment_start?;

    let code_end = line[..comment_start].trim_end().len();
    if code_end <= indent {
        return None;
    }

    Some((indent, code_end, comment_start))
}
