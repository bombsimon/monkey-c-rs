//! Converting the `file:` URIs an LSP client sends into filesystem paths.

use percent_encoding::percent_decode_str;

use std::path::PathBuf;

/// The filesystem path a `file:` URI refers to, or `None` for any other scheme
/// (a client may report an untitled buffer or a virtual document, which has no
/// path to walk up from).
pub fn to_path(uri: &str) -> Option<PathBuf> {
    let rest = uri.strip_prefix("file://")?;

    // `file://host/path` addresses another machine; only the empty (local)
    // authority is meaningful here.
    let path = match rest.find('/') {
        Some(0) => rest,
        _ => return None,
    };

    let decoded = percent_decode_str(path).decode_utf8().ok()?;

    // A Windows URI is `file:///C:/dir`, whose path component carries a leading
    // slash that is not part of the path.
    let decoded = match decoded.strip_prefix('/') {
        Some(without_slash) if is_windows_drive(without_slash) => without_slash,
        _ => &decoded,
    };

    Some(PathBuf::from(decoded))
}

/// Whether `path` starts with a `C:`-style drive letter.
fn is_windows_drive(path: &str) -> bool {
    let mut chars = path.chars();

    matches!(
        (chars.next(), chars.next(), chars.next()),
        (Some(letter), Some(':'), Some('/') | None) if letter.is_ascii_alphabetic()
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_unix_path_round_trips() {
        assert_eq!(
            to_path("file:///home/someone/project"),
            Some(PathBuf::from("/home/someone/project"))
        );
    }

    #[test]
    fn percent_escapes_are_decoded() {
        assert_eq!(
            to_path("file:///Users/someone/My%20Project"),
            Some(PathBuf::from("/Users/someone/My Project"))
        );
    }

    #[test]
    fn non_ascii_escapes_are_decoded() {
        assert_eq!(
            to_path("file:///tmp/g%C3%A5rd"),
            Some(PathBuf::from("/tmp/gård"))
        );
    }

    #[test]
    fn a_windows_drive_loses_its_leading_slash() {
        assert_eq!(
            to_path("file:///C:/Users/someone"),
            Some(PathBuf::from("C:/Users/someone"))
        );
    }

    #[test]
    fn other_schemes_have_no_path() {
        assert_eq!(to_path("untitled:Untitled-1"), None);
        assert_eq!(to_path("https://example.com/x"), None);
    }

    #[test]
    fn a_remote_authority_is_rejected() {
        assert_eq!(to_path("file://otherhost/share/project"), None);
    }
}
