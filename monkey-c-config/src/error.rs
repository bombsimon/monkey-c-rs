use std::error;
use std::fmt;
use std::io;
use std::path::PathBuf;

/// A `rafiki.toml` that could not be read or understood. Errors name the
/// offending file, since the file is discovered rather than named by the user
/// and so its location is not otherwise obvious. A glob also names its setting,
/// since several settings take globs.
#[derive(Debug)]
pub enum Error {
    Read {
        path: PathBuf,
        source: io::Error,
    },
    Parse {
        path: PathBuf,
        source: Box<toml::de::Error>,
    },
    Glob {
        setting: &'static str,
        path: Option<PathBuf>,
        pattern: String,
        source: Box<ignore::Error>,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Read { path, source } => write!(f, "{}: {source}", path.display()),
            Self::Parse { path, source } => write!(f, "{}: {source}", path.display()),
            Self::Glob {
                setting,
                path,
                pattern,
                source,
            } => {
                if let Some(path) = path {
                    write!(f, "{}: ", path.display())?;
                }

                write!(f, "invalid glob `{pattern}` in `{setting}`: {source}")
            }
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::Read { source, .. } => Some(source),
            Self::Parse { source, .. } => Some(source),
            Self::Glob { source, .. } => Some(source),
        }
    }
}

impl From<Error> for io::Error {
    fn from(error: Error) -> Self {
        let message = error.to_string();

        match error {
            Error::Read { source, .. } => io::Error::new(source.kind(), message),
            Error::Parse { .. } => io::Error::new(io::ErrorKind::InvalidData, message),
            Error::Glob { .. } => io::Error::new(io::ErrorKind::InvalidInput, message),
        }
    }
}
