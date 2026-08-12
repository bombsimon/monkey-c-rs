use std::error;
use std::fmt;
use std::io;
use std::path::PathBuf;

/// A `rafiki.toml` that could not be read or understood. Both variants name the
/// offending file, since the file is discovered rather than named by the user
/// and so its location is not otherwise obvious.
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
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Read { path, source } => write!(f, "{}: {source}", path.display()),
            Self::Parse { path, source } => write!(f, "{}: {source}", path.display()),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::Read { source, .. } => Some(source),
            Self::Parse { source, .. } => Some(source),
        }
    }
}

impl From<Error> for io::Error {
    fn from(error: Error) -> Self {
        let message = error.to_string();

        match error {
            Error::Read { source, .. } => io::Error::new(source.kind(), message),
            Error::Parse { .. } => io::Error::new(io::ErrorKind::InvalidData, message),
        }
    }
}
