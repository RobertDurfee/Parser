use std::result;
use std::fmt;
use std::error;
use std::convert;

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ErrorKind {
    NotImplemented,
    UnexpectedEOF,
    UnexpectedToken,
    UndefinedNonterminal,
    UndefinedRootNonterminal,
    NoMatch,
    PartialMatch,
}

impl ErrorKind {
    pub fn as_str(&self) -> &'static str {
        match *self {
            ErrorKind::NotImplemented => "not implemented",
            ErrorKind::UnexpectedEOF => "unexpected end of file",
            ErrorKind::UnexpectedToken => "unexpected token",
            ErrorKind::UndefinedNonterminal => "undefined nonterminal",
            ErrorKind::UndefinedRootNonterminal => "undefined root nonterminal",
            ErrorKind::NoMatch => "no match",
            ErrorKind::PartialMatch => "partial match",
        }
    }
}

#[derive(Debug)]
struct Custom {
    kind: ErrorKind,
    error: Box<dyn error::Error>,
}

enum Representation {
    Simple(ErrorKind),
    Custom(Box<Custom>),
}

impl fmt::Debug for Representation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Representation::Simple(kind) => f.debug_tuple("Kind").field(&kind).finish(),
            Representation::Custom(ref custom) => fmt::Debug::fmt(&custom, f),
        }
    }
}

pub struct Error {
    representation: Representation,
}

impl Error {
    pub fn new<E>(kind: ErrorKind, error: E) -> Error
    where
        E: Into<Box<dyn error::Error>>,
    {
        Self::_new(kind, error.into())
    }

    fn _new(kind: ErrorKind, error: Box<dyn error::Error>) -> Error {
        Error { representation: Representation::Custom(Box::new(Custom { kind, error })) }
    }

    pub fn kind(&self) -> ErrorKind {
        match self.representation {
            Representation::Simple(kind) => kind,
            Representation::Custom(ref custom) => custom.kind,
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.representation, f)
    }
}

impl convert::From<ErrorKind> for Error {
    #[inline]
    fn from(kind: ErrorKind) -> Error {
        Error { representation: Representation::Simple(kind) }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.representation {
            Representation::Simple(kind) => write!(f, "{}", kind.as_str()),
            Representation::Custom(ref custom) => custom.error.fmt(f),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self.representation {
            Representation::Simple(..) => None,
            Representation::Custom(ref custom) => custom.error.source(),
        }
    }
}
