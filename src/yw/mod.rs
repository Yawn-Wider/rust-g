
#[cfg(feature = "savefile")]
pub mod savefile;

#[derive(Fail, Debug)]
pub enum YWError {
    #[cfg(feature="savefile")]
    #[fail(display = "{}", _0)]
    SaveParseError(#[cause] savefile::ParseError),
}

#[cfg(feature = "savefile")]
impl From<savefile::ParseError> for YWError {
    fn from(error: savefile::ParseError) -> YWError {
        YWError::SaveParseError(error)
    }
}

#[cfg(feature = "savefile")]
impl From<savefile::ParseError> for crate::error::Error {
    fn from(error: savefile::ParseError) -> crate::error::Error {
        YWError::SaveParseError(error).into()
    }
}