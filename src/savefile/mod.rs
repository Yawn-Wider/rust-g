use crate::error::Result;
use std::{
    collections::HashMap,
    fmt::Display,
};

mod lexer;
mod parser;

#[derive(Clone, Debug, Serialize)]
pub enum SaveValue {
    Number(f32),
    String(String),
    List(Vec<(Option<String>, SaveValue)>),
    Object(HashMap<String, SaveValue>),
    Null,
}

impl SaveValue {
    fn from_string(raw: &str) -> Result<Self> {
        parser::parse(lexer::tokenize(raw)?)
    }
}

byond_fn! {
    savefile_to_json(save_string) {
        savefile_to_json_impl(save_string).ok()
    }
}

fn savefile_to_json_impl(save_string: &str) -> Result<String> {
    let s = SaveValue::from_string(save_string)?;
    serde_json::to_string_pretty(&s).map_err(|e| e.into())
}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnterminatedString(String),
    DuplicateKey(String),
    UnexpectedToken(String),
    UnexpectedEOF(String),
}

impl std::error::Error for ParseError {}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnterminatedString(detail) => {
                write!(f, "Unterminated String Value: {detail}")
            },
            ParseError::DuplicateKey(detail) => {
                write!(f, "Duplicate Key in Map: {detail}")
            },
            ParseError::UnexpectedToken(detail) => {
                write!(f, "Unexpected Token,: {detail}")
            }
            ParseError::UnexpectedEOF(detail) => {
                write!(f, "Unexpected EOF: {detail}")
            }
        }
    }
}