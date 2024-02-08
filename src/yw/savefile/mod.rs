use serde::{ser::{SerializeMap, SerializeSeq}, Serializer};

use crate::error::Result;
use std::{
    collections::HashMap,
    fmt::Display,
};

mod lexer;
mod parser;

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
pub enum SaveValue {
    Number(f32),
    String(String),
    List(Vec<SaveListItem>),
    Object(HashMap<String, SaveValue>),
    Datum(String),
    Null,
}

impl SaveValue {
    fn from_string(raw: &str) -> Result<Self> {
        parser::parse(lexer::tokenize(raw)?)
    }
}

impl serde::Serialize for SaveValue {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> where S: Serializer {
        match self {
            SaveValue::Number(n) => serializer.serialize_f32(*n),
            SaveValue::String(s) => serializer.serialize_str(s),
            SaveValue::List(l) => {
                let mut seq = serializer.serialize_seq(Some(l.len()))?;
                for v in l {
                    seq.serialize_element(v)?;
                }
                seq.end()
            },
            SaveValue::Object(o) => {
                let mut map = serializer.serialize_map(Some(o.len()))?;
                for (k, v) in o {
                    map.serialize_entry(k, v)?;
                }
                map.end()
            },
            SaveValue::Datum(s) => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("datum", s)?;
                map.end()
            }
            SaveValue::Null => serializer.serialize_none(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum SaveListItem {
    Named(String, SaveValue),
    Unnamed(SaveValue),
}

impl serde::Serialize for SaveListItem {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> where S: Serializer {
        match self {
            SaveListItem::Named(k, v) => {
                let mut obj = serializer.serialize_map(Some(1))?;
                obj.serialize_entry(k, v)?;
                obj.end()
            },
            SaveListItem::Unnamed(v) => v.serialize(serializer),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnterminatedString(String),
    DuplicateKey(String),
    UnexpectedToken(String),
    UnexpectedEOF(String),
    Context(String, Box<ParseError>),
}

impl ParseError {
    fn with_context(self, detail: impl AsRef<str>) -> Self {
        ParseError::Context(detail.as_ref().to_owned(), Box::new(self))
    }
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
                write!(f, "Unexpected Token: {detail}")
            }
            ParseError::UnexpectedEOF(detail) => {
                write!(f, "Unexpected EOF: {detail}")
            }
            ParseError::Context(detail, cause) => {
                write!(f, "{detail}\n{cause}")
            }
        }
    }
}

