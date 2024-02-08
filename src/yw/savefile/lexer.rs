use crate::error::Result;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    String(String),
    Assign,
    ListStart,
    ListSeparator,
    ListEnd,
    Indent,
    Newline,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Mode {
    Normal,
    String,
}

pub fn tokenize(raw: &str) -> Result<Vec<Token>> {
    let mut tokens = Vec::<Token>::new();
    let mut buf = String::new();
    let mut mode = Mode::Normal;

    let mut stream = raw.chars().peekable();

    #[allow(clippy::while_let_on_iterator)]
    while let Some(c) = stream.next() {
        match mode {
            Mode::Normal => {
                // First check for transition characters and clear the buffer
                // if they are found.
                match c {
                    '(' => {
                        if !buf.is_empty() {
                            if buf.to_lowercase() != "list" {
                                tokens.push(Token::Identifier(buf.clone()));
                            }
                            buf.clear();
                        }
                    }
                    '=' | '\t' | '\n' | '\r' | '"' | ',' | ')' => {
                        if !buf.is_empty() {
                            tokens.push(Token::Identifier(buf.clone()));
                            buf.clear();
                        }
                    }
                    c if c.is_whitespace() => {
                        if !buf.is_empty() {
                            tokens.push(Token::Identifier(buf.clone()));
                            buf.clear();
                        }
                    }
                    _ => {}
                }

                // Now actually analyze on that character and push tokens to the list.
                match c {
                    '=' => tokens.push(Token::Assign),
                    '\t' => tokens.push(Token::Indent),
                    '\n' | '\r' => {
                        // Deduplicate newlines
                        if tokens.last() != Some(&Token::Newline) {
                            tokens.push(Token::Newline)
                        }
                    }
                    '"' => mode = Mode::String,
                    ',' => tokens.push(Token::ListSeparator),
                    '(' => tokens.push(Token::ListStart),
                    ')' => tokens.push(Token::ListEnd),
                    c if c != '\t' && c.is_whitespace() => {} // ignore whitespace outside of transition state
                    c => buf.push(c),
                }
            }
            Mode::String => match c {
                // Unescaped quote means we
                '"' => {
                    tokens.push(Token::String(buf.clone()));
                    buf.clear();
                    mode = Mode::Normal;
                }
                '\\' => {
                    if let Some(next_char) = stream.next() {
                        buf.push(next_char);
                    } else {
                        return Err(super::ParseError::UnterminatedString(
                            "Unexpected escaped EOF".to_owned(),
                        )
                        .into());
                    }
                }
                c => buf.push(c),
            },
        }
    }

    if !buf.is_empty() && mode == Mode::String {
        return Err(super::ParseError::UnterminatedString("Unexpected EOF".to_owned()).into());
    }

    if !buf.is_empty() {
        tokens.push(Token::Identifier(buf));
    }

    Ok(tokens)
}
