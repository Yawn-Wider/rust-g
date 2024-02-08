use std::{collections::HashMap, iter::Peekable};

use super::{lexer::Token, ParseError, SaveValue};
use crate::error::Result;

/*
The BYOND savefile string format follows more or less the following grammar:

savefile:   statement*
statement:  [assignment | object]
assignment: IDENTIFIER ASSIGN value
list:       LISTSTART list_items? LISTEND
list_items: [assignment | value] (LISTSEPARATOR [assignment | value])*
value:      [list | STRING | NUMBER | NULL]
object:     IDENTIFIER NEWLINE (INDENT statement NEWLINE)+
*/

pub fn parse(tokens: Vec<Token>) -> Result<SaveValue> {
    Parser::new(tokens.into_iter()).parse_object(-1)
}

/// This is a basic LR(1) recursive descent parser that
/// parses a tokenized version of a DM save file.
struct Parser<T>
where
    T: Iterator<Item = Token>,
{
    tokens: Peekable<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse_object(&mut self, parent_indent_level: isize) -> Result<SaveValue> {
        let mut obj = HashMap::<String, SaveValue>::new();

        while self.tokens.peek().is_some() {
            // First, calculate the indent level of the current line.
            let mut this_line_indent: isize = 0;
            'indentcounter: while let Some(t) = self.tokens.peek() {
                match t {
                    Token::Indent => {
                        self.tokens.next();
                        this_line_indent += 1;
                    }
                    Token::Newline => {
                        self.tokens.next();
                        this_line_indent = 0;
                    }
                    _ => break 'indentcounter,
                }
            }

            if self.tokens.peek().is_none() {
                // EOF also means return
                return Ok(SaveValue::Object(obj))
            }

            // An indent greater than the key that owns the current object
            // means we're still inside that key, so parse statements.
            if this_line_indent > parent_indent_level {
                let (key, value) = self.parse_statement()?;
                if let Some(old) = obj.insert(key, value) {
                    return Err(ParseError::DuplicateKey(format!("{:?}", old)).into());
                }
            }
            // An indent level that is the same or lower means that we have
            // exited the current owning key.
            else {
                return Ok(SaveValue::Object(obj));
            }
        }
        // EOF also means return
        Ok(SaveValue::Object(obj))
    }

    fn parse_statement(&mut self) -> Result<(String, SaveValue)> {
        match self.tokens.next() {
            Some(Token::Identifier(value)) => match self.tokens.peek() {
                Some(Token::Assign) => Ok((value, self.parse_assignment()?)),
                Some(Token::Newline) => Ok((value, self.parse_object(0)?)),
                Some(t) => Err(ParseError::UnexpectedToken(format!(
                    "Parsing identifier RHS: {:?}",
                    t
                ))
                .into()),
                None => Err(ParseError::UnexpectedEOF("Parsing statement RHS".to_owned()).into()),
            },
            Some(t) => {
                Err(ParseError::UnexpectedToken(format!("Parsing statement: {:?}", t)).into())
            }
            None => Err(ParseError::UnexpectedEOF("Parsing statement LHS".to_owned()).into()),
        }
    }

    fn parse_assignment(&mut self) -> Result<SaveValue> {
        match self.tokens.next() {
            Some(Token::Assign) => self.parse_value(),
            Some(t) => {
                Err(ParseError::UnexpectedToken(format!("Parsing assignment RHS: {:?}", t)).into())
            }
            None => Err(ParseError::UnexpectedEOF("Parsing list sequence".to_owned()).into()),
        }
    }

    fn parse_value(&mut self) -> Result<SaveValue> {
        match self.tokens.next() {
            Some(Token::ListStart) => self.parse_list(),
            Some(Token::String(s)) => Ok(SaveValue::String(s)),
            Some(Token::Identifier(v)) => match v.to_lowercase().as_str() {
                "null" => Ok(SaveValue::Null),
                v => match v.parse::<f32>() {
                    Ok(n) => Ok(SaveValue::Number(n)),
                    Err(_) => {
                        // Not a float, nor any of the others? Unexpected token.
                        Err(ParseError::UnexpectedToken(format!("Parsing value: {:?}", v)).into())
                    }
                },
            },
            Some(t) => Err(ParseError::UnexpectedToken(format!("{:?}", t)).into()),
            None => Err(ParseError::UnexpectedEOF("Parsing value".to_owned()).into()),
        }
    }

    fn parse_list(&mut self) -> Result<SaveValue> {
        let mut list = Vec::<(Option<String>, SaveValue)>::new();

        'mainloop: while let Some(t) = self.tokens.peek() {
            // Consume a list item
            match t {
                Token::ListEnd => {
                    self.tokens.next();
                    return Ok(SaveValue::List(list));
                }
                Token::Indent | Token::Newline => {
                    self.tokens.next();
                    continue 'mainloop;
                }
                _ => list.push(self.parse_list_item()?),
            }

            'innerloop: while let Some(t) = self.tokens.peek() {
                match t {
                    // If we see a list ender, then let the list be done
                    Token::ListEnd => {
                        self.tokens.next();
                        return Ok(SaveValue::List(list));
                    }
                    // If we see a separator, consume another item.
                    Token::ListSeparator => {
                        self.tokens.next();
                        continue 'mainloop;
                    }
                    // Consume whitespace until we find something else.
                    Token::Indent | Token::Newline => {
                        self.tokens.next();
                        continue 'innerloop;
                    }
                    t => {
                        return Err(ParseError::UnexpectedToken(format!(
                            "Parsing list separator: {:?}",
                            t
                        ))
                        .into())
                    }
                }
            }
            return Err(ParseError::UnexpectedEOF("Parsing list separator".to_owned()).into());
        }
        Err(ParseError::UnexpectedEOF("parsing list sequence".to_owned()).into())
    }

    fn parse_list_item(&mut self) -> Result<(Option<String>, SaveValue)> {
        match self.tokens.peek().cloned() {
            Some(Token::ListStart) => {
                self.tokens.next();
                Ok((None, self.parse_list()?))
            }
            Some(Token::String(s)) => {
                self.tokens.next();
                Ok((None, SaveValue::String(s.clone())))
            }
            Some(Token::Identifier(v)) => match v.to_lowercase().as_str() {
                "null" => {
                    self.tokens.next();
                    Ok((None, SaveValue::Null))
                }
                v => match v.parse::<f32>() {
                    Ok(n) => {
                        self.tokens.next();
                        Ok((None, SaveValue::Number(n)))
                    }
                    // Not a float, nor any of the others? This may be an assignment statement rather than a value literal
                    Err(_) => {
                        let lhs = v.to_owned();
                        self.tokens.next();
                        match self.tokens.peek() {
                            Some(Token::Assign) => Ok((Some(lhs), self.parse_assignment()?)),
                            Some(t) => Err(ParseError::UnexpectedToken(format!(
                                "Unknown list item form: {:?}",
                                t
                            ))
                            .into()),
                            None => Err(ParseError::UnexpectedEOF(
                                "Parsing list item assignment RHS".to_owned(),
                            )
                            .into()),
                        }
                    }
                },
            },
            Some(t) => {
                Err(ParseError::UnexpectedToken(format!("Parsing List Item: {:?}", t)).into())
            }
            None => Err(ParseError::UnexpectedEOF("List item".to_owned()).into()),
        }
    }
}

#[cfg(test)]
mod test {
    
}