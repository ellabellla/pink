use std::convert::identity;
use crate::lexer::charstream::CharStream;
use crate::lexer::token_lookup::*;
use queues::*;

pub struct Tokenizer {
    stream: CharStream,
    history: Queue<usize>,
    line: usize,
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        self.history.add(self.stream.pos()).expect("history queue failed");


        self.parse_number().or(
            self.parse_reserved_word().or( 
                self.parse_identifier().or(
                    self.parse_string()
                )
            )
        )
    }
}

impl<'a> Tokenizer {
    pub fn new(input: &str) -> Tokenizer {
        Tokenizer{stream: CharStream::new(input), history: queue![], line: 0}
    }

    pub fn back(&mut self) {
        let pos = self.history.remove().unwrap_or_else(|_| 0);

        self.stream.seek(pos);
    }

    fn skip_whitespace(&mut self) {
        while let Some(next) = self.stream.peek() {
            if next.is_whitespace() {
                if next == '\n' {
                    self.line += 1;
                }
                self.stream.next();
            }
            else {
                break;
            }
        }
    }

    fn parse_reserved_word(&mut self) -> Option<Token> {
        let mut lookup_column: usize = 0;
        let mut lookup_index: usize = 0;
        let mut lookup_count: usize = token_lookup_table[0].column.len();

        let mut last_token = Token::UNDEFINED;
        let mut last_valid_pos = self.stream.pos();

        while let Some(next) = self.stream.next() {
            let mut found = false;
            for i in lookup_index..(lookup_index + lookup_count) {
                let (character, data) = token_lookup_table[lookup_column].column[i].clone();
                if character == next {
                    found = true;
                    match data {
                        Lookup_Data::INDEX(index, count) => {
                            lookup_index = index;
                            lookup_count = count;
                            last_token = Token::UNDEFINED;
                            break;
                        },
                        Lookup_Data::TOKEN(token) => {
                            return Some(token); 
                        },
                        Lookup_Data::INDEX_TOKEN(index, count, token) => {
                            lookup_index = index;
                            lookup_count = count;
                            last_token = token;
                            last_valid_pos = self.stream.pos();
                            break;
                        }
                    }
                }
            }
            if found && lookup_column + 1 < token_lookup_table.len() {
                lookup_column += 1;
            } else {
                self.stream.seek(last_valid_pos);
                if last_token == Token::UNDEFINED {
                    return None;
                } else {
                    return Some(last_token)
                }
            }
        }

        self.stream.seek(last_valid_pos);
        None
    }
    fn parse_number(&mut self) -> Option<Token>{
        let last_valid_pos = self.stream.pos();
        let mut data = Vec::new();

        if let Some(character) = self.stream.peek() {
            if character == '-' {
                data.push(character);
                self.stream.next();
            }
        }

        let mut found_dot  = false;

        while let Some(character) = self.stream.peek() {
            if !found_dot && character == '.' {
                found_dot = true;
                data.push(character)
            } else if !character.is_digit(10) {
                break;
            } else {
                data.push(character)
            }
            self.stream.next();
        }

        let string: String = data.iter().collect();
        match string.parse::<f64>() {
            Ok(number) => {
                return Some(Token::NUMBER(number));
            },
            Err(_) => {
                self.stream.seek(last_valid_pos);
                return None;
            }
        }
    }

    fn parse_identifier(&mut self) -> Option<Token> {
        let last_valid_pos = self.stream.pos();
        let mut data = Vec::new();

        if let Some(character) = self.stream.next() {
            if character.is_alphabetic() || character == '_' {
                data.push(character);
                while let Some(character) = self.stream.peek() {
                    if character.is_alphanumeric() || character == '_' {
                        data.push(character);
                        self.stream.next();
                    } else {
                        break;
                    }
                }

                if data.len() > 0 {
                    return Some(Token::IDENTIFIER(data.iter().collect()));
                }
            }
        } 

        self.stream.seek(last_valid_pos);
        None
    }

    fn parse_string(&mut self) -> Option<Token> {
        let last_valid_pos = self.stream.pos();
        let mut data = Vec::new();

        let mut found_start = true;
        for start_character in  token_string_start.chars() {
            if let Some(character) = self.stream.next() {
                if character != start_character {
                    found_start = false;
                    break;
                }
            } else {
                found_start = false;
                break;
            }
        }
        if !found_start {
            return None
        } 

        while let Some(character) = self.stream.peek() {

            if character == '\\' {
                self.stream.next();
                if let Some(character) = self.stream.peek() {
                    let mut found = true;
                    for (escape, replacement) in token_string_escape {
                        if escape == character {
                            found = true;
                            data.push(replacement); 
                            break;
                        }
                    }
                    if !found {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                let mut found_end = false;
                let start_pos = self.stream.pos();
                if token_string_end.starts_with(character) {
                    found_end = true;
                    for end_character in  token_string_end.chars() {
                        if let Some(character) = self.stream.next() {
                            if character != end_character {
                                found_end = false;
                                break;
                            }
                        } else {
                            found_end = false;
                            break;
                        }
                    }
                }

                if !found_end {
                    self.stream.seek(start_pos);
                    data.push(character);
                } else {
                    return Some(Token::STRING(data.iter().collect()));
                }
            } 

            self.stream.next();
        }

        self.stream.seek(last_valid_pos);
        None
    }
}


#[cfg(test)]
mod tokenizer_test {
    use super::{Tokenizer, Token};


    #[test]
    fn test_next() {
        let input = "+ - * / = += -= -> -*";
        let expected = [Token::ADD, Token::SUB, Token::MUL, Token::DIV, Token:: EQU, Token::ADD_EQU, Token::SUB_EQU, Token::EXEC, Token::REDUCE_EXEC];
        let mut tokenizer = Tokenizer::new(input).enumerate();


        while let Some((i, token)) = tokenizer.next() {
            assert_eq!(token, expected[i]);
        }
    }

    #[test]
    fn test_parse_number() {
        let input = "-0.1";
        let mut tokeniser = Tokenizer::new(input);
        assert_eq!(Some(Token::NUMBER(-0.1)), tokeniser.parse_number());


        let input = "-.1";
        let mut tokeniser = Tokenizer::new(input);
        assert_eq!(Some(Token::NUMBER(-0.1)), tokeniser.parse_number());


        let input = ".1";
        let mut tokeniser = Tokenizer::new(input);
        assert_eq!(Some(Token::NUMBER(0.1)), tokeniser.parse_number());

        let input = "123a";
        let mut tokeniser = Tokenizer::new(input);
        assert_eq!(Some(Token::NUMBER(123.0)), tokeniser.parse_number());

        let input = "a";
        let mut tokeniser = Tokenizer::new(input);
        assert_eq!(None, tokeniser.parse_number());
    }

    #[test]
    fn test_parse_indentifier() {
        let input = "ident";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::IDENTIFIER(String::from("ident"))), tokenizer.parse_identifier());

        let input = "_ident";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::IDENTIFIER(String::from("_ident"))), tokenizer.parse_identifier());

        let input = "ide2j89nt";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::IDENTIFIER(String::from("ide2j89nt"))), tokenizer.parse_identifier());

        let input = "ident_";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::IDENTIFIER(String::from("ident_"))), tokenizer.parse_identifier());

        let input = "1ide";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(None, tokenizer.parse_identifier());
    }

    #[test]
    fn test_parse_string() {
        let input = "\"[hello world]";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::STRING(String::from("hello world"))), tokenizer.parse_string());

        let input = "\"[hello world\\\\n\\n]";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::STRING(String::from("hello world\\n\n"))), tokenizer.parse_string());
    }
}