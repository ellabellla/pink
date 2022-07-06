use crate::lexer::charstream::CharStream;
use crate::lexer::token_lookup::*;
use queues::*;

enum Whitespace {
    Newline,
    Blank,
}

pub struct Tokenizer {
    stream: CharStream,
    history: Queue<usize>,
    line: usize,
    prev_whitespace: Whitespace,
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        
        if self.stream.peek() == None {
            return Some(Token::EOF);
        }

        self.history.add(self.stream.pos()).expect("history queue failed");

        self.parse_number().or_else(||
            self.parse_reserved_word().or_else(||
                self.parse_identifier().or_else(||
                    self.parse_string()
                )
            )
        )
    }
}

#[allow(dead_code)]
impl<'a> Tokenizer {
    pub fn new(input: &str) -> Tokenizer {
        Tokenizer{stream: CharStream::new(input), history: queue![], line: 0, prev_whitespace: Whitespace::Blank}
    }

    pub fn back(&mut self) {
        let pos = self.history.remove().unwrap_or_else(|_| 0);

        self.stream.seek(pos);
    }

    pub fn pos(&self) -> usize {
        self.stream.pos()
    }

    pub fn seek(&mut self, index:usize) {
        self.stream.seek(index);
    }

    pub fn was_newline_before(&self) -> bool {
        matches!(self.prev_whitespace, Whitespace::Newline)
    }

    pub fn newline_next(&mut self) -> bool {
        let start_pos = self.stream.pos();
        let mut found = false;
        while let Some(next) = self.stream.peek() {
            if next.is_whitespace() {
                if next == '\n' {
                    found = true;
                    break;
                }
                self.stream.next();
            }
            else {
                break;
            }
        }
        self.stream.seek(start_pos);
        found
    
    }

    fn skip_whitespace(&mut self) {
        self.prev_whitespace = Whitespace::Blank;
        while let Some(next) = self.stream.peek() {
            if next.is_whitespace() {
                if next == '\n' {
                    self.line += 1;
                    self.prev_whitespace = Whitespace::Newline;
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
        let mut lookup_count: usize = TOKEN_LOOKUP_TABLE[0].column.len();

        let mut last_token = Token::Undefined;
        let mut last_valid_pos = self.stream.pos();

        while let Some(next) = self.stream.next() {
            let mut found = false;
            for i in lookup_index..(lookup_index + lookup_count) {
                let (character, data) = TOKEN_LOOKUP_TABLE[lookup_column].column[i].clone();
                if character == next {
                    found = true;
                    match data {
                        Lookup_Data::INDEX(index, count) => {
                            lookup_index = index;
                            lookup_count = count;
                            last_token = Token::Undefined;
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
            if found && lookup_column + 1 < TOKEN_LOOKUP_TABLE.len() {
                lookup_column += 1;
            } else {
                self.stream.seek(last_valid_pos);
                if last_token == Token::Undefined {
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
                return Some(Token::Number(number));
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
                    return Some(Token::Identifier(data.iter().collect()));
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
        for start_character in  TOKEN_STRING_START.chars() {
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
            self.stream.seek(last_valid_pos);
            return None
        } 

        while let Some(character) = self.stream.peek() {
            if !character.is_ascii() {
                return None
            }
            
            if character == '\\' {
                self.stream.next();
                if let Some(character) = self.stream.peek() {
                    let mut found = true;
                    for (escape, replacement) in TOKEN_STRING_ESCAPE {
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
                if TOKEN_STRING_END.starts_with(character) {
                    found_end = true;
                    for end_character in  TOKEN_STRING_END.chars() {
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
                    return Some(Token::ListString(data.iter().collect()));
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
        let input = "+ - * / = +: -: -> *>";
        let expected = [Token::Add, Token::Subtract, Token::Multiply, Token::Divide, Token:: Equals, Token::AddAndSet, Token::SubtractAndSet, Token::Exec, Token::Reduce];
        let mut tokenizer = Tokenizer::new(input).enumerate();


        while let Some((i, token)) = tokenizer.next() {
            assert_eq!(token, expected[i]);
        }
    }
    
    #[test]
    fn test_next_nospace() {
        let input = "variable:10,";
        let expected = [Token::Identifier("variable".to_string()), Token::Set, Token::Number(10.0), Token::SeparateAndPush];
        let mut tokenizer = Tokenizer::new(input).enumerate();


        while let Some((i, token)) = tokenizer.next() {
            assert_eq!(token, expected[i]);
        }
    }

    #[test]
    fn test_parse_number() {
        let input = "-0.1";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::Number(-0.1)), tokenizer.parse_number());


        let input = "-.1";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::Number(-0.1)), tokenizer.parse_number());


        let input = ".1";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::Number(0.1)), tokenizer.parse_number());

        let input = "123a";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::Number(123.0)), tokenizer.parse_number());

        let input = "a";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(None, tokenizer.parse_number());
    }

    #[test]
    fn test_parse_indentifier() {
        let input = "ident";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::Identifier(String::from("ident"))), tokenizer.parse_identifier());

        let input = "_ident";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::Identifier(String::from("_ident"))), tokenizer.parse_identifier());

        let input = "ide2j89nt";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::Identifier(String::from("ide2j89nt"))), tokenizer.parse_identifier());

        let input = "ident_";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::Identifier(String::from("ident_"))), tokenizer.parse_identifier());

        let input = "1ide";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(None, tokenizer.parse_identifier());
    }

    #[test]
    fn test_parse_string() {
        let input = "\"[hello world]";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::ListString(String::from("hello world"))), tokenizer.parse_string());

        let input = "\"[hello world\\\\n\\n]";
        let mut tokenizer = Tokenizer::new(input);
        assert_eq!(Some(Token::ListString(String::from("hello world\\n\n"))), tokenizer.parse_string());
    }
}