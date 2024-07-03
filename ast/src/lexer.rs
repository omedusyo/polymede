use crate::token;
use crate::token::{Token, SeparatorSymbol};

#[derive(Debug, Clone, Copy)]
pub struct Position { pub column: usize, pub line : usize }

#[derive(Debug, Clone)]
pub struct LocatedToken {
    pub token: Token,
    pub position: Position,
}

impl LocatedToken {
    pub fn new(token: Token, position: Position) -> Self {
        Self { token, position }
    }
}

#[derive(Debug)]
pub enum Request {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Keyword(token::Keyword),
    TypeDeclarationKeyword,
    Identifier,
    Separator(SeparatorSymbol),
    BindingSeparator,
    Nat32,
    End,
}


#[derive(Debug)]
// TODO: Introduce Committed vs Recoverable Error
pub enum Error {
    UnexpectedEnd,
    Expected { requested: Request, found: String },
    ExpectedTypeDeclarationKeyword,
    Nat32LiteralTooBig,
}

#[derive(Debug)]
pub struct State<'a> {
    tokens: &'a str,
    position: Position
}

impl <'state> State<'state> {
    // 'a lives atleast as long as 'state ('a contains 'state)
    pub fn new<'a: 'state>(str: &'a str) -> Self {
        Self {
            tokens: str,
            position: Position { column: 1, line: 1 },
        }
    }

    #[inline]
    pub fn position(&self) -> Position {
        self.position
    }

    #[inline]
    pub fn tokens(&self) -> &str {
        &self.tokens
    }

    pub fn clone(&self) -> Self {
        Self {
            tokens: self.tokens,
            position: self.position,
        }
    }

    pub fn new_line(&mut self) {
        self.position.column = 1;
        self.position.line += 1;
    }

    pub fn move_column_by(&mut self, n: usize) {
        self.position.column += n;
    }

    pub fn consume_by(&mut self, n: usize) {
        self.tokens = &self.tokens[n..]
    }

    pub fn consume_whitespace(&mut self) {
        for c in self.tokens.chars() {
            if c == '\n' {
                self.new_line();
                self.consume_by(1);
            } else if c == ' ' || c == '\t' {
                self.move_column_by(1);
                self.consume_by(1);
            } else {
                return
            }
        }
    }

    // Note that this returns the position BEFORE the advancement.
    pub fn advance(&mut self) -> Position {
        let previous_position = self.position;
        self.move_column_by(1);
        self.consume_by(1);
        previous_position
    }

    pub fn read_char_or_fail_when_end(&self) -> Result<char, Error> {
        match self.tokens.chars().next() {
            Some(c) => Ok(c),
            None => Err(Error::UnexpectedEnd) 
        }
    }

    pub fn match_string(&mut self, str: &str, request: Request, result_token: Token) -> Result<LocatedToken, Error> {
        let mut chars_for_error: Vec<char> = vec![];
        let start_position = self.position();
        for c0 in str.chars() {
            let c = self.read_char_or_fail_when_end()?;
            chars_for_error.push(c);
            if c != c0 {
                return Err(Error::Expected { requested: request, found: chars_for_error.into_iter().collect() })
            }
            self.advance();
        }
        Ok(LocatedToken::new(result_token, start_position))
    }

    fn match_keyword(&mut self, request: Request, keyword: token::Keyword) -> Result<LocatedToken, Error> {
        self.match_string(keyword.string(), request, Token::Keyword(keyword))
    }

    pub fn request(&mut self, request: Request) -> Result<LocatedToken, Error> {
        self.consume_whitespace();
        match request {
            Request::OpenParen => {
                self.match_string("(", request, Token::OpenParen)
            },
            Request::CloseParen => {
                self.match_string(")", request, Token::CloseParen)
            },
            Request::OpenCurly => {
                self.match_string("{", request, Token::OpenCurly)
            },
            Request::CloseCurly => {
                self.match_string("}", request, Token::CloseCurly)
            },
            Request::Keyword(keyword) => {
                self.match_keyword(request, keyword)
            },
            Request::TypeDeclarationKeyword => {
                let c = self.read_char_or_fail_when_end()?;
                if c == 'e' {
                    self.match_keyword(request, token::Keyword::Enum)
                } else if c == 'i' {
                    self.match_keyword(request, token::Keyword::Ind)
                } else {
                    Err(Error::ExpectedTypeDeclarationKeyword)
                }
            },
            Request::Identifier => {
                let mut chars: Vec<char> = vec![];
                let identifier_start_position = {
                    let c = self.read_char_or_fail_when_end()?;
                    if c.is_alphabetic() || c == '_' {
                        chars.push(c);
                        self.advance()
                    } else {
                        return Err(Error::Expected { requested: request, found: c.to_string() })
                    }
                };

                while let Ok(c) = self.read_char_or_fail_when_end() {
                    if c.is_alphanumeric() || c == '_' {
                        chars.push(c);
                        self.advance();
                    } else {
                        let s: String = chars.into_iter().collect();
                        return Ok(LocatedToken::new(Token::Identifier(s), identifier_start_position))
                    }
                }
                let s: String = chars.into_iter().collect();
                return Ok(LocatedToken::new(Token::Identifier(s), identifier_start_position))
            }
            Request::Nat32 => {
                self.nat32(request)
            },
            Request::Separator(separator_symbol) => {
                let c = self.read_char_or_fail_when_end()?;
                let c0 = match separator_symbol {
                    SeparatorSymbol::Comma => SeparatorSymbol::COMMA,
                    SeparatorSymbol::Or => SeparatorSymbol::OR,
                    SeparatorSymbol::And => SeparatorSymbol::AND,
                };
                if c == c0 {
                    let token_position = self.advance();
                    Ok(LocatedToken::new(Token::Separator(separator_symbol), token_position))
                } else {
                    Err(Error::Expected { requested: request, found: c.to_string() })
                }
            },
            Request::BindingSeparator => {
                self.match_string(token::BINDING_SEPARATOR, request, Token::BindingSeparator)
            },
            Request::End => {
                if self.tokens.is_empty() {
                    Ok(LocatedToken::new(Token::End, self.position))
                } else {
                    Err(Error::UnexpectedEnd) 
                }
            },
        }

    }

    pub fn consume_optional_or(&mut self) -> Result<(), Error> {
        self.consume_whitespace();
        let c = self.read_char_or_fail_when_end()?;
        if c == SeparatorSymbol::OR {
            self.advance();
        }
        Ok(())
    }

    fn nat32(&mut self, request: Request) -> Result<LocatedToken, Error> {
        fn digit(c: char) -> Option<u32> {
            match c {
                '0' => Some(0),
                '1' => Some(1),
                '2' => Some(2),
                '3' => Some(3),
                '4' => Some(4),
                '5' => Some(5),
                '6' => Some(6),
                '7' => Some(7),
                '8' => Some(8),
                '9' => Some(9),
                _ => None,
            }
        }

        // Consume one digit (fail otherwise)
        let c = self.read_char_or_fail_when_end()?;
        let mut sum: u32;
        let token_position: Position;
        match digit(c) {
            Some(d) => {
                token_position = self.advance();

                sum = d;
            },
            None => {
                return Err(Error::Expected { requested: request, found: c.to_string() })
            }
        }

        // Here atleast one digit was consumed.
        // Ignore all zeroes.
        if sum == 0 {
            loop {
                match self.read_char_or_fail_when_end() {
                    Ok('0') => {
                        self.advance();
                    },
                    Ok(_) => break,
                    Err(_) => return Ok(LocatedToken::new(Token::Nat32(0), token_position))
                }
            }
        }

        // Consume the rest of the consecutive digits, and
        // construct the actual number.
        while let Ok(c) = self.read_char_or_fail_when_end() {
            match digit(c) {
                Some(d) => {
                        self.advance();

                    // Watch out for 32 bit overflow.
                    match sum.checked_mul(10) {
                        Some(mul_10_sum) => {
                            match mul_10_sum.checked_add(d) {
                                Some(new_sum) => {
                                    sum = new_sum
                                },
                                None => return Err(Error::Nat32LiteralTooBig)
                            }
                        },
                        None => return Err(Error::Nat32LiteralTooBig)
                    }
                    
                },
                None => {
                    break
                }
            }
        }
            
        Ok(LocatedToken::new(Token::Nat32(sum), token_position))
    }

}

// pub fn example0() {
//     use Token::*;
//     use OperatorSymbol::*;
//     // let x = lex("(   1 + 2  * 3 )")
//     let l = vec![OpenParen, Nat32(1), Operator(Add), Nat32(2), Operator(Mul), Nat32(3), CloseParen];
// }

// pub fn example1() {
//     let s = "123x  foo";
//     let x = lex_int32(s);
//     println!("Result of lex: {:?}", x);
// }

// pub fn example2() {
//     let s = "(x  foo";
//     let x = lex_simple_symbol(s);
//     println!("Result of lex: {:?}", x);
// }

// pub fn example3() {
//     let s = "123+3+5*6";

//     println!("===========");
//     lex_until_end(s);
// }

// pub fn example4() {
//     let mut state = State::new("    xxx");
//     state.consume_whitespace();
//     println!("WS removed: {:?}", state.tokens());
// }

// pub fn example5() -> Result<(), Error> {
//     let s = " 000123555555 *  3  + 5*6";

//     let mut state = State::new(s);

//     let token = state.request(Request::Nat32)?;
//     println!("{:?}", token);
//     let token = state.request(Request::Separator(SeparatorSymbol::Mul))?;
//     println!("{:?}", token);
//     let token = state.request(Request::Nat32)?;
//     println!("{:?}", token);
//     let token = state.request(Request::Separator(SeparatorSymbol::Add))?;
//     println!("{:?}", token);
//     let token = state.request(Request::Nat32)?;
//     println!("{:?}", token);
//     let token = state.request(Request::Separator(SeparatorSymbol::Mul))?;
//     println!("{:?}", token);
//     let token = state.request(Request::Nat32)?;
//     println!("{:?}", token);
//     let token = state.request(Request::End)?;
//     println!("{:?}", token);

//     Ok(())
// }

