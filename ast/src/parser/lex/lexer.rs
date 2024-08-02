use crate::parser::lex::{
    token,
    token::{Token, SeparatorSymbol}
};
use std::str::Chars;

type Result<A> = std::result::Result<A, Error>;

#[derive(Debug)]
pub struct State<'a> {
    code_points: Chars<'a>,
    position: Position
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEnd,
    Expected { requested: Request, found: String },
    ExpectedI32 { found: String },
    ExpectedDeclarationKeyword,
    ExpectedTypeDeclarationKeyword,
    Int32LiteralOutOfBounds,
}

#[derive(Debug, Clone, Copy)]
pub struct Position { pub column: usize, pub line : usize }

impl Position {
    pub fn new_line(self) -> Self {
        Position { column: 1, line: self.line + 1 }
    }

    pub fn move_column_by(self, n: usize) -> Self {
        Position { column: self.column + n, line: self.line }
    }
}

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
    OpenAngle,
    CloseAngle,
    OpenCurly,
    CloseCurly,
    Quote,
    Keyword(token::Keyword),
    TypeDeclarationKeyword,
    Identifier,
    Separator(SeparatorSymbol),
    BindingSeparator,
    End,
}

#[derive(Debug)]
pub enum DeclarationKind {
    Type,
    Run,
    UserFunction,
    ForeignFunction,
}

enum WhitespaceState {
    ConsumeWhitespace,
    ConsumeAllUntilNewline,
}

impl <'state> State<'state> {
    // 'a lives atleast as long as 'state ('a contains 'state)
    pub fn new<'a: 'state>(str: &'a str) -> Self {
        Self {
            code_points: str.chars(),
            position: Position { column: 1, line: 1 },
        }
    }

    #[inline]
    pub fn position(&self) -> Position {
        self.position
    }

    pub fn clone(&self) -> Self {
        Self {
            code_points: self.code_points.clone(),
            position: self.position,
        }
    }

    pub fn consume_whitespace(&mut self) {
        use WhitespaceState::*;
        let mut ws_state = ConsumeWhitespace;

        loop {
            let tokens_backup = self.code_points.clone();
            let char = self.code_points.next(); 
            match ws_state {
                ConsumeWhitespace => {
                    match char {
                    Some('\n') => {
                        self.position = self.position.new_line();
                    },
                    Some(' ') | Some('\t') => {
                        self.position = self.position.move_column_by(1);
                    },
                    Some('/') => {
                        self.position = self.position.move_column_by(1);
                        ws_state = ConsumeAllUntilNewline;
                    },
                    Some(_) => {
                        self.code_points = tokens_backup; // backtrack
                        break
                    },
                    None => break,
                }
                },
                ConsumeAllUntilNewline => {
                    match char {
                    Some('\n') => {
                        self.position = self.position.new_line();
                        ws_state = ConsumeWhitespace;
                    },
                    Some(_) => {
                        self.position = self.position.move_column_by(1);
                    },
                    None => break,
                    }
                },
            }
        }
    }

    // Note that this returns the position BEFORE the advancement.
    pub fn advance(&mut self) -> Position {
        let previous_position = self.position;
        self.position = self.position.move_column_by(1);
        self.code_points.next();
        previous_position
    }

    pub fn consume_whitespace_or_fail_when_end(&mut self) -> Result<()> {
        self.consume_whitespace();
        match self.code_points.clone().next() {
            Some(_) => {
                Ok(())
            },
            None => Err(Error::UnexpectedEnd)
        }
    }

    pub fn read_char_or_fail_when_end(&mut self) -> Result<char> {
        match self.code_points.clone().next() {
            Some(c) => Ok(c),
            None => Err(Error::UnexpectedEnd) 
        }
    }

    pub fn match_string(&mut self, str: &str, request: Request, result_token: Token) -> Result<LocatedToken> {
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

    fn match_keyword(&mut self, request: Request, keyword: token::Keyword) -> Result<LocatedToken> {
        self.match_string(keyword.string(), request, Token::Keyword(keyword))
    }

    pub fn request(&mut self, request: Request) -> Result<LocatedToken> {
        self.consume_whitespace();
        match request {
            Request::OpenParen => {
                self.match_string("(", request, Token::OpenParen)
            },
            Request::CloseParen => {
                self.match_string(")", request, Token::CloseParen)
            },
            Request::OpenAngle => {
                self.match_string("<", request, Token::OpenAngle)
            },
            Request::CloseAngle => {
                self.match_string(">", request, Token::CloseAngle)
            },
            Request::OpenCurly => {
                self.match_string("{", request, Token::OpenCurly)
            },
            Request::CloseCurly => {
                self.match_string("}", request, Token::CloseCurly)
            },
            Request::Quote => {
                self.match_string("\"", request, Token::CloseCurly)
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
                if self.code_points.next().is_some() {
                    Ok(LocatedToken::new(Token::End, self.position))
                } else {
                    Err(Error::UnexpectedEnd) 
                }
            },
        }

    }

    pub fn request_keyword(&mut self, keyword: token::Keyword) -> Result<LocatedToken> {
        self.request(Request::Keyword(keyword))
    }

    pub fn consume_optional_or(&mut self) -> Result<()> {
        self.consume_whitespace();
        let c = self.read_char_or_fail_when_end()?;
        if c == SeparatorSymbol::OR {
            self.advance();
        }
        Ok(())
    }

    pub fn consume_optional_comma(&mut self) -> Result<()> {
        self.consume_whitespace();
        let c = self.read_char_or_fail_when_end()?;
        if c == SeparatorSymbol::COMMA {
            self.advance();
        }
        Ok(())
    }

    fn is_next_char(&mut self, c0: char) -> Result<bool> {
        self.consume_whitespace();
        match self.read_char_or_fail_when_end() {
            Ok(c) => Ok(c == c0),
            Err(Error::UnexpectedEnd) => Ok(false),
            Err(e) => Err(e)
        }
    }

    pub fn is_next_token_open_paren(&mut self) -> Result<bool> {
        self.is_next_char('(')
    }

    pub fn is_next_token_open_angle(&mut self) -> Result<bool> {
        self.is_next_char('<')
    }

    pub fn is_next_token_eq(&mut self) -> Result<bool> {
        self.is_next_char('=')
    }

    pub fn is_next_token_start_type_annotation(&mut self) -> Result<bool> {
        self.is_next_char('#')
    }

    pub fn commit_if_next_token_forall(&mut self) -> Result<bool> {
        self.consume_whitespace();
        let c = self.read_char_or_fail_when_end()?;
        if c == 'f' {
            self.match_keyword(Request::Keyword(token::Keyword::Forall), token::Keyword::Forall)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn peek_declaration_token(&mut self) -> Result<DeclarationKind> {
        self.consume_whitespace();
        let c = self.read_char_or_fail_when_end()?;
        match c {
            't' => Ok(DeclarationKind::Type),
            'r' => Ok(DeclarationKind::Run),
            'f' => {
                let saved_state = self.clone();
                self.advance();
                let c = self.read_char_or_fail_when_end()?;
                *self = saved_state;
                match c {
                    'o' => Ok(DeclarationKind::ForeignFunction),
                    'n' => Ok(DeclarationKind::UserFunction),
                    _ => Err(Error::ExpectedDeclarationKeyword),
                }
            },
            _ => Err(Error::ExpectedDeclarationKeyword),
        }
    }

    pub fn commit_if_next_token_int(&mut self) -> Result<Option<i32>> {
        fn digit(c: char) -> Option<i32> {
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

        self.consume_whitespace();

        let mut c = self.read_char_or_fail_when_end()?;
        let is_positive = match c {
            '-' => {
                self.advance();
                c = self.read_char_or_fail_when_end()?;
                false
            },
            _ if c.is_digit(10) => true,
            _ => return Ok(None),
        };
        // We commit.

        let mut sum: i32;
        match digit(c) {
            Some(d) => {
                sum = if is_positive { d } else { -d };
                self.advance();
            },
            None => return Err(Error::ExpectedI32 { found: c.to_string() })
        }

        // Atleast one digit was consumed.
        // Ignore all zeroes.
        if sum == 0 {
            loop {
                match self.read_char_or_fail_when_end() {
                    Ok('0') => {
                        self.advance();
                    },
                    Ok(_) => break,
                    Err(_) => return Ok(Some(0))
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
                            match mul_10_sum.checked_add(if is_positive { d } else { -d }) {
                                Some(new_sum) => {
                                    sum = new_sum
                                },
                                None => {
                                    return Err(Error::Int32LiteralOutOfBounds)
                                }
                            }
                        },
                        None => {
                            return Err(Error::Int32LiteralOutOfBounds)
                        }
                    }
                },
                None => break,
            }
        }
            
        Ok(Some(sum))
    }
}
