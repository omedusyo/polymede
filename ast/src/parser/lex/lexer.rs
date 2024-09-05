use crate::parser::lex::{
    token,
    token::{SeparatorSymbol, Token},
};
use std::str::Chars;

type Result<A> = std::result::Result<A, ErrorWithPosition>;

#[derive(Debug)]
pub struct State<'a> {
    chars: Chars<'a>, // UTF code-points iterator
    position: Position,
}

#[derive(Debug)]
pub struct ErrorWithPosition {
    pub position: Position,
    pub error: Error,
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEnd,
    Expected { requested: Request, found: String },
    ExpectedI32 { found: String },
    ExpectedF32 { found: String },
    ExpectedF32FailedToParse { error: std::num::ParseFloatError },
    ExpectedValidCharacterAfterEscapeSequenceInStringLiteral { found: String },
    ExpectedValidUnicodeSequenceInStringLiteral { found: String },
    ExpectedOpeningBraceForUnicodeSequenceInStringLiteral { found: String },
    ExpectedClosingBraceForUnicodeSequenceInStringLiteral { found: String },
    ExpectedDefinitionKeyword,
    ExpectedTypeDefinitionKeyword,
    Int32LiteralOutOfBounds,
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub column: usize,
    pub line: usize,
}

impl Position {
    pub fn new_line(self) -> Self {
        Position {
            column: 1,
            line: self.line + 1,
        }
    }

    pub fn move_column_by(self, n: usize) -> Self {
        Position {
            column: self.column + n,
            line: self.line,
        }
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
    TypeDefinitionKeyword,
    Identifier,
    Separator(SeparatorSymbol),
    BindingSeparator,
    End,
}

#[derive(Debug)]
pub enum DefinitionKind {
    Type,
    Run,
    UserFunction,
    ForeignFunction,
    MsgType,
}

enum WhitespaceState {
    ConsumeWhitespace,
    ConsumeAllUntilNewline,
}

impl<'state> State<'state> {
    // 'a lives atleast as long as 'state ('a contains 'state)
    pub fn new<'a: 'state>(str: &'a str) -> Self {
        Self {
            chars: str.chars(),
            position: Position { column: 1, line: 1 },
        }
    }

    #[inline]
    pub fn position(&self) -> Position {
        self.position
    }

    #[inline]
    pub fn chars(&'state mut self) -> &mut Chars<'state> {
        &mut self.chars
    }

    pub fn error<A>(&self, err: Error) -> Result<A> {
        Err(ErrorWithPosition {
            position: self.position,
            error: err,
        })
    }

    pub fn save_copy(&self) -> Self {
        Self {
            chars: self.chars.clone(),
            position: self.position,
        }
    }

    pub fn consume_whitespace(&mut self) {
        use WhitespaceState::*;
        let mut ws_state = ConsumeWhitespace;

        loop {
            let tokens_backup = self.chars.clone();
            let char = self.chars.next();
            match ws_state {
                ConsumeWhitespace => {
                    match char {
                        Some('\n') => {
                            self.position = self.position.new_line();
                        }
                        Some(' ') | Some('\t') => {
                            self.position = self.position.move_column_by(1);
                        }
                        Some('/') => {
                            self.position = self.position.move_column_by(1);
                            ws_state = ConsumeAllUntilNewline;
                        }
                        Some(_) => {
                            self.chars = tokens_backup; // backtrack
                            break;
                        }
                        None => break,
                    }
                }
                ConsumeAllUntilNewline => match char {
                    Some('\n') => {
                        self.position = self.position.new_line();
                        ws_state = ConsumeWhitespace;
                    }
                    Some(_) => {
                        self.position = self.position.move_column_by(1);
                    }
                    None => break,
                },
            }
        }
    }

    // Note that this returns the position BEFORE the advancement.
    pub fn advance(&mut self) -> Position {
        let previous_position = self.position;
        self.position = self.position.move_column_by(1);
        self.chars.next();
        previous_position
    }

    pub fn consume_whitespace_or_fail_when_end(&mut self) -> Result<()> {
        self.consume_whitespace();
        match self.chars.clone().next() {
            Some(_) => Ok(()),
            None => self.error(Error::UnexpectedEnd),
        }
    }

    pub fn read_char_or_fail_when_end(&mut self) -> Result<char> {
        match self.chars.clone().next() {
            Some(c) => Ok(c),
            None => self.error(Error::UnexpectedEnd),
        }
    }

    pub fn consume_char_or_fail_when_end(&mut self) -> Result<char> {
        match self.chars.next() {
            Some(c) => Ok(c),
            None => self.error(Error::UnexpectedEnd),
        }
    }

    pub fn match_string(
        &mut self,
        str: &str,
        request: Request,
        result_token: Token,
    ) -> Result<LocatedToken> {
        let mut chars_for_error: Vec<char> = vec![];
        let start_position = self.position();
        for c0 in str.chars() {
            let c = self.read_char_or_fail_when_end()?;
            chars_for_error.push(c);
            if c != c0 {
                return self.error(Error::Expected {
                    requested: request,
                    found: chars_for_error.into_iter().collect(),
                });
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
            Request::OpenParen => self.match_string("(", request, Token::OpenParen),
            Request::CloseParen => self.match_string(")", request, Token::CloseParen),
            Request::OpenAngle => self.match_string("<", request, Token::OpenAngle),
            Request::CloseAngle => self.match_string(">", request, Token::CloseAngle),
            Request::OpenCurly => self.match_string("{", request, Token::OpenCurly),
            Request::CloseCurly => self.match_string("}", request, Token::CloseCurly),
            Request::Quote => self.match_string("\"", request, Token::CloseCurly),
            Request::Keyword(keyword) => self.match_keyword(request, keyword),
            Request::TypeDefinitionKeyword => {
                let c = self.read_char_or_fail_when_end()?;
                if c == 'e' {
                    self.match_keyword(request, token::Keyword::Enum)
                } else if c == 'i' {
                    self.match_keyword(request, token::Keyword::Ind)
                } else {
                    self.error(Error::ExpectedTypeDefinitionKeyword)
                }
            }
            Request::Identifier => {
                let mut chars: Vec<char> = vec![];
                let identifier_start_position = {
                    let c = self.read_char_or_fail_when_end()?;
                    if c.is_alphabetic() || c == '_' {
                        chars.push(c);
                        self.advance()
                    } else {
                        return self.error(Error::Expected {
                            requested: request,
                            found: c.to_string(),
                        });
                    }
                };

                while let Ok(c) = self.read_char_or_fail_when_end() {
                    if c.is_alphanumeric() || c == '_' {
                        chars.push(c);
                        self.advance();
                    } else {
                        let s: String = chars.into_iter().collect();
                        return Ok(LocatedToken::new(
                            Token::Identifier(s),
                            identifier_start_position,
                        ));
                    }
                }
                let s: String = chars.into_iter().collect();
                Ok(LocatedToken::new(
                    Token::Identifier(s),
                    identifier_start_position,
                ))
            }
            Request::Separator(separator_symbol) => {
                let c = self.read_char_or_fail_when_end()?;
                let c0 = match separator_symbol {
                    SeparatorSymbol::Comma => SeparatorSymbol::COMMA,
                    SeparatorSymbol::Or => SeparatorSymbol::OR,
                    SeparatorSymbol::And => SeparatorSymbol::AND,
                };
                if c == c0 {
                    let token_position = self.advance();
                    Ok(LocatedToken::new(
                        Token::Separator(separator_symbol),
                        token_position,
                    ))
                } else {
                    self.error(Error::Expected {
                        requested: request,
                        found: c.to_string(),
                    })
                }
            }
            Request::BindingSeparator => {
                self.match_string(token::BINDING_SEPARATOR, request, Token::BindingSeparator)
            }
            Request::End => {
                if self.chars.next().is_some() {
                    Ok(LocatedToken::new(Token::End, self.position))
                } else {
                    self.error(Error::UnexpectedEnd)
                }
            }
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
            Err(ErrorWithPosition {
                error: Error::UnexpectedEnd,
                ..
            }) => Ok(false),
            Err(e) => Err(e),
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
            self.match_keyword(
                Request::Keyword(token::Keyword::Forall),
                token::Keyword::Forall,
            )?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn peek_definition_token(&mut self) -> Result<DefinitionKind> {
        self.consume_whitespace();
        let c = self.read_char_or_fail_when_end()?;
        match c {
            't' => Ok(DefinitionKind::Type),
            'r' => Ok(DefinitionKind::Run),
            'm' => Ok(DefinitionKind::MsgType),
            'f' => {
                let saved_state = self.save_copy();
                self.advance();
                let c = self.read_char_or_fail_when_end()?;
                *self = saved_state;
                match c {
                    'o' => Ok(DefinitionKind::ForeignFunction),
                    'n' => Ok(DefinitionKind::UserFunction),
                    _ => self.error(Error::ExpectedDefinitionKeyword),
                }
            }
            _ => self.error(Error::ExpectedDefinitionKeyword),
        }
    }

    pub fn commit_if_next_token_int(&mut self) -> Result<Option<i32>> {
        self.consume_whitespace();

        let mut c = self.read_char_or_fail_when_end()?;
        let is_positive = match c {
            '-' => {
                self.advance();
                c = self.read_char_or_fail_when_end()?;
                false
            }
            _ if c.is_ascii_digit() => true,
            _ => return Ok(None),
        };
        // We commit.

        let mut sum: i32;
        match digit10(c) {
            Some(d) => {
                sum = if is_positive { d } else { -d };
                self.advance();
            }
            None => {
                return self.error(Error::ExpectedI32 {
                    found: c.to_string(),
                })
            }
        }

        // Atleast one digit was consumed.
        // Ignore all zeroes.
        if sum == 0 {
            loop {
                match self.read_char_or_fail_when_end() {
                    Ok('0') => {
                        self.advance();
                    }
                    Ok(_) => break,
                    Err(_) => return Ok(Some(0)),
                }
            }
        }

        // Consume the rest of the consecutive digits, and
        // construct the actual number.
        while let Ok(c) = self.read_char_or_fail_when_end() {
            match digit10(c) {
                Some(d) => {
                    self.advance();

                    // Watch out for 32 bit overflow.
                    match sum.checked_mul(10) {
                        Some(mul_10_sum) => {
                            match mul_10_sum.checked_add(if is_positive { d } else { -d }) {
                                Some(new_sum) => sum = new_sum,
                                None => return self.error(Error::Int32LiteralOutOfBounds),
                            }
                        }
                        None => return self.error(Error::Int32LiteralOutOfBounds),
                    }
                }
                None => break,
            }
        }

        Ok(Some(sum))
    }

    pub fn commit_if_next_token_string_literal(&mut self) -> Result<Option<String>> {
        self.consume_whitespace();

        if self.read_char_or_fail_when_end()? != '"' {
            return Ok(None);
        }
        self.advance();

        let mut chars: Vec<char> = vec![];
        loop {
            match self.consume_char_or_fail_when_end()? {
                '\\' => match self.consume_char_or_fail_when_end()? {
                    'n' => chars.push('\n'),
                    '\\' => chars.push('\\'),
                    '"' => chars.push('"'),
                    '\'' => chars.push('\''),
                    't' => chars.push('\t'),
                    'u' => {
                        let open_brace = self.consume_char_or_fail_when_end()?;
                        if open_brace != '{' {
                            return self.error(
                                Error::ExpectedOpeningBraceForUnicodeSequenceInStringLiteral {
                                    found: format!("\\u{}", open_brace),
                                },
                            );
                        };

                        let x = self.hex_int()?;

                        let close_brace = self.consume_char_or_fail_when_end()?;
                        if close_brace != '}' {
                            return self.error(
                                Error::ExpectedClosingBraceForUnicodeSequenceInStringLiteral {
                                    found: format!("\\u{{{:x}{}", x, close_brace),
                                },
                            );
                        };

                        match char::from_u32(x) {
                            Some(c) => chars.push(c),
                            None => {
                                return self.error(
                                    Error::ExpectedValidUnicodeSequenceInStringLiteral {
                                        found: format!("\\u{{{:x}}}", x),
                                    },
                                )
                            }
                        }
                    }
                    c => {
                        return self.error(
                            Error::ExpectedValidCharacterAfterEscapeSequenceInStringLiteral {
                                found: format!("\\{}", c),
                            },
                        )
                    }
                },
                '"' => break,
                c => chars.push(c),
            }
        }
        Ok(Some(chars.into_iter().collect()))
    }

    // Note that it doesn't consume whitespace.
    fn hex_int(&mut self) -> Result<u32> {
        let c = self.consume_char_or_fail_when_end()?;
        let mut sum: u32 = match digit16(c) {
            Some(d) => d,
            None => {
                return self.error(Error::ExpectedValidUnicodeSequenceInStringLiteral {
                    found: format!("\\u{}", c),
                })
            }
        };

        // Ignore all zeroes.
        if sum == 0 {
            loop {
                match self.read_char_or_fail_when_end() {
                    Ok('0') => {
                        self.advance();
                    }
                    Ok(_) => break,
                    Err(_) => return Ok(0),
                }
            }
        }

        // Consume the rest of the consecutive digits, and
        // construct the actual number.
        while let Ok(c) = self.read_char_or_fail_when_end() {
            match digit16(c) {
                Some(d) => {
                    self.advance();

                    // Watch out for 32 bit overflow.
                    match sum.checked_mul(16) {
                        Some(mul_16_sum) => match mul_16_sum.checked_add(d) {
                            Some(new_sum) => sum = new_sum,
                            None => {
                                return self.error(
                                    Error::ExpectedValidUnicodeSequenceInStringLiteral {
                                        found: format!("\\u{:x}{:x}", sum, d),
                                    },
                                )
                            }
                        },
                        None => {
                            return self.error(Error::ExpectedValidUnicodeSequenceInStringLiteral {
                                found: format!("\\u{:x}{:x}", sum, d),
                            })
                        }
                    }
                }
                None => break,
            }
        }

        Ok(sum)
    }

    // We implement the following grammar
    //     Float  ::= Sign? Digit+ '.' Digit+ Exp?
    //     Exp    ::= 'e' Sign? Digit+
    //     Sign   ::= [+-]
    //     Digit  ::= [0-9]
    pub fn commit_if_next_token_f32(&mut self) -> Result<Option<f32>> {
        fn make_float(state: &State, chars: Vec<char>) -> Result<Option<f32>> {
            match chars.into_iter().collect::<String>().parse::<f32>() {
                Ok(float) => Ok(Some(float)),
                Err(error) => state.error(Error::ExpectedF32FailedToParse { error }),
            }
        }

        self.consume_whitespace();

        match self.read_char_or_fail_when_end()? {
            '%' => {
                self.advance();
            }
            _ => return Ok(None),
        }

        let mut chars: Vec<char> = vec![];

        let mut c = self.consume_char_or_fail_when_end()?;
        chars.push(c);

        let mut atleast_one_digit_before_point: bool = false;
        match c {
            '-' | '+' => {}
            _ => {
                if digit10(c).is_some() {
                    atleast_one_digit_before_point = true;
                } else {
                    return self.error(Error::ExpectedF32 {
                        found: chars.into_iter().collect(),
                    });
                }
            }
        };

        loop {
            c = self.consume_char_or_fail_when_end()?;
            chars.push(c);
            match c {
                '.' => {
                    if atleast_one_digit_before_point {
                        break;
                    } else {
                        return self.error(Error::ExpectedF32 {
                            found: chars.into_iter().collect(),
                        });
                    }
                }
                _ => {
                    if digit10(c).is_some() {
                        atleast_one_digit_before_point = true;
                    } else {
                        // neither a digit nor '.'
                        return self.error(Error::ExpectedF32 {
                            found: chars.into_iter().collect(),
                        });
                    }
                }
            }
        }
        // we can assume that we've seen a '.'
        let mut atleast_one_digit_after_point: bool = false;
        loop {
            c = self.read_char_or_fail_when_end()?;
            if digit10(c).is_some() {
                atleast_one_digit_after_point = true;
                chars.push(c);
                self.advance();
            } else if atleast_one_digit_after_point {
                break;
            } else {
                chars.push(c);
                return self.error(Error::ExpectedF32 {
                    found: chars.into_iter().collect(),
                });
            }
        }

        // check if we have an exponent
        match c {
            'e' => {
                self.advance();
                chars.push(c);

                let mut atleaset_one_digit_in_exponent: bool = false;
                c = self.consume_char_or_fail_when_end()?;
                chars.push(c);
                match c {
                    '-' | '+' => {}
                    _ => {
                        if digit10(c).is_some() {
                            atleaset_one_digit_in_exponent = true;
                        } else {
                            return self.error(Error::ExpectedF32 {
                                found: chars.into_iter().collect(),
                            });
                        }
                    }
                }

                loop {
                    c = self.read_char_or_fail_when_end()?;
                    if digit10(c).is_some() {
                        atleaset_one_digit_in_exponent = true;
                        chars.push(c);
                        self.advance();
                    } else if atleaset_one_digit_in_exponent {
                        break;
                    } else {
                        chars.push(c);
                        return self.error(Error::ExpectedF32 {
                            found: chars.into_iter().collect(),
                        });
                    }
                }
                make_float(self, chars)
            }
            _ => make_float(self, chars),
        }
    }
}

fn digit10(c: char) -> Option<i32> {
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

fn digit16(c: char) -> Option<u32> {
    match c.to_ascii_lowercase() {
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
        'a' => Some(10),
        'b' => Some(11),
        'c' => Some(12),
        'd' => Some(13),
        'e' => Some(14),
        'f' => Some(15),
        _ => None,
    }
}
