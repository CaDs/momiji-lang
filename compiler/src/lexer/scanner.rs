use crate::errors::{LexerError, Span};
use crate::lexer::token::{Token, TokenKind};

/// The lexer/scanner for Momiji source code
pub struct Scanner<'a> {
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    current_pos: usize,
    start_pos: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.char_indices().peekable(),
            current_pos: 0,
            start_pos: 0,
        }
    }

    /// Tokenize the entire source code
    pub fn scan_all(mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.scan_token()?;
            let is_eof = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    /// Scan the next token
    pub fn scan_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace_and_comments()?;
        self.start_pos = self.current_pos;

        let Some((pos, ch)) = self.advance() else {
            return Ok(self.make_token(TokenKind::Eof));
        };
        self.start_pos = pos;

        match ch {
            // Single character tokens
            '(' => Ok(self.make_token(TokenKind::LParen)),
            ')' => Ok(self.make_token(TokenKind::RParen)),
            '[' => Ok(self.make_token(TokenKind::LBracket)),
            ']' => Ok(self.make_token(TokenKind::RBracket)),
            '{' => Ok(self.make_token(TokenKind::LBrace)),
            '}' => Ok(self.make_token(TokenKind::RBrace)),
            ',' => Ok(self.make_token(TokenKind::Comma)),
            ';' => Ok(self.make_token(TokenKind::Semicolon)),
            '~' => Ok(self.make_token(TokenKind::Tilde)),
            '@' => Ok(self.make_token(TokenKind::At)),
            '^' => Ok(self.make_token(TokenKind::Caret)),
            '%' => Ok(self.make_token(TokenKind::Percent)),
            '\n' => Ok(self.make_token(TokenKind::Newline)),

            // Two character tokens
            '+' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::PlusEq))
                } else {
                    Ok(self.make_token(TokenKind::Plus))
                }
            }
            '-' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::MinusEq))
                } else if self.match_char('>') {
                    Ok(self.make_token(TokenKind::Arrow))
                } else {
                    Ok(self.make_token(TokenKind::Minus))
                }
            }
            '*' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::StarEq))
                } else {
                    Ok(self.make_token(TokenKind::Star))
                }
            }
            '/' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::SlashEq))
                } else {
                    Ok(self.make_token(TokenKind::Slash))
                }
            }
            '=' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::EqEq))
                } else if self.match_char('>') {
                    Ok(self.make_token(TokenKind::FatArrow))
                } else {
                    Ok(self.make_token(TokenKind::Eq))
                }
            }
            '!' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::BangEq))
                } else {
                    Ok(self.make_token(TokenKind::Bang))
                }
            }
            '<' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::LtEq))
                } else if self.match_char('<') {
                    Ok(self.make_token(TokenKind::LtLt))
                } else {
                    Ok(self.make_token(TokenKind::Lt))
                }
            }
            '>' => {
                if self.match_char('=') {
                    Ok(self.make_token(TokenKind::GtEq))
                } else if self.match_char('>') {
                    Ok(self.make_token(TokenKind::GtGt))
                } else {
                    Ok(self.make_token(TokenKind::Gt))
                }
            }
            '&' => {
                if self.match_char('&') {
                    Ok(self.make_token(TokenKind::AmpAmp))
                } else {
                    Ok(self.make_token(TokenKind::Amp))
                }
            }
            '|' => {
                if self.match_char('|') {
                    Ok(self.make_token(TokenKind::PipePipe))
                } else {
                    Ok(self.make_token(TokenKind::Pipe))
                }
            }
            ':' => {
                if self.match_char(':') {
                    Ok(self.make_token(TokenKind::ColonColon))
                } else {
                    Ok(self.make_token(TokenKind::Colon))
                }
            }
            '.' => {
                if self.match_char('.') {
                    if self.match_char('.') {
                        Ok(self.make_token(TokenKind::DotDotDot))
                    } else {
                        Ok(self.make_token(TokenKind::DotDot))
                    }
                } else {
                    Ok(self.make_token(TokenKind::Dot))
                }
            }
            '?' => Ok(self.make_token(TokenKind::Question)),

            // String literals
            '"' => self.scan_string(),

            // Numbers
            c if c.is_ascii_digit() => self.scan_number(c),

            // Identifiers and keywords
            c if c.is_alphabetic() || c == '_' => self.scan_identifier(c),

            _ => Err(LexerError::UnexpectedCharacter {
                ch,
                span: self.current_span().into(),
            }),
        }
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        let result = self.chars.next();
        if let Some((pos, ch)) = result {
            self.current_pos = pos + ch.len_utf8();
        }
        result
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, ch)| *ch)
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn skip_whitespace_and_comments(&mut self) -> Result<(), LexerError> {
        loop {
            match self.peek() {
                Some(' ' | '\t' | '\r') => {
                    self.advance();
                }
                Some('#') => {
                    self.advance();
                    if self.match_char('[') {
                        // Block comment: #[ ... ]#
                        let start = self.current_pos - 2;
                        let mut depth = 1;
                        while depth > 0 {
                            match self.advance() {
                                Some((_, '#')) if self.match_char('[') => depth += 1,
                                Some((_, ']')) if self.match_char('#') => depth -= 1,
                                Some(_) => {}
                                None => {
                                    return Err(LexerError::UnterminatedComment {
                                        span: Span::new(start, self.current_pos).into(),
                                    });
                                }
                            }
                        }
                    } else {
                        // Line comment: # ...
                        while let Some(ch) = self.peek() {
                            if ch == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    }
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn scan_string(&mut self) -> Result<Token, LexerError> {
        let start = self.start_pos;
        let mut value = String::new();

        loop {
            match self.advance() {
                Some((_, '"')) => break,
                Some((_, '\\')) => {
                    // Escape sequences
                    match self.advance() {
                        Some((_, 'n')) => value.push('\n'),
                        Some((_, 't')) => value.push('\t'),
                        Some((_, 'r')) => value.push('\r'),
                        Some((_, '\\')) => value.push('\\'),
                        Some((_, '"')) => value.push('"'),
                        Some((_, '0')) => value.push('\0'),
                        Some((_, ch)) => {
                            value.push('\\');
                            value.push(ch);
                        }
                        None => {
                            return Err(LexerError::UnterminatedString {
                                span: Span::new(start, self.current_pos).into(),
                            });
                        }
                    }
                }
                Some((_, ch)) => value.push(ch),
                None => {
                    return Err(LexerError::UnterminatedString {
                        span: Span::new(start, self.current_pos).into(),
                    });
                }
            }
        }

        Ok(self.make_token(TokenKind::String(value)))
    }

    fn scan_number(&mut self, first: char) -> Result<Token, LexerError> {
        let mut value = String::from(first);
        let mut is_float = false;

        // Integer part
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() || ch == '_' {
                if ch != '_' {
                    value.push(ch);
                }
                self.advance();
            } else {
                break;
            }
        }

        // Decimal part
        if self.peek() == Some('.') {
            // Look ahead to make sure it's not a range operator (..)
            let mut lookahead = self.chars.clone();
            lookahead.next(); // consume the '.'
            if let Some((_, ch)) = lookahead.next() {
                if ch.is_ascii_digit() {
                    is_float = true;
                    self.advance(); // consume '.'
                    value.push('.');

                    while let Some(ch) = self.peek() {
                        if ch.is_ascii_digit() || ch == '_' {
                            if ch != '_' {
                                value.push(ch);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        // Exponent part
        if let Some('e' | 'E') = self.peek() {
            is_float = true;
            value.push(self.advance().unwrap().1);

            if let Some('+' | '-') = self.peek() {
                value.push(self.advance().unwrap().1);
            }

            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    value.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if is_float {
            match value.parse::<f64>() {
                Ok(n) => Ok(self.make_token(TokenKind::Float(n))),
                Err(_) => Err(LexerError::InvalidNumber {
                    span: self.current_span().into(),
                }),
            }
        } else {
            match value.parse::<i64>() {
                Ok(n) => Ok(self.make_token(TokenKind::Int(n))),
                Err(_) => Err(LexerError::InvalidNumber {
                    span: self.current_span().into(),
                }),
            }
        }
    }

    fn scan_identifier(&mut self, first: char) -> Result<Token, LexerError> {
        let mut value = String::from(first);

        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                value.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let kind = TokenKind::keyword(&value).unwrap_or(TokenKind::Identifier(value));
        Ok(self.make_token(kind))
    }

    fn current_span(&self) -> Span {
        Span::new(self.start_pos, self.current_pos)
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.current_span())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan(source: &str) -> Vec<TokenKind> {
        Scanner::new(source)
            .scan_all()
            .unwrap()
            .into_iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn test_keywords() {
        assert_eq!(
            scan("def end if else while for in return"),
            vec![
                TokenKind::Def,
                TokenKind::End,
                TokenKind::If,
                TokenKind::Else,
                TokenKind::While,
                TokenKind::For,
                TokenKind::In,
                TokenKind::Return,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_operators() {
        assert_eq!(
            scan("+ - * / = == != < <= > >= -> =>"),
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Eq,
                TokenKind::EqEq,
                TokenKind::BangEq,
                TokenKind::Lt,
                TokenKind::LtEq,
                TokenKind::Gt,
                TokenKind::GtEq,
                TokenKind::Arrow,
                TokenKind::FatArrow,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_integers() {
        assert_eq!(
            scan("0 42 1_000_000"),
            vec![
                TokenKind::Int(0),
                TokenKind::Int(42),
                TokenKind::Int(1000000),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_floats() {
        assert_eq!(
            scan("3.14 1.0e10 2.5E-3"),
            vec![
                TokenKind::Float(3.14),
                TokenKind::Float(1.0e10),
                TokenKind::Float(2.5e-3),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            scan(r#""hello" "world\n""#),
            vec![
                TokenKind::String("hello".to_string()),
                TokenKind::String("world\n".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(
            scan("foo bar_baz _private"),
            vec![
                TokenKind::Identifier("foo".to_string()),
                TokenKind::Identifier("bar_baz".to_string()),
                TokenKind::Identifier("_private".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_comments() {
        assert_eq!(
            scan("a # line comment\nb"),
            vec![
                TokenKind::Identifier("a".to_string()),
                TokenKind::Newline,
                TokenKind::Identifier("b".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_block_comments() {
        assert_eq!(
            scan("a #[ block comment ]# b"),
            vec![
                TokenKind::Identifier("a".to_string()),
                TokenKind::Identifier("b".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_hello_world() {
        let tokens = scan(
            r#"def main
  puts "Hello, Momiji!"
end"#,
        );
        assert_eq!(
            tokens,
            vec![
                TokenKind::Def,
                TokenKind::Identifier("main".to_string()),
                TokenKind::Newline,
                TokenKind::Identifier("puts".to_string()),
                TokenKind::String("Hello, Momiji!".to_string()),
                TokenKind::Newline,
                TokenKind::End,
                TokenKind::Eof,
            ]
        );
    }
}
