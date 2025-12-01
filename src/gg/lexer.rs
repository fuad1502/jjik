use std::{fs::File, io::Read, path::Path};

use crate::gg::error::SpannedError;

#[derive(Clone)]
pub struct Token {
    span: Span,
    class: TokenClass,
}

#[derive(Clone)]
pub struct Span {
    start_pos: usize,
    end_pos: usize,
}

impl Token {
    pub fn class(&self) -> TokenClass {
        self.class
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl Span {
    fn new(start_pos: usize, end_pos: usize) -> Self {
        Self { start_pos, end_pos }
    }

    fn character(start_pos: usize) -> Self {
        Self {
            start_pos,
            end_pos: start_pos + 1,
        }
    }

    fn string(start_pos: usize, s: &str) -> Self {
        Self {
            start_pos,
            end_pos: start_pos + s.len(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenClass {
    TerminalsSection,
    RulesSection,
    PrioritiesSection,
    Identifier,
    Assignment,
    Or,
    Semicolon,
    LeftParen,
    RightParen,
    Number,
    String,
    End,
}

impl Token {
    fn left_paren(start_pos: usize) -> Self {
        Self {
            span: Span::character(start_pos),
            class: TokenClass::LeftParen,
        }
    }

    fn right_paren(start_pos: usize) -> Self {
        Self {
            span: Span::character(start_pos),
            class: TokenClass::RightParen,
        }
    }

    fn assignment(start_pos: usize) -> Self {
        Self {
            span: Span::character(start_pos),
            class: TokenClass::Assignment,
        }
    }

    fn or(start_pos: usize) -> Self {
        Self {
            span: Span::character(start_pos),
            class: TokenClass::Or,
        }
    }

    fn semicolon(start_pos: usize) -> Self {
        Self {
            span: Span::character(start_pos),
            class: TokenClass::Semicolon,
        }
    }

    fn string(start_pos: usize, string: &str) -> Self {
        Self {
            span: Span::new(start_pos, start_pos + string.len() + 2),
            class: TokenClass::String,
        }
    }

    fn number(start_pos: usize, end_pos: usize) -> Self {
        Self {
            span: Span::new(start_pos, end_pos),
            class: TokenClass::Number,
        }
    }

    fn terminals_section(start_pos: usize) -> Self {
        Self {
            span: Span::string(start_pos, &TokenClass::TerminalsSection.to_string()),
            class: TokenClass::TerminalsSection,
        }
    }

    fn rules_section(start_pos: usize) -> Self {
        Self {
            span: Span::string(start_pos, &TokenClass::RulesSection.to_string()),
            class: TokenClass::RulesSection,
        }
    }

    fn priorities_section(start_pos: usize) -> Self {
        Self {
            span: Span::string(start_pos, &TokenClass::PrioritiesSection.to_string()),
            class: TokenClass::PrioritiesSection,
        }
    }

    fn id(start_pos: usize, id: &str) -> Self {
        Self {
            span: Span::string(start_pos, id),
            class: TokenClass::Identifier,
        }
    }

    fn end(start_pos: usize) -> Self {
        Self {
            span: Span::new(start_pos, start_pos),
            class: TokenClass::End,
        }
    }
}

pub struct Lexer {
    chars: Vec<u8>,
    line_start_indices: Vec<usize>,
    start_pos: usize,
    current_pos: usize,
    current_token: Option<Token>,
}

impl Lexer {
    pub fn new(gg_path: &Path) -> Result<Self, std::io::Error> {
        let mut file = File::open(gg_path)?;
        let mut source = String::new();
        let _ = file.read_to_string(&mut source)?;
        let chars = source.chars().map(|c| c as u8).collect::<Vec<u8>>();
        let mut line_start_indices = chars
            .iter()
            .enumerate()
            .filter_map(|(i, c)| if *c == b'\n' { Some(i + 1) } else { None })
            .collect::<Vec<usize>>();
        line_start_indices.insert(0, 0);
        Ok(Self {
            chars,
            line_start_indices,
            start_pos: 0,
            current_pos: 0,
            current_token: None,
        })
    }

    pub fn next(&mut self) -> Result<Token, SpannedError<Error>> {
        let token = self.peek()?.clone();
        self.move_start_pos();
        self.current_token = None;
        Ok(token)
    }

    pub fn peek(&mut self) -> Result<&Token, SpannedError<Error>> {
        if self.current_token.is_none() {
            self.current_token = Some(self.get()?);
        }
        Ok(self.current_token.as_ref().unwrap())
    }

    pub fn get_lexeme(&self, token: &Token) -> &str {
        str::from_utf8(&self.chars[token.span.start_pos..token.span.end_pos]).unwrap()
    }

    pub fn show_span(&self, span: &Span) -> String {
        let line_number = self
            .line_start_indices
            .partition_point(|&i| i <= span.start_pos);
        let line_start_idx = self.line_start_indices[line_number - 1];
        let line_end_idx = self.line_start_indices[line_number] - 1;
        let line = &self.chars[line_start_idx..line_end_idx];
        let line = str::from_utf8(line).unwrap();
        let span_offset = span.start_pos - line_start_idx;
        let span_length = span.end_pos - span.start_pos;
        let span_marker = format!(
            "{}{}{}",
            " ".repeat(span_offset),
            "^",
            "-".repeat(span_length - 1)
        );
        format!("Line {line_number:3}|{line}\n         {span_marker}")
    }

    fn get(&mut self) -> Result<Token, SpannedError<Error>> {
        loop {
            match self.read_char() {
                Some(c) if c.is_whitespace() => {
                    self.move_start_pos();
                    continue;
                }
                Some('(') => {
                    return Ok(Token::left_paren(self.start_pos));
                }
                Some(')') => {
                    return Ok(Token::right_paren(self.start_pos));
                }
                Some('=') => {
                    return Ok(Token::assignment(self.start_pos));
                }
                Some('|') => {
                    return Ok(Token::or(self.start_pos));
                }
                Some(';') => {
                    return Ok(Token::semicolon(self.start_pos));
                }
                Some('\"') => {
                    return self.read_string();
                }
                Some('%') => return self.read_section_id(),
                Some(c) if c.is_ascii_digit() => return self.read_number(),
                Some(c) if c.is_ascii_alphabetic() => return self.read_id(c),
                Some(_) => return Err(self.unexpected_char("")),
                None => return Ok(Token::end(self.start_pos)),
            };
        }
    }

    fn read_number(&mut self) -> Result<Token, SpannedError<Error>> {
        loop {
            match self.read_char_if(|c| !Self::is_terminator(c)) {
                Some(c) if c.is_ascii_digit() => continue,
                Some(_) => return Err(self.unexpected_char("digit")),
                None => break,
            }
        }
        Ok(Token::number(self.start_pos, self.current_pos))
    }

    fn read_string(&mut self) -> Result<Token, SpannedError<Error>> {
        let mut string = vec![];
        loop {
            match self.read_char() {
                Some('"') => break,
                Some('\n') => return Err(self.unterminated_string()),
                Some(c) => string.push(c as u8),
                None => break,
            }
        }
        let string = String::from_utf8(string).unwrap();
        Ok(Token::string(self.start_pos, &string))
    }

    fn read_section_id(&mut self) -> Result<Token, SpannedError<Error>> {
        let mut id = vec![];
        loop {
            match self.read_char_if(|c| !Self::is_terminator(c)) {
                Some(c) if c.is_ascii_uppercase() => id.push(c as u8),
                Some(_) => return Err(self.unexpected_char("uppercase character")),
                None => break,
            }
        }
        let id = String::from_utf8(id).unwrap();
        match &id[..] {
            "TERMINALS" => Ok(Token::terminals_section(self.start_pos)),
            "RULES" => Ok(Token::rules_section(self.start_pos)),
            "PRIORITIES" => Ok(Token::priorities_section(self.start_pos)),
            _ => Err(SpannedError::new(
                Error::UnrecognizedSectionId(id),
                self.current_span(),
            )),
        }
    }

    fn read_id(&mut self, first_char: char) -> Result<Token, SpannedError<Error>> {
        let mut id = vec![first_char as u8];
        loop {
            match self.read_char_if(|c| !Self::is_terminator(c)) {
                Some(c) if c.is_ascii_alphanumeric() => id.push(c as u8),
                Some(_) => return Err(self.unexpected_char("alphanumeric character")),
                None => break,
            }
        }
        let id = String::from_utf8(id).unwrap();
        if Self::is_valid_id(&id) {
            return Ok(Token::id(self.start_pos, &id));
        }
        Err(SpannedError::new(Error::InvalidId(id), self.current_span()))
    }

    fn unexpected_char(&self, expected: &'static str) -> SpannedError<Error> {
        let span = Span::character(self.current_pos - 1);
        let actual = self.chars[self.current_pos - 1] as char;
        let error = Error::UnexpectedChar(actual, expected);
        SpannedError::new(error, span)
    }

    fn unterminated_string(&self) -> SpannedError<Error> {
        let span = self.current_span();
        let error = Error::UnterminatedString;
        SpannedError::new(error, span)
    }

    fn is_valid_id(id: &str) -> bool {
        id.chars().next().is_some_and(|c| c.is_ascii_uppercase())
    }

    fn is_terminator(ch: char) -> bool {
        ch.is_whitespace() || ch == ';' || ch == '(' || ch == ')'
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.get(self.current_pos).copied().map(|c| c as char)
    }

    fn read_char(&mut self) -> Option<char> {
        let ch = self.peek_char();
        if ch.is_some() {
            self.current_pos += 1;
        }
        ch
    }

    fn read_char_if(&mut self, predicate: fn(char) -> bool) -> Option<char> {
        let ch = self.peek_char();
        if ch.is_some() && predicate(ch.unwrap()) {
            self.current_pos += 1;
            return ch;
        }
        None
    }

    fn move_start_pos(&mut self) {
        self.start_pos = self.current_pos;
    }

    fn current_span(&self) -> Span {
        Span::new(self.start_pos, self.current_pos)
    }
}

impl std::fmt::Display for TokenClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenClass::TerminalsSection => write!(f, "%TERMINALS"),
            TokenClass::RulesSection => write!(f, "%RULES"),
            TokenClass::PrioritiesSection => write!(f, "%PRIORITIES"),
            TokenClass::Identifier => write!(f, "identifier"),
            TokenClass::Assignment => write!(f, "'='"),
            TokenClass::Or => write!(f, "'|'"),
            TokenClass::Semicolon => write!(f, "';'"),
            TokenClass::LeftParen => write!(f, "'('"),
            TokenClass::RightParen => write!(f, "')'"),
            TokenClass::Number => write!(f, "number"),
            TokenClass::String => write!(f, "string"),
            TokenClass::End => write!(f, "EOF"),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.class)
    }
}

pub enum Error {
    Io(std::io::Error),
    UnexpectedChar(char, &'static str),
    UnrecognizedSectionId(String),
    InvalidId(String),
    UnterminatedString,
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(error) => write!(f, "IO error: {error}"),
            Error::UnexpectedChar(found, expected) => write!(
                f,
                "Found unexpected character: {found}, expected: {expected}"
            ),
            Error::UnrecognizedSectionId(id) => write!(f, "Unrecognized section id: {id}"),
            Error::InvalidId(id) => write!(f, "Invalid formatted id: {id}"),
            Error::UnterminatedString => write!(f, "String not terminated"),
        }
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl std::error::Error for Error {}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use crate::gg::lexer::{Lexer, TokenClass};

    #[test]
    fn main() {
        let simple_calculator_gg = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("gg")
            .join("simple_calculator.gg");
        let mut lexer = Lexer::new(&simple_calculator_gg).unwrap();
        let mut tokens = vec![];
        loop {
            let token = match lexer.next() {
                Ok(t) => t,
                Err(e) => {
                    e.report(&lexer);
                    panic!()
                }
            };
            if token.class() == TokenClass::End {
                break;
            }
            tokens.push(token)
        }
        assert_eq!(tokens.len(), 49)
    }
}
