use crate::gg::{
    lexer::{self, Lexer, Span},
    parser,
};

pub struct SpannedError<E> {
    span: Span,
    error: E,
}

impl<E> SpannedError<E>
where
    E: std::error::Error,
{
    pub fn new(error: E, span: Span) -> Self {
        SpannedError { span, error }
    }

    pub fn report(&self, lexer: &Lexer) -> String {
        let span_str = lexer.show_span(&self.span);
        format!("{}:\n{span_str}", self.error)
    }
}

impl From<SpannedError<lexer::Error>> for SpannedError<parser::Error> {
    fn from(value: SpannedError<lexer::Error>) -> Self {
        let error = value.error;
        let span = value.span;
        let error = parser::Error::Lexer(error);
        Self::new(error, span)
    }
}
