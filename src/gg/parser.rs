use std::{collections::HashMap, rc::Rc};

use jlek::TokenSpec;

use crate::{
    NonTerminal, Priority, Rule, Symbol, Terminal, TerminalOrRule,
    gg::{error::SpannedError, lexer},
};

use super::{Gg, lexer::Lexer};

pub struct Parser {
    lexer: Lexer,
    terminals: Vec<(Terminal, String)>,
    non_terminals: Vec<(NonTerminal, String)>,
    rules: Vec<Rc<Rule>>,
    priorities: HashMap<TerminalOrRule, Priority>,
    token_specs: Vec<TokenSpec>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            lexer,
            terminals: vec![(Terminal::End, "End".to_string())],
            non_terminals: vec![(NonTerminal { id: 0 }, "SAcc".to_string())],
            rules: vec![],
            priorities: HashMap::new(),
            token_specs: vec![],
        }
    }

    pub fn parse(mut self) -> Result<Gg, String> {
        if let Err(err) = self.root() {
            return Err(err.report(&self.lexer));
        }
        let rule_acc = Rule {
            head: self.non_terminals[0].0,
            symbols: vec![Symbol::NonTerminal(self.non_terminals[1].0)],
        };
        self.rules.insert(0, Rc::new(rule_acc));
        Ok(Gg {
            terminals: self.terminals,
            non_terminals: self.non_terminals,
            rules: self.rules,
            priorities: self.priorities,
            token_specs: self.token_specs,
        })
    }

    fn root(&mut self) -> Result<(), SpannedError<Error>> {
        self.terminals_section()?;
        self.rules_section()?;
        self.priorities_section()?;
        let _ = self.expect(lexer::TokenClass::End)?;
        Ok(())
    }

    fn terminals_section(&mut self) -> Result<(), SpannedError<Error>> {
        let _ = self.expect(lexer::TokenClass::TerminalsSection)?;
        self.parse_tokens()
    }

    fn rules_section(&mut self) -> Result<(), SpannedError<Error>> {
        let _ = self.expect(lexer::TokenClass::RulesSection)?;
        self.parse_rules()
    }

    fn priorities_section(&mut self) -> Result<(), SpannedError<Error>> {
        let _ = self.expect(lexer::TokenClass::PrioritiesSection)?;
        self.parse_priorities()
    }

    fn parse_tokens(&mut self) -> Result<(), SpannedError<Error>> {
        while self.lexer.peek()?.class() == lexer::TokenClass::Identifier {
            self.parse_token()?;
        }
        Ok(())
    }

    fn parse_rules(&mut self) -> Result<(), SpannedError<Error>> {
        while self.lexer.peek()?.class() == lexer::TokenClass::Identifier {
            self.parse_rule()?;
        }
        Ok(())
    }

    fn parse_priorities(&mut self) -> Result<(), SpannedError<Error>> {
        while self.lexer.peek()?.class() == lexer::TokenClass::Identifier {
            self.parse_priority()?;
        }
        Ok(())
    }

    fn parse_rule(&mut self) -> Result<(), SpannedError<Error>> {
        let head = self.expect(lexer::TokenClass::Identifier)?;
        let _ = self.expect(lexer::TokenClass::Assignment)?;

        let mut rules_tokens = vec![vec![]];
        loop {
            let token = self.lexer.peek()?;
            let current_components = rules_tokens.last_mut().unwrap();
            if !current_components.is_empty() {
                if token.class() == lexer::TokenClass::Or {
                    _ = self.lexer.next()?;
                    rules_tokens.push(vec![]);
                    continue;
                } else if token.class() != lexer::TokenClass::Identifier {
                    break;
                }
            }
            let identifier = self.expect(lexer::TokenClass::Identifier)?;
            current_components.push(identifier);
        }

        let _ = self.expect(lexer::TokenClass::Semicolon)?;

        self.insert_rules(head, rules_tokens);

        Ok(())
    }

    fn parse_token(&mut self) -> Result<(), SpannedError<Error>> {
        let identifier = self.expect(lexer::TokenClass::Identifier)?;
        _ = self.expect(lexer::TokenClass::LeftParen)?;
        let pattern = self.expect(lexer::TokenClass::String)?;
        _ = self.expect(lexer::TokenClass::RightParen)?;
        let pattern = self.lexer.get_lexeme(&pattern);
        let pattern = &pattern[1..pattern.len() - 1];
        self.insert_terminal(identifier, pattern.to_string());

        Ok(())
    }

    fn parse_priority(&mut self) -> Result<(), SpannedError<Error>> {
        let identifier = self.expect(lexer::TokenClass::Identifier)?;
        let lexeme = self.lexer.get_lexeme(&identifier).to_string();
        if let Some(terminal) = self.get_terminal(&lexeme) {
            self.insert_terminal_priority(terminal);
        } else if let Some(non_terminal) = self.get_non_terminal(&lexeme) {
            if self.lexer.peek()?.class() == lexer::TokenClass::LeftParen {
                self.parse_rule_priority_with_order(non_terminal, lexeme)?;
            } else if self.number_of_rules(non_terminal) == 1 {
                let rule = self.get_rule(non_terminal, 0).unwrap();
                self.insert_rule_priority(rule);
            } else {
                let error = Error::RuleOrderNotProvidedInPriorities(lexeme);
                let span = identifier.span().clone();
                return Err(SpannedError::new(error, span));
            }
        } else {
            let error = Error::UnknownIdInPriorities(lexeme);
            let span = identifier.span().clone();
            return Err(SpannedError::new(error, span));
        }
        Ok(())
    }

    fn parse_rule_priority_with_order(
        &mut self,
        non_terminal: NonTerminal,
        lexeme: String,
    ) -> Result<(), SpannedError<Error>> {
        let _ = self.expect(lexer::TokenClass::LeftParen)?;
        let order_token = self.expect(lexer::TokenClass::Number)?;
        let order = str::parse::<usize>(self.lexer.get_lexeme(&order_token)).unwrap();
        if order == 0 {
            let error = Error::Generic("Order should be greater than 0");
            let span = order_token.span().clone();
            return Err(SpannedError::new(error, span));
        }
        if let Some(rule) = self.get_rule(non_terminal, order - 1) {
            self.insert_rule_priority(rule);
        } else {
            let error = Error::InvalidRuleInPriorities(lexeme, order);
            let span = order_token.span().clone();
            return Err(SpannedError::new(error, span));
        }
        let _ = self.expect(lexer::TokenClass::RightParen)?;
        Ok(())
    }

    fn insert_rules(&mut self, head: lexer::Token, rules_components: Vec<Vec<lexer::Token>>) {
        let head = self.insert_or_get_non_terminal(head);
        for components in rules_components {
            let mut symbols = vec![];
            for token in components {
                let symbol =
                    if let Some(terminal) = self.get_terminal(self.lexer.get_lexeme(&token)) {
                        Symbol::Terminal(terminal)
                    } else {
                        Symbol::NonTerminal(self.insert_or_get_non_terminal(token))
                    };
                symbols.push(symbol)
            }
            let rule = Rule { head, symbols };
            self.rules.push(Rc::new(rule));
        }
    }

    fn insert_or_get_non_terminal(&mut self, token: lexer::Token) -> NonTerminal {
        let lexeme = self.lexer.get_lexeme(&token);
        if let Some(non_terminal) = self.get_non_terminal(lexeme) {
            non_terminal
        } else {
            self.insert_non_terminal(token)
        }
    }

    fn insert_non_terminal(&mut self, token: lexer::Token) -> NonTerminal {
        let non_terminal_name = self.lexer.get_lexeme(&token);
        let non_terminal = NonTerminal {
            id: self.non_terminals.len(),
        };
        self.non_terminals
            .push((non_terminal, non_terminal_name.to_string()));
        non_terminal
    }

    fn insert_terminal(&mut self, token: lexer::Token, pattern: String) {
        let terminal_name = self.lexer.get_lexeme(&token);
        let terminal = Terminal::Other(self.terminals.len() - 1);
        self.terminals.push((terminal, terminal_name.to_string()));
        let token_spec = TokenSpec::new(terminal_name.to_string(), pattern);
        self.token_specs.push(token_spec);
    }

    fn get_non_terminal(&self, lexeme: &str) -> Option<NonTerminal> {
        self.non_terminals
            .iter()
            .find(|t| t.1 == lexeme)
            .map(|t| t.0)
    }

    fn get_terminal(&self, lexeme: &str) -> Option<Terminal> {
        self.terminals.iter().find(|t| t.1 == lexeme).map(|t| t.0)
    }

    fn insert_rule_priority(&mut self, rule: Rc<Rule>) {
        let terminal_or_rule = TerminalOrRule::Rule(rule);
        let priority = usize::MAX - self.priorities.len();
        self.priorities
            .insert(terminal_or_rule, Priority::new(priority));
    }

    fn insert_terminal_priority(&mut self, terminal: Terminal) {
        let terminal_or_rule = TerminalOrRule::Terminal(terminal);
        let priority = usize::MAX - self.priorities.len();
        self.priorities
            .insert(terminal_or_rule, Priority::new(priority));
    }

    fn expect(&mut self, expected: lexer::TokenClass) -> Result<lexer::Token, SpannedError<Error>> {
        let token = self.lexer.next()?;
        if token.class() != expected {
            let span = token.span().clone();
            let error = Error::Syntax(token, expected);
            return Err(SpannedError::new(error, span));
        }
        Ok(token)
    }

    fn number_of_rules(&self, non_terminal: NonTerminal) -> usize {
        self.rules.iter().filter(|r| r.head == non_terminal).count()
    }

    fn get_rule(&self, non_terminal: NonTerminal, order: usize) -> Option<Rc<Rule>> {
        self.rules
            .iter()
            .filter(|r| r.head == non_terminal)
            .nth(order)
            .cloned()
    }
}

pub enum Error {
    Lexer(lexer::Error),
    Syntax(lexer::Token, lexer::TokenClass),
    UnknownIdInPriorities(String),
    InvalidRuleInPriorities(String, usize),
    RuleOrderNotProvidedInPriorities(String),
    Generic(&'static str),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Lexer(error) => write!(f, "{error}"),
            Error::Syntax(token, expected) => {
                write!(f, "expected {expected} but found {token}")
            }
            Error::UnknownIdInPriorities(lexeme) => {
                write!(f, "{lexeme} is not declared as terminal / non-terminals")
            }
            Error::InvalidRuleInPriorities(non_terminal, order) => {
                let th_or_nd = if (order % 10) == 2 { "nd" } else { "th" };
                write!(
                    f,
                    "there are no {order}{th_or_nd} rule for non-terminal {non_terminal}"
                )
            }
            Error::RuleOrderNotProvidedInPriorities(non_terminal) => {
                write!(
                    f,
                    "there are multiple rules for {non_terminal} but rule order not specified"
                )
            }
            Error::Generic(message) => write!(f, "{message}"),
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

    use crate::{
        NonTerminal, Terminal, TerminalOrRule,
        gg::{lexer::Lexer, parser::Parser},
    };

    #[test]
    fn main() {
        let mut simple_calculator_gg = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        simple_calculator_gg.push("test/fixtures/simple_calculator.gg");
        let lexer = Lexer::new(&simple_calculator_gg).unwrap();
        let parser = Parser::new(lexer);
        let gg = parser.parse().unwrap();

        // Verify terminals field
        assert_eq!(gg.terminals.len(), 6);
        assert_eq!(gg.terminals[1], (Terminal::Other(0), "Number".to_string()));
        assert_eq!(
            gg.terminals[5],
            (Terminal::Other(4), "RightParen".to_string())
        );

        // Verify non-terminals field
        assert_eq!(gg.non_terminals.len(), 2);
        assert_eq!(
            gg.non_terminals[1],
            (NonTerminal { id: 1 }, "E".to_string())
        );

        // Verify rules field
        assert_eq!(gg.rules.len(), 5);

        // Verify priorities field
        assert_eq!(gg.priorities.len(), 4);
        assert_eq!(
            gg.priorities
                .get(&TerminalOrRule::Rule(gg.rules[2].clone()))
                .unwrap()
                .assigned_priority,
            Some(usize::MAX)
        );
        assert_eq!(
            gg.priorities
                .get(&TerminalOrRule::Rule(gg.rules[1].clone()))
                .unwrap()
                .assigned_priority,
            Some(usize::MAX - 2)
        );

        // Verify token specs field
        assert_eq!(gg.token_specs.len(), 5);
        assert_eq!(gg.token_specs[0].pattern(), "\\d\\d*");
    }
}
