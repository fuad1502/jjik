mod code_gen;
pub mod driver;
mod gg;
mod parse_table;

use std::rc::Rc;

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Debug)]
struct Rule {
    head: NonTerminal,
    symbols: Vec<Symbol>,
}

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Copy, Clone, Debug)]
enum Symbol {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
}

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Copy, Clone, Debug)]
struct NonTerminal {
    id: usize,
}

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Copy, Clone, Debug)]
enum Terminal {
    End,
    Empty,
    Other(usize),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Priority {
    assigned_priority: Option<usize>,
    is_shift: Option<bool>,
    rule_order: Option<usize>,
}

#[derive(PartialEq, Eq, Hash, Clone)]
enum TerminalOrRule {
    Terminal(Terminal),
    Rule(Rc<Rule>),
}

impl Rule {
    fn num_of_components(&self) -> usize {
        self.symbols.len()
    }

    fn head(&self) -> &NonTerminal {
        &self.head
    }
}

impl Priority {
    fn new(priority: usize) -> Self {
        Self {
            assigned_priority: Some(priority),
            is_shift: None,
            rule_order: None,
        }
    }

    fn shift() -> Self {
        Self {
            assigned_priority: None,
            is_shift: Some(true),
            rule_order: None,
        }
    }

    fn reduce(rule_order: usize) -> Self {
        Self {
            assigned_priority: None,
            is_shift: Some(false),
            rule_order: Some(rule_order),
        }
    }
}
