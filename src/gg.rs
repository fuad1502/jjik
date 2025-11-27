mod error;
mod lexer;
mod parser;

use std::rc::Rc;
use std::{collections::HashMap, path::Path};

use jlek::TokenSpec;

use crate::{NonTerminal, Priority, Rule, Symbol, Terminal, TerminalOrRule};

pub struct Gg {
    pub terminals: Vec<(Terminal, String)>,
    pub non_terminals: Vec<(NonTerminal, String)>,
    pub rules: Vec<Rc<Rule>>,
    pub priorities: HashMap<TerminalOrRule, Priority>,
    pub token_specs: Vec<TokenSpec>,
}

impl Gg {
    pub fn new(path: &Path) -> Result<Self, String> {
        let lexer = lexer::Lexer::new(path).map_err(|e| format!("IO error: {e}"))?;
        let parser = parser::Parser::new(lexer);
        parser.parse()
    }

    #[allow(dead_code)]
    pub fn simple_calculator() -> Self {
        let s_acc = NonTerminal { id: 0 };
        let e = NonTerminal { id: 2 };
        let number = Terminal::Other(0);
        let plus = Terminal::Other(1);
        let star = Terminal::Other(2);
        let left_paren = Terminal::Other(3);
        let right_paren = Terminal::Other(4);

        let rule_acc = Rule {
            head: s_acc,
            symbols: vec![Symbol::NonTerminal(e)],
        };
        let rule_0 = Rule {
            head: e,
            symbols: vec![
                Symbol::NonTerminal(e),
                Symbol::Terminal(plus),
                Symbol::NonTerminal(e),
            ],
        };
        let rule_1 = Rule {
            head: e,
            symbols: vec![
                Symbol::NonTerminal(e),
                Symbol::Terminal(star),
                Symbol::NonTerminal(e),
            ],
        };
        let rule_2 = Rule {
            head: e,
            symbols: vec![
                Symbol::Terminal(left_paren),
                Symbol::NonTerminal(e),
                Symbol::Terminal(right_paren),
            ],
        };
        let rule_3 = Rule {
            head: e,
            symbols: vec![Symbol::Terminal(number)],
        };

        let non_terminal_names = vec!["SAcc".to_string(), "E".to_string()];
        let terminal_names = vec![
            "Number".to_string(),
            "Plus".to_string(),
            "Star".to_string(),
            "LeftParen".to_string(),
            "RightParen".to_string(),
            "End".to_string(),
        ];
        let terminals = vec![number, plus, star, left_paren, right_paren, Terminal::End];
        let non_terminals = vec![s_acc, e];
        let rules = vec![rule_acc, rule_0, rule_1, rule_2, rule_3];
        let rules: Vec<Rc<Rule>> = rules.into_iter().map(Rc::new).collect();
        let mut priorities = HashMap::new();

        priorities.insert(TerminalOrRule::Rule(rules[2].clone()), Priority::new(4));
        priorities.insert(TerminalOrRule::Terminal(terminals[2]), Priority::new(3));
        priorities.insert(TerminalOrRule::Rule(rules[1].clone()), Priority::new(2));
        priorities.insert(TerminalOrRule::Terminal(terminals[1]), Priority::new(1));

        Gg {
            terminals: terminals.into_iter().zip(terminal_names).collect(),
            non_terminals: non_terminals.into_iter().zip(non_terminal_names).collect(),
            rules,
            priorities,
            token_specs: vec![],
        }
    }

    #[allow(dead_code)]
    pub fn example() -> Self {
        let s_acc = NonTerminal { id: 0 };
        let s = NonTerminal { id: 1 };
        let e = NonTerminal { id: 2 };
        let c = Terminal::Other(0);
        let d = Terminal::Other(1);

        let rule_acc = Rule {
            head: s_acc,
            symbols: vec![Symbol::NonTerminal(s)],
        };
        let rule_0 = Rule {
            head: s,
            symbols: vec![Symbol::NonTerminal(e), Symbol::NonTerminal(e)],
        };
        let rule_1 = Rule {
            head: e,
            symbols: vec![Symbol::Terminal(c), Symbol::NonTerminal(e)],
        };
        let rule_2 = Rule {
            head: e,
            symbols: vec![Symbol::Terminal(d)],
        };

        let non_terminal_names = vec!["SAcc".to_string(), "S".to_string(), "E".to_string()];
        let terminal_names = vec!["C".to_string(), "D".to_string(), "End".to_string()];
        let terminals = vec![c, d, Terminal::End];
        let non_terminals = vec![s_acc, s, e];
        let rules = vec![rule_acc, rule_0, rule_1, rule_2];
        let rules = rules.into_iter().map(Rc::new).collect();

        Gg {
            terminals: terminals.into_iter().zip(terminal_names).collect(),
            non_terminals: non_terminals.into_iter().zip(non_terminal_names).collect(),
            rules,
            priorities: HashMap::new(),
            token_specs: vec![],
        }
    }
}
