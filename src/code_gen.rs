use core::fmt::Formatter;
use std::{
    fs::{File, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
    rc::Rc,
};

use jlek::TokenSpec;

use crate::{
    NonTerminal, Rule, Symbol, Terminal,
    gg::Gg,
    parse_table::{Action, ParseTable},
};

pub struct CodeGen {
    token_specs: Vec<TokenSpec>,
    parse_table: ParseTable,
    terminals: Vec<(Terminal, String)>,
    non_terminals: Vec<(NonTerminal, String)>,
    rules: Vec<Rc<Rule>>,
}

impl CodeGen {
    pub fn new(gg: Gg) -> Self {
        let parse_table = ParseTable::new(&gg);
        Self {
            token_specs: gg.token_specs,
            parse_table,
            terminals: gg.terminals,
            non_terminals: gg.non_terminals,
            rules: gg.rules,
        }
    }

    pub fn generate(&self, output_directory: &Path) -> Result<(), String> {
        jlek::generate(&self.token_specs, output_directory).map_err(|e| e.to_string())?;
        let mut parser_file = Self::create_file_at("parser.rs", output_directory)?;
        self.write_parser_file(&mut parser_file)
            .map_err(|e| e.to_string())?;
        let mut symbol_file = Self::create_file_at("symbol.rs", output_directory)?;
        self.write_symbol_file(&mut symbol_file)
            .map_err(|e| e.to_string())
    }

    fn create_file_at(name: &str, directory: &Path) -> Result<File, String> {
        let file_path = PathBuf::from(directory).join(name);
        OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(file_path)
            .map_err(|e| e.to_string())
    }

    fn write_parser_file(&self, parser_file: &mut File) -> std::io::Result<()> {
        Self::write_parser_uses(parser_file)?;
        self.write_parser_globals(parser_file)?;
        Self::write_state_and_action_struct(parser_file)?;
        Self::write_parser_struct(parser_file)?;
        self.write_parser_impl(parser_file)
    }

    fn write_symbol_file(&self, symbol_file: &mut File) -> std::io::Result<()> {
        Self::write_base_structs(symbol_file)?;
        self.write_non_terminal_class_enum(symbol_file)?;
        self.write_terminal_class_enum(symbol_file)?;
        self.write_terminal_class_from_impl(symbol_file)
    }

    fn write_parser_impl(&self, parser_file: &mut File) -> std::io::Result<()> {
        writeln!(parser_file, "impl Parser {{")?;
        self.write_parser_impl_new(parser_file)?;
        self.write_parser_impl_others(parser_file)?;
        writeln!(parser_file, "}}")?;
        Self::write_parser_default_impl(parser_file)
    }

    fn write_parser_uses(parser_file: &mut File) -> Result<(), std::io::Error> {
        writeln!(
            parser_file,
            r#"use super::{{
    lexer::Lexer,
    symbol::{{NonTerminal, NonTerminalClass, Rule, Symbol, Terminal, TerminalClass}},
}};
"#
        )
    }

    fn write_parser_globals(&self, parser_file: &mut File) -> Result<(), std::io::Error> {
        self.write_actions_global(parser_file)?;
        writeln!(parser_file)?;
        self.write_next_states_global(parser_file)?;
        writeln!(parser_file)?;
        self.write_rule_component_counts_global(parser_file)?;
        writeln!(parser_file)?;
        self.write_rule_heads_global(parser_file)?;
        writeln!(parser_file)
    }

    fn write_actions_global(&self, parser_file: &mut File) -> Result<(), std::io::Error> {
        let number_of_terminals = self.terminals.len();
        let number_of_states = self.parse_table.states.len();
        write!(
            parser_file,
            "static ACTIONS: [[Action; {number_of_terminals}]; {number_of_states}] = "
        )?;
        self.write_actions_init(parser_file)
    }

    fn write_next_states_global(&self, parser_file: &mut File) -> Result<(), std::io::Error> {
        let number_of_non_terminals = self.non_terminals.len();
        let number_of_states = self.parse_table.states.len();
        write!(
            parser_file,
            "static NEXT_STATES: [[Option<usize>; {number_of_non_terminals}]; {number_of_states}] = "
        )?;
        self.write_next_states_init(parser_file)
    }

    fn write_rule_component_counts_global(
        &self,
        parser_file: &mut File,
    ) -> Result<(), std::io::Error> {
        let number_of_rules = self.rules.len();
        write!(
            parser_file,
            "static RULE_COMPONENT_COUNTS: [usize; {number_of_rules}] = "
        )?;
        self.write_rule_component_counts_init(parser_file)
    }

    fn write_rule_heads_global(&self, parser_file: &mut File) -> Result<(), std::io::Error> {
        let number_of_rules = self.rules.len();
        write!(
            parser_file,
            "static RULE_HEADS: [NonTerminalClass; {number_of_rules}] = "
        )?;
        self.write_rule_heads_init(parser_file)
    }

    fn write_state_and_action_struct(parser_file: &mut File) -> Result<(), std::io::Error> {
        write!(
            parser_file,
            r#"struct State {{
    symbol: Option<Symbol>,
    number: usize,
}}

#[derive(Clone, Copy)]
enum Action {{
    Shift(usize),
    Reduce(usize),
    Accept,
    Error,
}}

"#
        )
    }

    fn write_parser_struct(parser_file: &mut File) -> std::io::Result<()> {
        write!(
            parser_file,
            r#"pub struct Parser {{
    state_stack: Vec<State>,
}}

"#
        )
    }

    fn write_parser_impl_new(&self, parser_file: &mut File) -> std::io::Result<()> {
        write!(
            parser_file,
            r#"    pub fn new() -> Self {{
        let initial_state = State {{
            symbol: None,
            number: 0,
        }};
        let state_stack = vec![initial_state];
        Self {{ state_stack }}
    }}
"#
        )
    }

    fn write_actions_init(&self, parser_file: &mut File) -> std::io::Result<()> {
        writeln!(parser_file, "[")?;
        let mut tabs = Tabs::new(0);
        tabs.indent();
        for state in &self.parse_table.states {
            let state_ptr = Rc::as_ptr(state);
            writeln!(parser_file, "{tabs}[")?;
            tabs.indent();
            for (terminal, _) in &self.terminals {
                let action = &self.parse_table.action_table[&state_ptr][terminal];
                writeln!(parser_file, "{tabs}Action::{},", self.action_string(action))?;
            }
            tabs.deindent();
            writeln!(parser_file, "{tabs}],")?;
        }
        tabs.deindent();
        writeln!(parser_file, "];")
    }

    fn write_next_states_init(&self, parser_file: &mut File) -> std::io::Result<()> {
        writeln!(parser_file, "[")?;
        let mut tabs = Tabs::new(0);
        tabs.indent();
        for state in &self.parse_table.states {
            let state_ptr = Rc::as_ptr(state);
            writeln!(parser_file, "{tabs}[")?;
            tabs.indent();
            for (non_terminal, _) in &self.non_terminals {
                if self.parse_table.goto_table.contains_key(&state_ptr)
                    && self.parse_table.goto_table[&Rc::as_ptr(state)]
                        .contains_key(&Symbol::NonTerminal(*non_terminal))
                {
                    let next_state = &self.parse_table.goto_table[&state_ptr]
                        [&Symbol::NonTerminal(*non_terminal)];
                    writeln!(
                        parser_file,
                        "{tabs}Some({}),",
                        self.parse_table.get_state_index(next_state)
                    )?;
                } else {
                    writeln!(parser_file, "{tabs}None, ")?;
                }
            }
            tabs.deindent();
            writeln!(parser_file, "{tabs}],")?;
        }
        tabs.deindent();
        writeln!(parser_file, "{tabs}];")
    }

    fn write_rule_component_counts_init(&self, parser_file: &mut File) -> std::io::Result<()> {
        write!(parser_file, "[",)?;
        for rule in &self.rules {
            write!(parser_file, "{}, ", rule.num_of_components())?;
        }
        writeln!(parser_file, "];")
    }

    fn write_rule_heads_init(&self, parser_file: &mut File) -> std::io::Result<()> {
        write!(parser_file, "[")?;
        for rule in &self.rules {
            let head_name = &self
                .non_terminals
                .iter()
                .find(|(non_terminal, _)| non_terminal == rule.head())
                .unwrap()
                .1;
            write!(parser_file, "NonTerminalClass::{head_name}, ")?;
        }
        writeln!(parser_file, "];")
    }

    fn write_parser_impl_others(&self, parser_file: &mut File) -> Result<(), std::io::Error> {
        let number_of_rules = self.rules.len();
        write!(
            parser_file,
            r#"
    pub fn parse(&mut self, lexer: &mut Lexer) -> Result<Symbol, String> {{
        loop {{
            let terminal = lexer.peek_token()?;
            match self.get_action(terminal.class()) {{
                Action::Shift(state_number) => {{
                    self.shift(lexer.next_token().unwrap(), state_number)
                }}
                Action::Reduce(rule_number) => match rule_number {{
                    rule_number if rule_number <= {number_of_rules} => self.reduce_rule(rule_number),
                    _ => unreachable!(),
                }},
                Action::Accept => return Ok(self.get_top_symbol()),
                Action::Error => return Err(self.report_error(lexer)),
            }}
        }}
    }}

    fn report_error(&self, lexer: &mut Lexer) -> String {{
        let terminal = lexer.peek_token().unwrap();
        let span = terminal.span().clone();
        let found = terminal.class();
        let expected = self.expected_classes();
        let span_str = lexer.show_span(&span);
        let red = "\x1B[31m";
        let end = "\x1B[0m";
        format!("{{span_str}}\n{{red}}error{{end}}: found {{found:?}}, expected: {{expected:?}}")
    }}

    fn expected_classes(&self) -> Vec<TerminalClass> {{
        ACTIONS[self.current_state_number()]
            .iter()
            .enumerate()
            .filter_map(|(i, a)| {{
                match a {{
                    Action::Error => None,
                    _ => Some(TerminalClass::from(i)),
                }}
            }}).collect()
    }}

    fn reduce_rule(&mut self, rule_number: usize) {{
        let non_terminal_class = RULE_HEADS[rule_number];
        let rule = Rule::new(
            rule_number,
            self.get_top_symbols(RULE_COMPONENT_COUNTS[rule_number]),
        );
        let non_terminal = NonTerminal::new(rule, non_terminal_class);
        let new_state = State {{
            symbol: Some(Symbol::NonTerminal(non_terminal)),
            number: self.next(non_terminal_class),
        }};
        self.state_stack.push(new_state);
    }}

    fn shift(&mut self, terminal: Terminal, next_state_number: usize) {{
        let new_state = State {{
            symbol: Some(Symbol::Terminal(terminal)),
            number: next_state_number,
        }};
        self.state_stack.push(new_state);
    }}

    fn get_top_symbol(&mut self) -> Symbol {{
        self.state_stack.pop().unwrap().symbol.unwrap()
    }}

    fn get_top_symbols(&mut self, n: usize) -> Vec<Symbol> {{
        self.state_stack
            .split_off(self.state_stack.len() - n)
            .into_iter()
            .map(|s| s.symbol.unwrap())
            .collect()
    }}

    fn get_action(&self, terminal_class: TerminalClass) -> Action {{
        ACTIONS[self.current_state_number()][terminal_class as usize]
    }}

    fn next(&self, non_terminal_class: NonTerminalClass) -> usize {{
        NEXT_STATES[self.current_state_number()][non_terminal_class as usize].unwrap()
    }}

    fn current_state_number(&self) -> usize {{
        self.state_stack.last().unwrap().number
    }}
"#
        )
    }

    fn write_parser_default_impl(parser_file: &mut File) -> Result<(), std::io::Error> {
        write!(
            parser_file,
            r#"
impl Default for Parser {{
    fn default() -> Self {{
        Self::new()
    }}
}}"#
        )
    }

    fn write_base_structs(symbol_file: &mut File) -> Result<(), std::io::Error> {
        write!(
            symbol_file,
            r#"use super::lexer::Lexer;

#[derive(Debug)]
pub enum Symbol {{
    NonTerminal(NonTerminal),
    Terminal(Terminal),
}}

#[derive(Debug)]
pub struct NonTerminal {{
    pub rule: Rule,
    pub class: NonTerminalClass,
}}

#[derive(Clone, Debug)]
pub struct Terminal {{
    class: TerminalClass,
    span: Span,
}}

#[derive(Clone, Debug)]
pub struct Span {{
    start_pos: usize,
    end_pos: usize,
}}

#[derive(Debug)]
pub struct Rule {{
    pub components: Vec<Symbol>,
    pub number: usize,
}}

impl Symbol {{
    pub fn pretty_print(&self, lexer: &Lexer, indent: usize) {{
        let indent_str = "    ".repeat(indent);
        match self {{
            Symbol::NonTerminal(non_terminal) => {{
                println!(
                    "{{indent_str}}{{:?}}({{}}):",
                    non_terminal.class, non_terminal.rule.number
                );
                for symbol in non_terminal.rule.components.iter() {{
                    symbol.pretty_print(lexer, indent + 1)
                }}
            }}
            Symbol::Terminal(terminal) => println!("{{indent_str}}{{}}", lexer.get_lexeme(terminal)),
        }}
    }}
}}

impl Terminal {{
    pub fn new(class: TerminalClass, span: Span) -> Self {{
        Self {{ class, span }}
    }}

    pub fn class(&self) -> TerminalClass {{
        self.class
    }}

    pub fn span(&self) -> &Span {{
        &self.span
    }}
}}

impl NonTerminal {{
    pub fn new(rule: Rule, class: NonTerminalClass) -> Self {{
        Self {{ rule, class }}
    }}
}}

impl Rule {{
    pub fn new(number: usize, components: Vec<Symbol>) -> Self {{
        Self {{ number, components }}
    }}
}}

impl Span {{
    pub fn new(start_pos: usize, end_pos: usize) -> Self {{
        Self {{ start_pos, end_pos }}
    }}

    pub fn start_pos(&self) -> usize {{
        self.start_pos
    }}

    pub fn end_pos(&self) -> usize {{
        self.end_pos
    }}
}}

"#
        )
    }

    fn write_non_terminal_class_enum(&self, symbol_file: &mut File) -> Result<(), std::io::Error> {
        writeln!(
            symbol_file,
            "#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]\n pub enum NonTerminalClass {{"
        )?;
        for (_, name) in &self.non_terminals {
            let tabs = Tabs::new(1);
            writeln!(symbol_file, "{tabs}{name},")?;
        }
        writeln!(symbol_file, "}}")?;
        writeln!(symbol_file)
    }

    fn write_terminal_class_enum(&self, symbol_file: &mut File) -> Result<(), std::io::Error> {
        writeln!(
            symbol_file,
            "#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]\n pub enum TerminalClass {{"
        )?;
        for (_, name) in &self.terminals {
            let tabs = Tabs::new(1);
            writeln!(symbol_file, "{tabs}{name},")?;
        }
        writeln!(symbol_file, "}}")
    }

    fn write_terminal_class_from_impl(&self, symbol_file: &mut File) -> Result<(), std::io::Error> {
        write!(
            symbol_file,
            r#"
impl From<usize> for TerminalClass {{
    fn from(value: usize) -> Self {{
        match value {{
"#
        )?;

        let tabs = Tabs::new(3);
        for (i, (_, name)) in self.terminals.iter().enumerate() {
            writeln!(symbol_file, "{tabs}{i} => TerminalClass::{name},")?;
        }
        write!(symbol_file, "{tabs}_ => panic!()")?;

        write!(
            symbol_file,
            r#"
        }}
    }}
}}
        "#
        )
    }

    fn action_string(&self, action: &Action) -> String {
        match action {
            Action::Shift(state) => {
                format!("Shift({})", self.parse_table.get_state_index(state))
            }
            Action::Reduce(rule) => {
                format!("Reduce({})", self.parse_table.get_rule_index(rule))
            }
            Action::Accept => "Accept".to_string(),
            Action::Error => "Error".to_string(),
        }
    }
}

#[derive(Default)]
struct Tabs {
    indent: usize,
    tab: String,
}

impl Tabs {
    const TAB: &'static str = "    ";

    pub fn new(indent: usize) -> Self {
        Self {
            indent,
            tab: Self::TAB.repeat(indent),
        }
    }

    pub fn indent(&mut self) {
        self.indent += 1;
        self.tab = Self::TAB.repeat(self.indent);
    }

    pub fn deindent(&mut self) {
        self.indent -= 1;
        self.tab = Self::TAB.repeat(self.indent);
    }
}

impl std::fmt::Display for Tabs {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        fmt.write_str(&self.tab)
    }
}
