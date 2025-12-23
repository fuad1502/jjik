use core::{cmp::PartialEq, hash::Hash};
use std::{collections::HashMap, rc::Rc};

use crate::{NonTerminal, Priority, Rule, Symbol, Terminal, TerminalOrRule, gg::Gg};

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Debug)]
struct Item {
    rule: Rc<Rule>,
    position: usize,
    lookahead: Terminal,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct State {
    items: Vec<Item>,
}

#[derive(PartialEq, Eq)]
pub enum Action {
    Shift(Rc<State>),
    Reduce(Rc<Rule>),
    Accept,
    Error,
}

pub struct ParseTable {
    pub terminals: Vec<Terminal>,
    pub non_terminals: Vec<NonTerminal>,
    rules: Vec<Rc<Rule>>,
    symbols: Vec<Symbol>,
    first_table: HashMap<NonTerminal, Vec<Terminal>>,
    priorities: HashMap<TerminalOrRule, Priority>,
    pub states: Vec<Rc<State>>,
    pub goto_table: HashMap<Rc<State>, HashMap<Symbol, Rc<State>>>,
    pub action_table: HashMap<Rc<State>, HashMap<Terminal, Action>>,
    state_to_index: HashMap<Rc<State>, usize>,
    rule_to_index: HashMap<Rc<Rule>, usize>,
}

impl ParseTable {
    pub fn new(gg: &Gg) -> Self {
        let terminals: Vec<Terminal> = gg.terminals.iter().map(|t| t.0).collect();
        let non_terminals: Vec<NonTerminal> = gg.non_terminals.iter().map(|t| t.0).collect();
        let symbols = Self::chain_as_symbols(&terminals, &non_terminals);
        let mut rule_to_index = HashMap::new();
        for (idx, rule) in gg.rules.iter().enumerate() {
            rule_to_index.insert(rule.clone(), idx);
        }
        Self {
            terminals,
            non_terminals,
            priorities: gg.priorities.clone(),
            rules: gg.rules.clone(),
            symbols,
            first_table: HashMap::new(),
            states: vec![],
            goto_table: HashMap::new(),
            action_table: HashMap::new(),
            state_to_index: HashMap::new(),
            rule_to_index,
        }
        .create_first_table()
        .create_states()
        .create_action_table_for_all_states()
    }

    pub fn get_state_index(&self, state: &Rc<State>) -> usize {
        self.state_to_index[state]
    }

    pub fn get_rule_index(&self, rule: &Rule) -> usize {
        self.rule_to_index[rule]
    }

    fn create_first_table(mut self) -> Self {
        loop {
            let mut changed = false;
            for target_non_terminal in &self.non_terminals {
                for rule in self.rules_with_head(target_non_terminal) {
                    for symbol in &rule.symbols {
                        match symbol {
                            Symbol::NonTerminal(non_terminal) => {
                                if Self::copy_first_terminals(
                                    &mut self.first_table,
                                    non_terminal,
                                    target_non_terminal,
                                ) {
                                    changed = true
                                }
                                if !Self::first_contains_empty_terminal(
                                    &self.first_table,
                                    non_terminal,
                                ) {
                                    break;
                                }
                            }
                            Symbol::Terminal(terminal) => {
                                if Self::add_terminal_to_first(
                                    &mut self.first_table,
                                    target_non_terminal,
                                    terminal,
                                ) {
                                    changed = true;
                                }
                                break;
                            }
                        }
                    }
                }
            }
            if !changed {
                break;
            }
        }
        self
    }

    fn create_states(mut self) -> Self {
        let kernel_items = vec![Item {
            rule: self.rules[0].clone(),
            position: 0,
            lookahead: Terminal::End,
        }];
        let state_0 = Rc::new(self.closure(kernel_items));
        self.states.push(state_0.clone());
        let mut unvisited_states = vec![state_0];

        loop {
            if unvisited_states.is_empty() {
                break;
            }
            let current_state = unvisited_states.remove(0);
            for symbol in &self.symbols {
                if let Some(next_state) = self.goto(&current_state, *symbol) {
                    let next_state = if let Some(existing_state) =
                        self.states.iter().find(|s| ***s == next_state)
                    {
                        existing_state.clone()
                    } else {
                        let next_state = Rc::new(next_state);
                        self.states.push(next_state.clone());
                        self.state_to_index
                            .insert(next_state.clone(), self.states.len() - 1);
                        unvisited_states.push(next_state.clone());
                        next_state
                    };
                    Self::add_goto_entry(&mut self.goto_table, &current_state, symbol, &next_state);
                }
            }
        }
        self
    }

    fn create_action_table_for_all_states(mut self) -> Self {
        for state in &self.states {
            let actions = self.create_action_table_entry(state);
            self.action_table.insert(state.clone(), actions);
        }
        self
    }

    fn create_action_table_entry(&self, state: &Rc<State>) -> HashMap<Terminal, Action> {
        let mut action_map = HashMap::new();
        for terminal in &self.terminals {
            let mut action = self.deduce_action(state, terminal);
            if action == Action::Reduce(self.rules[0].clone()) {
                action = Action::Accept;
            }
            action_map.insert(*terminal, action);
        }
        action_map
    }

    fn deduce_action(&self, state: &Rc<State>, terminal: &Terminal) -> Action {
        let shift_action = Self::shift_action(state, &self.goto_table, terminal);
        let mut reduce_actions = Self::reduce_actions(state, terminal);
        match (shift_action, reduce_actions.len()) {
            (Some(action), 0) => action,
            (None, 1) => reduce_actions.pop().unwrap(),
            (None, 0) => Action::Error,
            (shift_action, _) => self.resolve_ambiguity(terminal, shift_action, reduce_actions),
        }
    }

    fn resolve_ambiguity(
        &self,
        terminal: &Terminal,
        shift_action: Option<Action>,
        reduce_actions: Vec<Action>,
    ) -> Action {
        let mut actions = reduce_actions;
        if let Some(action) = shift_action {
            actions.push(action);
        }
        actions
            .into_iter()
            .map(|action| match action {
                Action::Shift(next_state) => (
                    Action::Shift(next_state),
                    self.priorities
                        .get(&TerminalOrRule::Terminal(*terminal))
                        .cloned()
                        .unwrap_or(Priority::shift()),
                ),
                Action::Reduce(rule) => (
                    Action::Reduce(rule.clone()),
                    self.priorities
                        .get(&TerminalOrRule::Rule(rule.clone()))
                        .cloned()
                        .unwrap_or(Priority::reduce(self.get_rule_index(&rule))),
                ),
                _ => unreachable!("conflict resolution are only between shifts and/or reduces"),
            })
            .max_by_key(|(_, pri)| pri.clone())
            .unwrap()
            .0
    }

    fn shift_action(
        state: &Rc<State>,
        goto_table: &HashMap<Rc<State>, HashMap<Symbol, Rc<State>>>,
        terminal: &Terminal,
    ) -> Option<Action> {
        if !goto_table.contains_key(state) {
            return None;
        }
        goto_table
            .get(state)
            .unwrap()
            .iter()
            .find(|(symbol, _)| **symbol == Symbol::Terminal(*terminal))
            .map(|(_, state)| Action::Shift(state.clone()))
    }

    fn reduce_actions(state: &Rc<State>, terminal: &Terminal) -> Vec<Action> {
        state
            .items
            .iter()
            .filter(|i| i.lookahead == *terminal)
            .filter(|i| i.symbol_right_of_dot().is_none())
            .map(|i| Action::Reduce(i.rule.clone()))
            .collect()
    }

    fn chain_as_symbols(terminals: &[Terminal], non_terminals: &[NonTerminal]) -> Vec<Symbol> {
        let terminals = terminals.iter().map(|t| Symbol::Terminal(*t));
        let non_terminals = non_terminals.iter().map(|nt| Symbol::NonTerminal(*nt));
        terminals.chain(non_terminals).collect()
    }

    fn rules_with_head(&self, non_terminal: &NonTerminal) -> Vec<Rc<Rule>> {
        self.rules
            .iter()
            .filter(|r| r.head == *non_terminal)
            .map(Rc::clone)
            .collect()
    }

    fn copy_first_terminals(
        first_table: &mut HashMap<NonTerminal, Vec<Terminal>>,
        from: &NonTerminal,
        to: &NonTerminal,
    ) -> bool {
        if !first_table.contains_key(from) {
            return false;
        }
        let mut changed = false;
        let terminals = first_table.get(from).unwrap().clone();
        for terminal in &terminals {
            if Self::add_terminal_to_first(first_table, to, terminal) {
                changed = true;
            }
        }
        changed
    }

    fn first_contains_empty_terminal(
        first_table: &HashMap<NonTerminal, Vec<Terminal>>,
        non_terminal: &NonTerminal,
    ) -> bool {
        if !first_table.contains_key(non_terminal) {
            return false;
        }
        first_table
            .get(non_terminal)
            .unwrap()
            .contains(&Terminal::Empty)
    }

    fn add_terminal_to_first(
        first_table: &mut HashMap<NonTerminal, Vec<Terminal>>,
        non_terminal: &NonTerminal,
        terminal: &Terminal,
    ) -> bool {
        if let Some(terminals) = first_table.get_mut(non_terminal) {
            if terminals.contains(terminal) {
                return false;
            }
            terminals.push(*terminal);
            true
        } else {
            first_table.insert(*non_terminal, vec![*terminal]);
            true
        }
    }

    fn add_goto_entry(
        goto_table: &mut HashMap<Rc<State>, HashMap<Symbol, Rc<State>>>,
        state: &Rc<State>,
        symbol: &Symbol,
        next_state: &Rc<State>,
    ) {
        if let Some(entries) = goto_table.get_mut(state) {
            entries.insert(*symbol, next_state.clone());
        } else {
            let entries = HashMap::from([(*symbol, next_state.clone())]);
            goto_table.insert(state.clone(), entries);
        }
    }

    fn closure(&self, kernel_items: Vec<Item>) -> State {
        let mut items = kernel_items.clone();
        let mut unvisited_items = kernel_items;
        loop {
            if unvisited_items.is_empty() {
                break;
            }
            let item = unvisited_items.pop().unwrap();
            let non_terminal = match item.symbol_right_of_dot() {
                Some(Symbol::NonTerminal(non_terminal)) => non_terminal,
                _ => continue,
            };
            let lookaheads = self.first(item.symbol_after_right_of_dot().copied(), item.lookahead);
            for rule in self.rules.iter().filter(|r| r.head == *non_terminal) {
                for lookahead in &lookaheads {
                    let new_item = Item {
                        rule: rule.clone(),
                        position: 0,
                        lookahead: *lookahead,
                    };
                    if !items.contains(&new_item) {
                        items.push(new_item.clone());
                        unvisited_items.push(new_item);
                    }
                }
            }
        }
        items.sort();
        State { items }
    }

    fn goto(&self, state: &State, symbol: Symbol) -> Option<State> {
        let mut kernel_items = vec![];
        for item in &state.items {
            if item.symbol_right_of_dot() == Some(&symbol) {
                kernel_items.push(Item {
                    rule: item.rule.clone(),
                    position: item.position + 1,
                    lookahead: item.lookahead,
                })
            }
        }
        if kernel_items.is_empty() {
            return None;
        }
        Some(self.closure(kernel_items))
    }

    fn first(&self, symbol: Option<Symbol>, terminal: Terminal) -> Vec<Terminal> {
        match symbol {
            Some(Symbol::Terminal(terminal)) => vec![terminal],
            Some(Symbol::NonTerminal(non_terminal)) => {
                let mut terminals = self.first_table[&non_terminal].clone();
                if self.first_table[&non_terminal].contains(&Terminal::Empty) {
                    terminals.push(terminal);
                }
                terminals
            }
            None => vec![terminal],
        }
    }
}

impl Item {
    fn symbol_right_of_dot(&self) -> Option<&Symbol> {
        self.rule.symbols.get(self.position)
    }

    fn symbol_after_right_of_dot(&self) -> Option<&Symbol> {
        self.rule.symbols.get(self.position + 1)
    }
}

#[cfg(test)]
mod test {
    use core::assert_eq;

    use crate::{gg::Gg, parse_table::ParseTable};

    #[test]
    fn main() {
        let gg = Gg::example();
        let parse_table = ParseTable::new(&gg);
        assert_eq!(parse_table.states.len(), 10);
    }
}
