/*!
JJIK generates Rust code for parsing the language you specify using a GG file. Here is an example
of a GG file for specifying simple arithmetic expressions:

```text
%TERMINALS
Number("\d\d*")
Plus("+")
Star("\*")
LeftParen("\(")
RightParen("\)")

%RULES
E = E Plus E
| E Star E
| LeftParen E RightParen
| Number;

%PRIORITIES
E(2)
Star
E(1)
Plus
```
GG file does not allow actions to be embedded into the grammar. Instead, JJIK generates a `parser`
module with a `parse` method that returns a concrete syntax tree representation of your input. You
can then insert your actions during tree traversal. See [Usage](crate#usage) for a
detailed explanation on how to use JJIK.

# Usage

First, add JJIK as a build dependency:

```toml
[build-dependencies]
jjik = "0.1.0"
```
Then, create a GG file for specifying your language grammar. For example, create
`gg/simple_calculator.gg` containing the example GG file for specifying arithmetic expressions.

Next, create a build script (`build.rs`):

```rust
use std::path::PathBuf;

fn main() {
    let gg_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("gg")
        .join("simple_calculator.gg");
    let output_directory = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src");
    if let Err(e) = jjik::driver::run(&gg_file_path, &output_directory) {
        eprintln!("{}", e);
        panic!("Failed to compile .gg file!");
    }
    println!("cargo:rerun-if-changed=gg/simple_calculator.gg");
}
```
This will generate three Rust modules: `parser.rs`, `lexer.rs` and `symbol.rs` inside the `src`
folder when `cargo build` is executed. If there are any syntax errors in the GG file, `cargo build`
will emit those errors.

To use the parser, create `src/lib.rs` to include the generated modules:

```rust
mod lexer;
mod parser;
mod symbol;
```
Finally, construct the concrete syntax tree for your input:

```rust
let input_str = "1 + (2 + 3 * 4 - 5) + 6 / 2";
let mut lexer = lexer::Lexer::from_source_str(input_str);
let mut parser = parser::Parser::new();
let root = parser.parse(&mut lexer)?;
```
Once you constructed the concrete syntax tree, you can embed actions by traversing the tree. See
[Concrete Syntax Tree Data Structures](crate#concrete-syntax-tree-data-structures) to see how to
traverse the tree.

# GG Syntax

It is my hope that the GG grammar is self-explanatory after looking at the example above. However,
this section will try to formalize the grammar. Note that I will assume that readers are familir
with the terminologies used in [Backus Naur Form](en.wikipedia.org/wiki/Backus–Naur_form). The
following is the GG grammar described using [Extended Backus Naur
Form](en.wikipedia.org/wiki/Extended_Backus–Naur_form):

```ebnf
gg = "%TERMINALS", {terminal}, "%RULES", {rule}, "%PRIORITIES", {priority};
terminal = identifer, "(", string literal ,")"
rule = identifier, "=", (identifier)+, { "|", (identifier)+ }, ";"
priority = identifier, ["(", number, ")"]
```

A GG file consists of three sections: `%TERMINALS`, `%RULES`, and `%PRIORITIES`, which must be
placed in that specific order. Terminals are specified with an identifier followed by a
parenthesized string literal, e.g. `Number("\d\d*")`. The string literal is a regular expression
used to identify character strings in the input as the terminal. The supported regular expression
syntax is specified in [JLEK](). The identifier will be enumerated in `TerminalClass` in the
generated `symbol.rs`.

Rules consists of a head and a single or multiple productions separated by "|". Each production
consists of identifiers separated by whitespaces. If the identifier is not listed in the terminals
section, it is classified as a non-terminal and will be enumerated in `NonTerminalClass` along with
the head in the generated `symbol.rs`.

If the specified grammar is ambigous, during parse table creation, a shift-reduce or reduce-reduce
conflict can arise. The default action is to prioritize shifts over reductions and to reduce using
the earliest specified rule. However, you can explicitly assign a priority to each terminal and
rule production by listing it in the priorities section. Items listed first will be assigned a
higher priority. To list a terminal, simply write down the identifier. To list a rule production,
write down the rule head (non-terminal) identifier. However, if the non-terminal has multiple
productions associated with it, you must include a 1-based index to specify which production to
assign the priority to.

# Concrete Syntax Tree Data Structures

The concrete syntax tree nodes are `Symbol`s:

```rust
pub enum Symbol {
    NonTerminal(NonTerminal),
    Terminal(Terminal),
}

pub struct Terminal {
    class: TerminalClass,
    span: Span,
}

pub struct NonTerminal {
    pub rule: Rule,
    pub class: NonTerminalClass,
}

```
`Terminal`s are the leaf nodes, with a `class` to identify it, and a `span` to trace its location
back to the `Lexer`. To get the lexeme of a terminal, you have to query the `Lexer` through
`get_lexeme`:

```rust
pub fn get_lexeme(&self, token: &Terminal) -> &str
```
`NonTerminal` also have a `class` to identify it. However, it also has a `rule` associated with it,
through which we can traverse down the tree. As specified in the GG file, each rule has symbols
that it derives. These symbols are accessible through the `components` field of a `Rule`:

```
pub struct Rule {
    pub components: Vec<Symbol>,
    pub number: usize,
}
```
All of these data structures can be viewed in the generated `symbol.rs` file.
*/

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
