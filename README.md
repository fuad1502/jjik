# JJIK 🤢

[![CI](https://github.com/fuad1502/jjik/actions/workflows/CI.yml/badge.svg)](https://github.com/fuad1502/jjik/actions/workflows/CI.yml)

> [!NOTE]
> This project is part of the ["Compiler
> Toys"](https://github.com/fuad1502/compiler_toys) project, originally meant
> as a learning exercise on Compilers.

JJIK is a LR(1) parser generator similar to YACC.

JJIK generates Rust code for parsing the language you specify using a GG file.
Here is an example of a GG file for specifying simple arithmetic expressions:

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
GG file does not allow actions to be embedded into the grammar. Instead, JJIK
generates a `parser` module with a `parse` method that returns a concrete
syntax tree representation of your input. You can then insert your actions
during tree traversal.

## Documentation

All information on how to use JJIK, the syntax for specifying a grammar using
GG, and how to traverse the returned concrete syntax tree can be read in the
[crate's documentation](https://docs.rs/jjik/latest/jjik/).

Moreover, you can check out an example project utilizing JJIK for parsing
mathematical expressions
[here](https://github.com/fuad1502/compiler_toys/tree/master/jjik_simple_calculator).

## Features

### Concrete Syntax Tree construction

Unlike some parser generators out there, JJIK does not accept actions in the
grammar specification. The main reasons are to keep Rust code out of the GG
file and to keep things simple. The drawback of this approach is having the
concrete syntax tree explicitly constructed, which might be wasteful, compared
to, for example, directly constructing the abstract syntax tree, or evaluating
expressions.

### Error reporting

If there are any errors encountered during lexing or parsing, the parser
function will return an `Result::Err(String)` which explicitly show where the
error occured, and in case of syntax error, the list of expected tokens. For
example, if the string `"100 * 200 500"` is given as input to
[jjik_simple_calculator::calculate](https://github.com/fuad1502/compiler_toys/blob/master/jjik_simple_calculator/src/lib.rs#L56),
it will return the following error:

```text
Line   1|100 * 200 500
                   ^--
error: found Number, expected: [End, Plus, Minus, Star, Slash]
```
> [!NOTE]
> Currently, the parser will terminate as soon as an error is found. Later,
> I'll add support for synchronization non-terminal and token to recover from
> errors.

## Why is it called JJIK?

JJIK is pronounced like the Indonesian word *jijik* which means "disgusting"
and I usually pronounce the well known parser generator, YACC, as "yuck", a
word used to express disgust :)
