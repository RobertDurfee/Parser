#[macro_use]
extern crate lazy_static;

mod parser;
mod util;
mod char_class;

use std::collections::HashMap;
use crate::parser::Parsable;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Nonterminal {
    Expression,
    Sum,
    Primary,
    Whitespace,
    Number,
    Unknown,
}

impl Default for Nonterminal {
    fn default() -> Self { Nonterminal::Unknown }
}

fn main() {
    use Nonterminal::*;
    let definitions: HashMap<Nonterminal, Box<dyn Parsable<Nonterminal> + Sync>> = hashmap![
        Expression => nt!(Sum),
        Sum => cat!(nt!(Primary), rep!(cat!(lit!("+"), nt!(Primary)), Some(0), None)),
        Primary => alt!(nt!(Number), cat!(lit!("("), nt!(Sum), lit!(")"))),
        Whitespace => rep!(chc!("[ \\t\\r\\n]"), Some(1), None),
        Number => rep!(chc!("[0-9]"), Some(1), None)
    ];
    match nt!(Expression).parse("(1+2)+3", &definitions) {
        Ok(tree) => println!("{:#?}", tree),
        Err(msg) => println!("{}", msg),
    }
}
