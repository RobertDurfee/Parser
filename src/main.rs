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
    // @skip whitespace {
    //     expression ::= sum;
    //     sum ::= primary ('+' primary)*;
    //     primary ::= number | '(' sum ')';
    // }
    // whitespace ::= [ \t\r\n]+;
    // number ::= [0-9]+;
    let definitions: HashMap<Nonterminal, Box<dyn Parsable<Nonterminal> + Sync>> = hashmap![
        Expression => cat!(skp!(nt!(Whitespace)), nt!(Sum), skp!(nt!(Whitespace))),
        Sum => cat!(skp!(nt!(Whitespace)), nt!(Primary), skp!(nt!(Whitespace)),
            rep!(cat!(skp!(nt!(Whitespace)), lit!("+"), skp!(nt!(Whitespace)), nt!(Primary),
            skp!(nt!(Whitespace))), Some(0), None), skp!(nt!(Whitespace))),
        Primary => alt!(cat!(skp!(nt!(Whitespace)), nt!(Number), skp!(nt!(Whitespace))),
            cat!(skp!(nt!(Whitespace)), lit!("("), skp!(nt!(Whitespace)), nt!(Sum),
            skp!(nt!(Whitespace)), lit!(")"), skp!(nt!(Whitespace)))),
        Whitespace => rep!(chc!("[ \\t\\r\\n]"), Some(1), None),
        Number => rep!(chc!("[0-9]"), Some(1), None)
    ];
    match nt!(Expression).parse("(1+ \n2)+3", &definitions) {
        Ok(tree) => println!("{:#?}", tree),
        Err(msg) => println!("{}", msg),
    }
}
