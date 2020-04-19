mod parser;
mod util;

use parser::Parsable;
use std::collections::HashMap;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Nonterminal {
    Semester,
    Season,
    Year,
    Number,
    Space,
    None,
}

impl Default for Nonterminal {
    fn default() -> Self { Nonterminal::None }
}

fn main() {
    let definitions: HashMap<Nonterminal, Box<dyn Parsable<_>>> = hashmap![
        Nonterminal::Semester => cat!(skp!(nt!(Nonterminal::Space)), nt!(Nonterminal::Season), skp!(nt!(Nonterminal::Space)), nt!(Nonterminal::Year), skp!(nt!(Nonterminal::Space))),
        Nonterminal::Season => alt!(cat!(skp!(nt!(Nonterminal::Space)), lit!("fall"), skp!(nt!(Nonterminal::Space))), cat!(skp!(nt!(Nonterminal::Space)), lit!("spring"), skp!(nt!(Nonterminal::Space)))),
        Nonterminal::Year => rep!(cat!(skp!(nt!(Nonterminal::Space)), nt!(Nonterminal::Number), skp!(nt!(Nonterminal::Space))), Some(2), Some(2)),
        Nonterminal::Number => alt!(lit!("0"), lit!("1"), lit!("2"), lit!("3"), lit!("4"), lit!("5"), lit!("6"), lit!("7"), lit!("8"), lit!("9")),
        Nonterminal::Space => lit!(" ")
    ];
    match nt!(Nonterminal::Semester).parse("     spring    2 3", &definitions) {
        Ok(tree) => println!("{:#?}", tree),
        Err(msg) => println!("Error: {}", msg),
    }
}
