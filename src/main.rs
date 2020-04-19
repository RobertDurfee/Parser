mod parser;
mod util;

use parser::Parsable;
use std::collections::HashMap;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Nonterminal {
    CharacterClass,
    Longhand,
    Negation,
    Shorthand,
    Range,
    Single,
    Any,
    Whitespace,
    NonWhitespace,
    Digit,
    NonDigit,
    Word,
    NonWord,
    None,
}

impl Default for Nonterminal {
    fn default() -> Self { Nonterminal::None }
}

fn main() {
    let definitions: HashMap<Nonterminal, Box<dyn Parsable<_>>> = hashmap![
        Nonterminal::CharacterClass => alt!(nt!(Nonterminal::Longhand), nt!(Nonterminal::Negation), nt!(Nonterminal::Shorthand)),
        Nonterminal::Negation => cat!(lit!("[^"), rep!(alt!(nt!(Nonterminal::Range), nt!(Nonterminal::Single), nt!(Nonterminal::Shorthand)), Some(1), None), lit!("]")),
        Nonterminal::Longhand => cat!(lit!("["), rep!(alt!(nt!(Nonterminal::Range), nt!(Nonterminal::Single), nt!(Nonterminal::Shorthand)), Some(1), None), lit!("]")),
        Nonterminal::Range => cat!(nt!(Nonterminal::Single), lit!("-"), nt!(Nonterminal::Single)),
        Nonterminal::Single => alt!(lit!(" "), lit!("!"), lit!("\""), lit!("#"), lit!("$"), lit!("%"), lit!("&"), lit!("'"), lit!("("), lit!(")"), lit!("*"), lit!("+"), lit!(","), lit!("-"), lit!("."), lit!("/"), lit!("0"), lit!("1"), lit!("2"), lit!("3"), lit!("4"), lit!("5"), lit!("6"), lit!("7"), lit!("8"), lit!("9"), lit!(":"), lit!(";"), lit!("<"), lit!("="), lit!(">"), lit!("?"), lit!("@"), lit!("A"), lit!("B"), lit!("C"), lit!("D"), lit!("E"), lit!("F"), lit!("G"), lit!("H"), lit!("I"), lit!("J"), lit!("K"), lit!("L"), lit!("M"), lit!("N"), lit!("O"), lit!("P"), lit!("Q"), lit!("R"), lit!("S"), lit!("T"), lit!("U"), lit!("V"), lit!("W"), lit!("X"), lit!("Y"), lit!("Z"), lit!("\\["), lit!("\\\\"), lit!("\\]"), lit!("\\^"), lit!("_"), lit!("`"), lit!("a"), lit!("b"), lit!("c"), lit!("d"), lit!("e"), lit!("f"), lit!("g"), lit!("h"), lit!("i"), lit!("j"), lit!("k"), lit!("l"), lit!("m"), lit!("n"), lit!("o"), lit!("p"), lit!("q"), lit!("r"), lit!("s"), lit!("t"), lit!("u"), lit!("v"), lit!("w"), lit!("x"), lit!("y"), lit!("z"), lit!("{"), lit!("|"), lit!("}"), lit!("~")),
        Nonterminal::Shorthand => alt!(nt!(Nonterminal::Any), nt!(Nonterminal::Whitespace), nt!(Nonterminal::NonWhitespace), nt!(Nonterminal::Digit), nt!(Nonterminal::NonDigit), nt!(Nonterminal::Word), nt!(Nonterminal::NonWord)),
        Nonterminal::Any => lit!("."),
        Nonterminal::Whitespace => lit!("\\s"),
        Nonterminal::NonWhitespace => lit!("\\S"),
        Nonterminal::Digit => lit!("\\d"),
        Nonterminal::NonDigit => lit!("\\D"),
        Nonterminal::Word => lit!("\\w"),
        Nonterminal::NonWord => lit!("\\W")
    ];
    match nt!(Nonterminal::CharacterClass).parse("[^\\w-\\W]", &definitions) {
        Ok(tree) => println!("{:#?}", tree),
        Err(msg) => println!("Error: {}", msg),
    }
}
