use std::collections::{HashSet, HashMap};
use std::char;

use crate::parser;
use crate::{hashmap, alt, nt, lit, cat, plus};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Nonterminal {
    Root,
    Longhand,
    Union,
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
}

enum CharacterClass {
    Negation(Box<CharacterClass>),
    Union(Vec<Box<CharacterClass>>),
    Range(Box<CharacterClass>, Box<CharacterClass>),
    Single(String),
    Any,
    Whitespace,
    Digit,
    Word,
}

impl CharacterClass {
    const ANY_CHARACTERS: [char; 98] = [
        '\t', '\n', '\r', ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')',  '*', '+', ',', '-',
        '.',  '/',  '0',  '1', '2', '3', '4', '5', '6', '7', '8',  '9', ':',  ';', '<', '=', '>',
        '?',  '@',  'A',  'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',  'J', 'K',  'L', 'M', 'N', 'O', 
        'P',  'Q',  'R',  'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',  '[', '\\', ']', '^', '_', '`', 
        'a',  'b',  'c',  'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',  'l', 'm',  'n', 'o', 'p', 'q', 
        'r',  's',  't',  'u', 'v', 'w', 'x', 'y', 'z', '{', '|',  '}', '~'
    ];
    const WHITESPACE_CHARACTERS: [char; 4] = [ '\t', '\n', '\r', ' ' ];
    const DIGIT_CHARACTERS: [char; 10] = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ];
    const WORD_CHARACTERS: [char; 63] = [
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
        'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1',
        '2', '3', '4', '5', '6', '7', '8', '9', '_'
    ];

    fn new(tree: &parser::Tree<Nonterminal>) -> Result<CharacterClass, String> {
        match tree.name {
            Some(Nonterminal::Root) => return CharacterClass::new(tree.children.get(0).unwrap()),
            Some(Nonterminal::Longhand) => return CharacterClass::new(tree.children.get(0).unwrap()),
            Some(Nonterminal::Negation) => return Ok(CharacterClass::Negation(Box::new(CharacterClass::new(tree.children.get(0).unwrap())?))),
            Some(Nonterminal::Shorthand) => return CharacterClass::new(tree.children.get(0).unwrap()),
            Some(Nonterminal::Union) => {
                let mut char_classes = Vec::new();
                for child in &tree.children {
                    char_classes.push(Box::new(CharacterClass::new(&child)?));
                }
                return Ok(CharacterClass::Union(char_classes));
            }
            Some(Nonterminal::Range) => return Ok(CharacterClass::Range(Box::new(CharacterClass::new(tree.children.get(0).unwrap())?), Box::new(CharacterClass::new(tree.children.get(1).unwrap())?))),
            Some(Nonterminal::Single) => return Ok(CharacterClass::Single(tree.contents.clone())),
            Some(Nonterminal::Any) => return Ok(CharacterClass::Any),
            Some(Nonterminal::Whitespace) => return Ok(CharacterClass::Whitespace),
            Some(Nonterminal::NonWhitespace) => return Ok(CharacterClass::Negation(Box::new(CharacterClass::Whitespace))),
            Some(Nonterminal::Digit) => return Ok(CharacterClass::Digit),
            Some(Nonterminal::NonDigit) => return Ok(CharacterClass::Negation(Box::new(CharacterClass::Digit))),
            Some(Nonterminal::Word) => return Ok(CharacterClass::Word),
            Some(Nonterminal::NonWord) => return Ok(CharacterClass::Negation(Box::new(CharacterClass::Word))),
            _ => return Err(format!("Unknown nonterminal '{:?}'", tree.name)),
        }
    }

    fn characters(&self) -> Result<Vec<char>, String> {
        match *self {
            CharacterClass::Negation(ref char_class) => {
                let any_characters = CharacterClass::ANY_CHARACTERS.iter().cloned().collect::<HashSet<char>>();
                let characters = char_class.characters()?.iter().cloned().collect();
                let difference = any_characters.difference(&characters);
                return Ok(difference.cloned().collect());
            },
            CharacterClass::Union(ref char_classes) => {
                let mut result = Vec::new();
                for char_class in char_classes {
                    result.extend(char_class.characters()?);
                }
                return Ok(result);
            },
            CharacterClass::Range(ref left, ref right) => {
                // Safe to unwrap because left and right can only be Single
                let left_char = *left.characters().unwrap().get(0).unwrap();
                let right_char = *right.characters().unwrap().get(0).unwrap();
                if left_char > right_char {
                    return Err(format!("Range '{}-{}' is out of order", left_char, right_char));
                }
                let mut result = Vec::new();
                for i in (left_char as u32)..=(right_char as u32) {
                    result.push(char::from_u32(i).unwrap());
                }
                return Ok(result);
            },
            CharacterClass::Single(ref single) => {
                return match single.as_str() {
                    "\\t" => Ok(vec!['\t']),
                    "\\n" => Ok(vec!['\n']),
                    "\\r" => Ok(vec!['\r']),
                    "\\[" => Ok(vec!['[']),
                    "\\\\" => Ok(vec!['\\']),
                    "\\]" => Ok(vec![']']),
                    "\\^" => Ok(vec!['^']),
                    " " | "!" | "\"" | "#" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" | "," |
                    "-" | "." | "/" |  "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" |
                    ":" | ";" | "<" |  "=" | ">" | "?" | "@" | "A" | "B" | "C" | "D" | "E" | "F" |
                    "G" | "H" | "I" |  "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" |
                    "T" | "U" | "V" |  "W" | "X" | "Y" | "Z" | "_" | "`" | "a" | "b" | "c" | "d" |
                    "e" | "f" | "g" |  "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" |
                    "r" | "s" | "t" |  "u" | "v" | "w" | "x" | "y" | "z" | "{" | "|" | "}" | 
                    "~" => Ok(vec![single.chars().next().unwrap()]),
                    _ => Err(format!("Single '{}' is not valid", single)),
                };
            },
            CharacterClass::Any => Ok(CharacterClass::ANY_CHARACTERS.to_vec()),
            CharacterClass::Whitespace => Ok(CharacterClass::WHITESPACE_CHARACTERS.to_vec()),
            CharacterClass::Digit => Ok(CharacterClass::DIGIT_CHARACTERS.to_vec()),
            CharacterClass::Word => Ok(CharacterClass::WORD_CHARACTERS.to_vec()),
        }
    }
}

lazy_static! {
    // root ::= longhand | negation | shorthand;
    // longhand ::= '[' union ']';
    // negation ::= '[^' union ']';
    // shorthand ::= any | whitespace | nonwhitespace | digit | nondigit | word | nonword;
    // union ::= (range | single | shorthand)+;
    // range ::= single '-' single;
    // single ::= '\t' | '\n' | '\r' | ' ' | '!' | '"' | '#' | '$'  | '%'  | '&'  | '\'' |
    //            '('  | ')'  | '*'  | '+' | ',' | '-' | '.' | '/'  | '0'  | '1'  | '2'  |
    //            '3'  | '4'  | '5'  | '6' | '7' | '8' | '9' | ':'  | ';'  | '<'  | '='  |
    //            '>'  | '?'  | '@'  | 'A' | 'B' | 'C' | 'D' | 'E'  | 'F'  | 'G'  | 'H'  |
    //            'I'  | 'J'  | 'K'  | 'L' | 'M' | 'N' | 'O' | 'P'  | 'Q'  | 'R'  | 'S'  |
    //            'T'  | 'U'  | 'V'  | 'W' | 'X' | 'Y' | 'Z' | '\[' | '\\' | '\]' | '\^' |
    //            '_'  | '`'  | 'a'  | 'b' | 'c' | 'd' | 'e' | 'f'  | 'g'  | 'h'  | 'i'  |
    //            'j'  | 'k'  | 'l'  | 'm' | 'n' | 'o' | 'p' | 'q'  | 'r'  | 's'  | 't'  |
    //            'u'  | 'v'  | 'w'  | 'x' | 'y' | 'z' | '{' | '|'  | '}'  | '~';
    // any ::= '.';
    // whitespace ::= '\s';
    // nonwhitespace ::= '\S';
    // digit ::= '\d';
    // nondigit ::= '\D';
    // word ::= '\w';
    // nonword ::= '\W';
    static ref DEFINITIONS: HashMap<Nonterminal, Box<parser::Term<Nonterminal>>> = hashmap![
        Nonterminal::Root => alt!(
            nt!(Nonterminal::Longhand),
            nt!(Nonterminal::Negation),
            nt!(Nonterminal::Shorthand)
        ),
        Nonterminal::Longhand => cat!(
            lit!("["),
            nt!(Nonterminal::Union),
            lit!("]")
        ),
        Nonterminal::Negation => cat!(
            lit!("[^"),
            nt!(Nonterminal::Union),
            lit!("]")
        ),
        Nonterminal::Shorthand => alt!(
            nt!(Nonterminal::Any),
            nt!(Nonterminal::Whitespace),
            nt!(Nonterminal::NonWhitespace),
            nt!(Nonterminal::Digit),
            nt!(Nonterminal::NonDigit),
            nt!(Nonterminal::Word),
            nt!(Nonterminal::NonWord)
        ),
        Nonterminal::Union => plus!(
            alt!(
                nt!(Nonterminal::Range),
                nt!(Nonterminal::Single),
                nt!(Nonterminal::Shorthand)
            )
        ),
        Nonterminal::Range => cat!(
            nt!(Nonterminal::Single),
            lit!("-"),
            nt!(Nonterminal::Single)
        ),
        Nonterminal::Single => alt!(
            lit!("\\t"),  lit!("\\n"), lit!("\\r"), lit!(" "), lit!("!"), lit!("\""), lit!("#"),
            lit!("$"),    lit!("%"),   lit!("&"),   lit!("'"), lit!("("), lit!(")"),  lit!("*"),
            lit!("+"),    lit!(","),   lit!("-"),   lit!("."), lit!("/"), lit!("0"),  lit!("1"),
            lit!("2"),    lit!("3"),   lit!("4"),   lit!("5"), lit!("6"), lit!("7"),  lit!("8"),
            lit!("9"),    lit!(":"),   lit!(";"),   lit!("<"), lit!("="), lit!(">"),  lit!("?"),
            lit!("@"),    lit!("A"),   lit!("B"),   lit!("C"), lit!("D"), lit!("E"),  lit!("F"),
            lit!("G"),    lit!("H"),   lit!("I"),   lit!("J"), lit!("K"), lit!("L"),  lit!("M"),
            lit!("N"),    lit!("O"),   lit!("P"),   lit!("Q"), lit!("R"), lit!("S"),  lit!("T"),
            lit!("U"),    lit!("V"),   lit!("W"),   lit!("X"), lit!("Y"), lit!("Z"),  lit!("\\["),
            lit!("\\\\"), lit!("\\]"), lit!("\\^"), lit!("_"), lit!("`"), lit!("a"),  lit!("b"),
            lit!("c"),    lit!("d"),   lit!("e"),   lit!("f"), lit!("g"), lit!("h"),  lit!("i"),
            lit!("j"),    lit!("k"),   lit!("l"),   lit!("m"), lit!("n"), lit!("o"),  lit!("p"),
            lit!("q"),    lit!("r"),   lit!("s"),   lit!("t"), lit!("u"), lit!("v"),  lit!("w"),
            lit!("x"),    lit!("y"),   lit!("z"),   lit!("{"), lit!("|"), lit!("}"),  lit!("~")
        ),
        Nonterminal::Any => lit!("."),
        Nonterminal::Whitespace => lit!("\\s"),
        Nonterminal::NonWhitespace => lit!("\\S"),
        Nonterminal::Digit => lit!("\\d"),
        Nonterminal::NonDigit => lit!("\\D"),
        Nonterminal::Word => lit!("\\w"),
        Nonterminal::NonWord => lit!("\\W")
    ];
}

pub fn characters(input: &str) -> Result<Vec<char>, String> {
    return CharacterClass::new(&parser::parse(input, &DEFINITIONS, Nonterminal::Root)?)?.characters();
}
