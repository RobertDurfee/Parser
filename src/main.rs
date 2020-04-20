#[macro_use]
extern crate lazy_static;

mod parser;
mod util;
mod char_class;

use std::collections::HashMap;
use crate::parser::{Parsable, ParseTree};
use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Nonterminal {
    Root,
    SkipBlock,
    Production,
    Alternation,
    Concatenation,
    Repetition,
    Unit,
    RepeatOperator,
    Number,
    Range,
    UpperBound,
    LowerBound,
    Nonterminal,
    Terminal,
    QuotedString,
    CharacterSet,
    AnyCharacter,
    CharacterClass,
    Whitespace,
    Unknown,
}

impl Default for Nonterminal {
    fn default() -> Self { Nonterminal::Unknown }
}

#[derive(Debug, Clone)]
enum Grammar<T> {
    Root(Vec<Box<Grammar<T>>>),
    SkipBlock(T, Vec<Box<Grammar<T>>>),
    Production(T, Box<Grammar<T>>),
    Alternation(Vec<Box<Grammar<T>>>),
    Concatenation(Vec<Box<Grammar<T>>>),
    Repetition(Box<Grammar<T>>, Option<u32>, Option<u32>),
    Skip(Box<Grammar<T>>),
    Nonterminal(T),
    Literal(String),
    CharacterClass(String),
}

impl<T: Clone + Default + Debug + FromStr + Hash + PartialEq + Eq + Sync + 'static> Grammar<T> {
    fn new(tree: &ParseTree<Nonterminal>) -> Result<Grammar<T>, String> {
        match tree.name {
            Nonterminal::Root => {
                let mut grammars = Vec::new();
                for child in &tree.children {
                    match Grammar::new(&child) {
                        Ok(grammar) => grammars.push(Box::new(grammar)),
                        Err(msg) => return Err(msg),
                    }
                }
                return Ok(Grammar::Root(grammars));
            },
            Nonterminal::SkipBlock => {
                match T::from_str(&tree.children.get(0).unwrap().contents) {
                    Ok(nonterminal) => {
                        let mut grammars = Vec::new();
                        for child in &tree.children[1..] {
                            match Grammar::new(&child) {
                                Ok(grammar) => grammars.push(Box::new(grammar)),
                                Err(msg) => return Err(msg),
                            }
                        }
                        return Ok(Grammar::SkipBlock(nonterminal, grammars));
                    },
                    Err(_) => return Err(format!("Unable to parse '{:?}'", tree.children.get(0).unwrap()))
                }
            },
            Nonterminal::Production => {
                match T::from_str(&tree.children.get(0).unwrap().contents) {
                    Ok(nonterminal) => {
                        match Grammar::new(tree.children.get(1).unwrap()) {
                            Ok(grammar) => return Ok(Grammar::Production(nonterminal, Box::new(grammar))),
                            Err(msg) => return Err(msg),
                        }
                    },
                    Err(_) => return Err(format!("Unable to parse '{:?}'", tree.children.get(0).unwrap()))
                }
            },
            Nonterminal::Alternation => {
                if tree.children.len() > 1 {
                    let mut grammars = Vec::new();
                    for child in &tree.children {
                        match Grammar::new(&child) {
                            Ok(grammar) => grammars.push(Box::new(grammar)),
                            Err(msg) => return Err(msg),
                        }
                    }
                    return Ok(Grammar::Alternation(grammars));
                } else {
                    return Grammar::new(tree.children.get(0).unwrap());
                }
            },
            Nonterminal::Concatenation => {
                if tree.children.len() > 1 {
                    let mut grammars = Vec::new();
                    for child in &tree.children {
                        match Grammar::new(&child) {
                            Ok(grammar) => grammars.push(Box::new(grammar)),
                            Err(msg) => return Err(msg),
                        }
                    }
                    return Ok(Grammar::Concatenation(grammars));
                } else {
                    return Grammar::new(tree.children.get(0).unwrap());
                }
            },
            Nonterminal::Repetition => {
                match Grammar::new(tree.children.get(0).unwrap()) {
                    Ok(grammar) => {
                        if let Some(operator) = tree.children.get(1) {
                            if let Some(bound) = operator.children.get(0) {
                                match bound.name {
                                    Nonterminal::Number => {
                                        let bound = bound.contents.parse::<u32>().unwrap();
                                        return Ok(Grammar::Repetition(Box::new(grammar), Some(bound), Some(bound)));
                                    },
                                    Nonterminal::Range => {
                                        let lower_bound = bound.children.get(0).unwrap().contents.parse::<u32>().unwrap();
                                        let upper_bound = bound.children.get(1).unwrap().contents.parse::<u32>().unwrap();
                                        return Ok(Grammar::Repetition(Box::new(grammar), Some(lower_bound), Some(upper_bound)));
                                    },
                                    Nonterminal::LowerBound => {
                                        let lower_bound = bound.children.get(0).unwrap().contents.parse::<u32>().unwrap();
                                        return Ok(Grammar::Repetition(Box::new(grammar), Some(lower_bound), None));
                                    },
                                    Nonterminal::UpperBound => {
                                        let upper_bound = bound.children.get(0).unwrap().contents.parse::<u32>().unwrap();
                                        return Ok(Grammar::Repetition(Box::new(grammar), None, Some(upper_bound)));
                                    },
                                    _ => return Err(format!("Invalid repetition bound '{:?}'", bound.name)),
                                }
                            } else {
                                match operator.contents.as_str() {
                                    "?" => return Ok(Grammar::Repetition(Box::new(grammar), Some(0), Some(1))),
                                    "*" => return Ok(Grammar::Repetition(Box::new(grammar), Some(0), None)),
                                    "+" => return Ok(Grammar::Repetition(Box::new(grammar), Some(1), None)),
                                    _ => return Err(format!("Invalid repetition operator '{}'", operator.contents)),
                                }
                            }
                        } else {
                            return Ok(grammar);
                        }
                    },
                    Err(msg) => return Err(msg),
                }
            },
            Nonterminal::Unit => Grammar::new(tree.children.get(0).unwrap()),
            Nonterminal::Nonterminal => {
                match T::from_str(&tree.contents) {
                    Ok(nonterminal) => Ok(Grammar::Nonterminal(nonterminal)),
                    Err(_) => Err(format!("Unable to parse '{:?}'", tree.contents)),
                }
            },
            Nonterminal::Terminal => Grammar::new(tree.children.get(0).unwrap()),
            Nonterminal::QuotedString => Ok(Grammar::Literal(tree.contents.clone())),
            Nonterminal::CharacterSet => Ok(Grammar::CharacterClass(tree.contents.clone())),
            Nonterminal::AnyCharacter => Ok(Grammar::CharacterClass(tree.contents.clone())),
            Nonterminal::CharacterClass => Ok(Grammar::CharacterClass(tree.contents.clone())),
            _ => Err(format!("'{:?}' is not valid nonterminal", tree.name)),
        }
    }

    fn definitions(&self) -> Result<HashMap<T, Box<dyn Parsable<T> + Sync>>, String> {
        match *self {
            Grammar::Root(ref grammars) => {
                let mut result = HashMap::new();
                for grammar in grammars {
                    match grammar.definitions() {
                        Ok(definitions) => {
                            result.extend(definitions);
                        },
                        Err(msg) => return Err(msg),
                    }
                }
                return Ok(result);
            },
            Grammar::SkipBlock(ref skip_nonterminal, ref grammars) => {
                let mut result = HashMap::new();
                for grammar in grammars {
                    match grammar.skip(skip_nonterminal).definitions() {
                        Ok(definitions) => {
                            result.extend(definitions);
                        },
                        Err(msg) => return Err(msg),
                    }
                }
                return Ok(result);
            },
            Grammar::Production(ref nonterminal, ref grammar) => {
                match grammar.definition() {
                    Ok(definition) => {
                        return Ok(hashmap![nonterminal.clone() => definition]);
                    },
                    Err(msg) => return Err(msg),
                }
            },
            _ => Err(format!("Cannot get definitions for {:?}", *self)),
        }
    }

    fn definition(&self) -> Result<Box<dyn Parsable<T> + Sync>, String> {
        match *self {
            Grammar::Alternation(ref grammars) => {
                let mut definitions = Vec::new();
                for grammar in grammars {
                    match grammar.definition() {
                        Ok(definition) => definitions.push(definition),
                        Err(msg) => return Err(msg),
                    }
                }
                return Ok(Box::new(parser::Alternation::new(definitions)));
            },
            Grammar::Concatenation(ref grammars) => {
                let mut definitions = Vec::new();
                for grammar in grammars {
                    match grammar.definition() {
                        Ok(definition) => definitions.push(definition),
                        Err(msg) => return Err(msg),
                    }
                }
                return Ok(Box::new(parser::Concatenation::new(definitions)));
            },
            Grammar::Repetition(ref grammar, ref min, ref max) => {
                match grammar.definition() {
                    Ok(definition) => Ok(Box::new(parser::Repetition::new(definition, *min, *max))),
                    Err(msg) => Err(msg),
                }
            },
            Grammar::Skip(ref grammar) => {
                match grammar.definition() {
                    Ok(definition) => Ok(Box::new(parser::Skip::new(definition))),
                    Err(msg) => Err(msg),
                }
            },
            Grammar::Nonterminal(ref nonterminal) => Ok(Box::new(parser::Nonterminal::new(nonterminal.clone()))),
            Grammar::Literal(ref literal) => Ok(Box::new(parser::Literal::new(&literal))),
            Grammar::CharacterClass(ref character_class) => Ok(Box::new(parser::CharacterClass::new(&character_class))),
            _ => Err(format!("Cannot get definition for {:?}", *self)),
        }
    }

    fn skip(&self, nonterminal: &T) -> Grammar<T> {
        match *self {
            Grammar::Nonterminal(_) |
            Grammar::Literal(_) |
            Grammar::CharacterClass(_) => {
                return Grammar::Concatenation(vec![Box::new(Grammar::Skip(Box::new(Grammar::Nonterminal(nonterminal.clone())))), Box::new(self.clone()), Box::new(Grammar::Skip(Box::new(Grammar::Nonterminal(nonterminal.clone()))))]);
            },
            _ => return self.clone(),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum MyNonterminal {
    Expression,
    Sum,
    Primary,
    Number,
    Whitespace,
    Unknown,
}

impl Default for MyNonterminal {
    fn default() -> Self { MyNonterminal::Unknown }
}

impl FromStr for MyNonterminal {
    type Err = String;

    fn from_str(my_nonterminal: &str) -> Result<Self, Self::Err> {
        match my_nonterminal {
            "expression" => Ok(MyNonterminal::Expression),
            "sum" => Ok(MyNonterminal::Sum),
            "primary" => Ok(MyNonterminal::Primary),
            "number" => Ok(MyNonterminal::Number),
            "whitespace" => Ok(MyNonterminal::Whitespace),
            _ => Err(format!("'{}' is not a valid MyNonterminal", my_nonterminal)),
        }
    }
}

fn main() {
    // @skip {
    //     root ::= (production | skip)+;
    //     skip ::= '@skip' nonterminal '{' production+ '}';
    //     production ::= nonterminal '::=' alternation ';';
    //     alternation ::= concatentation ('|' concatenation)*;
    //     concatenation ::= repetition*;
    //     repetition ::= unit repeatOperator?;
    //     unit ::= nonterminal | terminal | '(' alternation ')';
    // }
    // repeatoperator ::= [*+?] | '{' (number | range | upperbound | lowerbound) '}';
    // number ::= [0-9]+;
    // range ::= number ',' number;
    // upperbound ::= ',' number;
    // lowerbound ::= number ',';
    // nonterminal ::= [a-zA-Z_][a-zA-Z_0-9]*;
    // terminal ::= quotedstring | characterset | anycharacter | characterclass;
    // quotedstring ::= '"' ([^"\r\n\\] | '\\' .)* '"' | "'" ([^'\r\n\\] | '\\' .)* "'";
    // characterset ::= '[' ([^\]\r\n\\] | '\\' .)+ ']';
    // anycharacter ::= '.';
    // characterclass ::= '\\' [DSWdsw];
    // whitespace ::= [ \t\r\n];
    let definitions: HashMap<Nonterminal, Box<dyn Parsable<Nonterminal> + Sync>> = hashmap![
        Nonterminal::Root => plus!(
            alt!(
                cat!(
                    skp!(nt!(Nonterminal::Whitespace)), 
                    nt!(Nonterminal::Production), 
                    skp!(nt!(Nonterminal::Whitespace))
                ),
                cat!(
                    skp!(nt!(Nonterminal::Whitespace)), 
                    nt!(Nonterminal::SkipBlock),
                    skp!(nt!(Nonterminal::Whitespace))
                )
            )
        ),
        Nonterminal::SkipBlock => cat!(
            skp!(nt!(Nonterminal::Whitespace)), 
            lit!("@skip"),
            skp!(nt!(Nonterminal::Whitespace)), 
            nt!(Nonterminal::Nonterminal),
            skp!(nt!(Nonterminal::Whitespace)),
            lit!("{"),
            skp!(nt!(Nonterminal::Whitespace)),
            plus!(nt!(Nonterminal::Production)),
            skp!(nt!(Nonterminal::Whitespace)),
            lit!("}"),
            skp!(nt!(Nonterminal::Whitespace))
        ),
        Nonterminal::Production => cat!(
            skp!(nt!(Nonterminal::Whitespace)),
            nt!(Nonterminal::Nonterminal),
            skp!(nt!(Nonterminal::Whitespace)),
            lit!("::="),
            skp!(nt!(Nonterminal::Whitespace)),
            nt!(Nonterminal::Alternation),
            skp!(nt!(Nonterminal::Whitespace)),
            lit!(";"),
            skp!(nt!(Nonterminal::Whitespace))
        ),
        Nonterminal::Alternation => cat!(
            skp!(nt!(Nonterminal::Whitespace)),
            nt!(Nonterminal::Concatenation),
            skp!(nt!(Nonterminal::Whitespace)),
            star!(
                cat!(
                    skp!(nt!(Nonterminal::Whitespace)), 
                    lit!("|"),
                    skp!(nt!(Nonterminal::Whitespace)),
                    nt!(Nonterminal::Concatenation),
                    skp!(nt!(Nonterminal::Whitespace))
                )
            )
        ),
        Nonterminal::Concatenation => star!(
            cat!(
                skp!(nt!(Nonterminal::Whitespace)),
                nt!(Nonterminal::Repetition),
                skp!(nt!(Nonterminal::Whitespace))
            )
        ),
        Nonterminal::Repetition => cat!(
            skp!(nt!(Nonterminal::Whitespace)),
            nt!(Nonterminal::Unit),
            skp!(nt!(Nonterminal::Whitespace)),
            qm!(
                cat!(
                    skp!(nt!(Nonterminal::Whitespace)),
                    nt!(Nonterminal::RepeatOperator),
                    skp!(nt!(Nonterminal::Whitespace))
                )
            )
        ),
        Nonterminal::Unit => alt!(
            cat!(
                skp!(nt!(Nonterminal::Whitespace)),
                nt!(Nonterminal::Nonterminal),
                skp!(nt!(Nonterminal::Whitespace))
            ),
            cat!(
                skp!(nt!(Nonterminal::Whitespace)),
                nt!(Nonterminal::Terminal),
                skp!(nt!(Nonterminal::Whitespace))
            ),
            cat!(
                skp!(nt!(Nonterminal::Whitespace)),
                lit!("("),
                skp!(nt!(Nonterminal::Whitespace)),
                nt!(Nonterminal::Alternation),
                skp!(nt!(Nonterminal::Whitespace)),
                lit!(")"),
                skp!(nt!(Nonterminal::Whitespace))
            )
        ),
        Nonterminal::RepeatOperator => alt!(
            chc!("[*+?]"), 
            cat!(
                lit!("{"),
                alt!(
                    nt!(Nonterminal::Number),
                    nt!(Nonterminal::Range),
                    nt!(Nonterminal::UpperBound),
                    nt!(Nonterminal::LowerBound)
                ),
                lit!("}")
            )
        ),
        Nonterminal::Number => plus!(chc!("[0-9]")),
        Nonterminal::Range => cat!(
            nt!(Nonterminal::Number),
            lit!(","),
            nt!(Nonterminal::Number)
        ),
        Nonterminal::UpperBound => cat!(
            lit!(","),
            nt!(Nonterminal::Number)
        ),
        Nonterminal::LowerBound => cat!(
            nt!(Nonterminal::Number),
            lit!(",")
        ),
        Nonterminal::Nonterminal => cat!(
            chc!("[a-zA-Z_]"),
            star!(chc!("[a-zA-Z_0-9]"))
        ),
        Nonterminal::Terminal => alt!(
            nt!(Nonterminal::QuotedString),
            nt!(Nonterminal::CharacterSet),
            nt!(Nonterminal::AnyCharacter),
            nt!(Nonterminal::CharacterClass)
        ),
        Nonterminal::QuotedString => alt!(
            cat!(
                lit!("\""),
                star!(
                    alt!(
                        chc!("[^\"\\r\\n\\\\]"),
                        cat!(lit!("\\"), chc!("."))
                    )
                ),
                lit!("\"")
            ),
            cat!(
                lit!("'"),
                star!(
                    alt!(
                        chc!("[^'\\r\\n\\\\]"),
                        cat!(lit!("\\"), chc!("."))
                    )
                ),
                lit!("'")
            )
        ),
        Nonterminal::CharacterSet => cat!(
            lit!("["),
            plus!(
                alt!(
                    chc!("[^\\]\\r\\n\\\\]"),
                    cat!(lit!("\\"), chc!("."))
                )
            ),
            lit!("]")
        ),
        Nonterminal::AnyCharacter => lit!("."),
        Nonterminal::CharacterClass => cat!(
            lit!("\\"),
            chc!("[DSWdsw]")
        ),
        Nonterminal::Whitespace => chc!("[ \\t\\r\\n]")
    ];
    let grammar = r#"
        @skip whitespace {
            expression ::= sum;
            sum ::= primary ('+' primary)*;
            primary ::= number | '(' sum ')';
        }
        whitespace ::= [ \t\r\n]+;
        number ::= [0-9]+;
    "#;
    match nt!(Nonterminal::Root).parse(grammar, &definitions) {
        Ok(tree) => {
            println!("{:#?}", tree);
            let result: Result<Grammar<MyNonterminal>, String> = Grammar::new(&tree);
            match result {
                Ok(ast) => {
                    match ast.definitions() {
                        Ok(definitions) => {
                            match nt!(MyNonterminal::Expression).parse("(1+2)", &definitions) {
                                Ok(tree) => println!("{:#?}", tree),
                                Err(msg) => println!("{}", msg),
                            }
                        }
                        Err(msg) => println!("Err def {}", msg),
                    }
                },
                Err(msg) => println!("Err ast {}", msg),
            }
        }
        Err(msg) => println!("Err tree {}", msg),
    }
}
