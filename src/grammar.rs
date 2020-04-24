use std::collections::HashMap;
use std::str::FromStr;
use std::hash::Hash;
use std::fmt::Debug;
use crate::parser;
use crate::{hashmap, cat, lit, star, plus, nt, alt, chc, skp, qm, rep};

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

impl<T: Clone + Debug + FromStr + Hash + PartialEq + Eq + Sync + 'static> Grammar<T> {
    fn new(tree: &parser::Tree<Nonterminal>) -> Result<Grammar<T>, String> {
        match tree.name {
            Some(Nonterminal::Root) => {
                let mut grammars = Vec::new();
                for child in &tree.children {
                    grammars.push(Box::new(Grammar::new(&child)?));
                }
                return Ok(Grammar::Root(grammars));
            },
            Some(Nonterminal::SkipBlock) => {
                match T::from_str(&tree.children.get(0).unwrap().contents) {
                    Ok(nonterminal) => {
                        let mut grammars = Vec::new();
                        for child in &tree.children[1..] {
                            grammars.push(Box::new(Grammar::new(&child)?));
                        }
                        return Ok(Grammar::SkipBlock(nonterminal, grammars));
                    },
                    Err(_) => return Err(format!("Unable to parse '{:?}'", tree.children.get(0).unwrap()))
                }
            },
            Some(Nonterminal::Production) => {
                match T::from_str(&tree.children.get(0).unwrap().contents) {
                    Ok(nonterminal) => return Ok(Grammar::Production(nonterminal, Box::new(Grammar::new(tree.children.get(1).unwrap())?))),
                    Err(_) => return Err(format!("Unable to parse '{:?}'", tree.children.get(0).unwrap()))
                }
            },
            Some(Nonterminal::Alternation) => {
                if tree.children.len() > 1 {
                    let mut grammars = Vec::new();
                    for child in &tree.children {
                        grammars.push(Box::new(Grammar::new(&child)?));
                    }
                    return Ok(Grammar::Alternation(grammars));
                } else {
                    return Grammar::new(tree.children.get(0).unwrap());
                }
            },
            Some(Nonterminal::Concatenation) => {
                if tree.children.len() > 1 {
                    let mut grammars = Vec::new();
                    for child in &tree.children {
                        grammars.push(Box::new(Grammar::new(&child)?));
                    }
                    return Ok(Grammar::Concatenation(grammars));
                } else {
                    return Grammar::new(tree.children.get(0).unwrap());
                }
            },
            Some(Nonterminal::Repetition) => {
                let grammar = Grammar::new(tree.children.get(0).unwrap())?;
                if let Some(operator) = tree.children.get(1) {
                    if let Some(bound) = operator.children.get(0) {
                        match bound.name {
                            Some(Nonterminal::Number) => {
                                let bound = bound.contents.parse::<u32>().unwrap();
                                return Ok(Grammar::Repetition(Box::new(grammar), Some(bound), Some(bound)));
                            },
                            Some(Nonterminal::Range) => {
                                let lower_bound = bound.children.get(0).unwrap().contents.parse::<u32>().unwrap();
                                let upper_bound = bound.children.get(1).unwrap().contents.parse::<u32>().unwrap();
                                return Ok(Grammar::Repetition(Box::new(grammar), Some(lower_bound), Some(upper_bound)));
                            },
                            Some(Nonterminal::LowerBound) => {
                                let lower_bound = bound.children.get(0).unwrap().contents.parse::<u32>().unwrap();
                                return Ok(Grammar::Repetition(Box::new(grammar), Some(lower_bound), None));
                            },
                            Some(Nonterminal::UpperBound) => {
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
                            any => return Err(format!("Invalid repetition operator '{:?}'", any)),
                        }
                    }
                } else {
                    return Ok(grammar);
                }
            },
            Some(Nonterminal::Unit) => Grammar::new(tree.children.get(0).unwrap()),
            Some(Nonterminal::Nonterminal) => {
                match T::from_str(&tree.contents) {
                    Ok(nonterminal) => Ok(Grammar::Nonterminal(nonterminal)),
                    Err(_) => Err(format!("Unable to parse '{:?}'", tree.contents)),
                }
            },
            Some(Nonterminal::Terminal) => Grammar::new(tree.children.get(0).unwrap()),
            Some(Nonterminal::QuotedString) => Ok(Grammar::Literal(String::from(&tree.contents[1..tree.contents.len()-1]))),
            Some(Nonterminal::CharacterSet) => Ok(Grammar::CharacterClass(tree.contents.clone())),
            Some(Nonterminal::AnyCharacter) => Ok(Grammar::CharacterClass(tree.contents.clone())),
            Some(Nonterminal::CharacterClass) => Ok(Grammar::CharacterClass(tree.contents.clone())),
            ref any => Err(format!("'{:?}' is not valid nonterminal", any)),
        }
    }

    fn definitions(&self) -> Result<HashMap<T, Box<parser::Term<T>>>, String> {
        match *self {
            Grammar::Root(ref grammars) => {
                let mut result = HashMap::new();
                for grammar in grammars {
                    result.extend(grammar.definitions()?);
                }
                return Ok(result);
            },
            Grammar::SkipBlock(ref nonterminal, ref grammars) => {
                let mut result = HashMap::new();
                for grammar in grammars {
                    result.extend(grammar.skip(nonterminal).definitions()?);
                }
                return Ok(result);
            },
            Grammar::Production(ref nonterminal, ref grammar) => Ok(hashmap![nonterminal.clone() => grammar.definition()?]),
            _ => Err(format!("Cannot get definitions for '{:?}'", *self)),
        }
    }

    fn definition(&self) -> Result<Box<parser::Term<T>>, String> {
        match *self {
            Grammar::Alternation(ref grammars) => {
                let mut definitions = Vec::new();
                for grammar in grammars {
                    definitions.push(grammar.definition()?);
                }
                return Ok(Box::new(parser::Term::Alternation(definitions)));
            },
            Grammar::Concatenation(ref grammars) => {
                let mut definitions = Vec::new();
                for grammar in grammars {
                    definitions.push(grammar.definition()?);
                }
                return Ok(Box::new(parser::Term::Concatenation(definitions)));
            },
            Grammar::Repetition(ref grammar, ref min, ref max) => Ok(rep!(grammar.definition()?, *min, *max)),
            Grammar::Skip(ref grammar) => Ok(skp!(grammar.definition()?)),
            Grammar::Nonterminal(ref nonterminal) => Ok(nt!(nonterminal.clone())),
            Grammar::Literal(ref literal) => Ok(lit!(literal)),
            Grammar::CharacterClass(ref character_class) => Ok(chc!(&character_class)),
            _ => Err(format!("Cannot get definition for {:?}", *self)),
        }
    }

    fn skip(&self, skip_nonterminal: &T) -> Grammar<T> {
        match *self {
            Grammar::Root(ref grammars) => {
                let mut skipped = Vec::new();
                for grammar in grammars {
                    skipped.push(Box::new(grammar.skip(skip_nonterminal)));
                }
                return Grammar::Root(skipped);
            },
            Grammar::SkipBlock(ref nonterminal, ref grammars) => {
                let mut skipped = Vec::new();
                for grammar in grammars {
                    skipped.push(Box::new(grammar.skip(skip_nonterminal)));
                }
                return Grammar::SkipBlock(nonterminal.clone(), skipped);
            },
            Grammar::Production(ref nonterminal, ref grammar) => return Grammar::Production(nonterminal.clone(), Box::new(grammar.skip(skip_nonterminal))),
            Grammar::Alternation(ref grammars) => {
                let mut skipped = Vec::new();
                for grammar in grammars {
                    skipped.push(Box::new(grammar.skip(skip_nonterminal)));
                }
                return Grammar::Alternation(skipped);
            },
            Grammar::Concatenation(ref grammars) => {
                let mut skipped = Vec::new();
                for grammar in grammars {
                    skipped.push(Box::new(grammar.skip(skip_nonterminal)));
                }
                return Grammar::Concatenation(skipped);
            },
            Grammar::Repetition(ref grammar, ref min, ref max) => return Grammar::Repetition(Box::new(grammar.skip(skip_nonterminal)), min.clone(), max.clone()),
            Grammar::Skip(ref grammar) => return Grammar::Skip(Box::new(grammar.skip(skip_nonterminal))),
            Grammar::Nonterminal(_) | Grammar::Literal(_) | Grammar::CharacterClass(_) => {
                return Grammar::Concatenation(vec![
                    Box::new(Grammar::Skip(Box::new(Grammar::Nonterminal(skip_nonterminal.clone())))), 
                    Box::new(self.clone()),
                    Box::new(Grammar::Skip(Box::new(Grammar::Nonterminal(skip_nonterminal.clone()))))])
            },
        }
    }
}

lazy_static! {
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
    static ref DEFINITIONS: HashMap<Nonterminal, Box<parser::Term<Nonterminal>>> = hashmap![
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
}

pub fn definitions<T: Clone + Debug + Eq + FromStr + Hash + Sync + 'static>(input: &str) -> Result<HashMap<T, Box<parser::Term<T>>>, String> {
    return Grammar::new(&parser::parse(input, &DEFINITIONS, Nonterminal::Root)?)?.definitions();
}
