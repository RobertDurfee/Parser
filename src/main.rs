#[macro_use]
extern crate lazy_static;

mod parser;
mod util;
mod char_class;

use std::collections::HashMap;
use crate::parser::Parsable;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Nonterminal {
    Root,
    Skip,
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
                    nt!(Nonterminal::Skip),
                    skp!(nt!(Nonterminal::Whitespace))
                )
            )
        ),
        Nonterminal::Skip => cat!(
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
        Ok(tree) => println!("{:#?}", tree),
        Err(msg) => println!("{}", msg),
    }
}
