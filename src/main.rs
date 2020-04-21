#[macro_use]
extern crate lazy_static;
extern crate enum_utils;

mod parser;
mod util;
mod character_class;
mod grammar;

use std::hash::Hash;

#[derive(Clone, Debug, Hash, PartialEq, Eq, enum_utils::FromStr)]
enum Nonterminal {
    Expression,
    GlueRight,
    LowerOverlay,
    UpperOverlay,
    Resize,
    Primitive,
    GlueBelowOperator,
    Filename,
    CaptionSurround,
    Caption,
    Number,
    Question,
    Whitespace,
}

fn main() -> Result<(), String> {
    let grammar = r#"
        @skip Whitespace {
            Expression ::= GlueRight (GlueBelowOperator GlueRight)*;
            GlueRight ::= LowerOverlay ('|' LowerOverlay)*;
            LowerOverlay ::= UpperOverlay ('_' UpperOverlay)*;
            UpperOverlay ::= Resize ('^' Resize)*;
            Resize ::= Primitive ('@' (Number | Question) 'x' (Number | Question))*;
            Primitive ::= Filename | CaptionSurround | '(' Expression ')';
        }
        GlueBelowOperator ::= '---' '-'*;
        Filename ::= [A-Za-z0-9./][A-Za-z0-9./_-]*;
        CaptionSurround ::= '"' Caption '"';
        Caption ::= [^\n"]*;
        Number ::= [0-9]+;
        Question ::= '?';
        Whitespace ::= [ \t\r\n]+;
    "#;
    let expression = r#"
        (filename1.png | filename2.png)@?x200
    "#;
    println!("{:#?}", parser::parse(expression, &grammar::definitions(grammar)?, Nonterminal::Expression)?);
    return Ok(());
}
