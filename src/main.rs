#[macro_use]
extern crate lazy_static;

mod parser;
mod util;
mod character_class;
mod grammar;

use std::hash::Hash;
use std::str::FromStr;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

impl FromStr for Nonterminal {
    type Err = String;

    fn from_str(my_nonterminal: &str) -> Result<Self, Self::Err> {
        match my_nonterminal {
            "expression" => Ok(Nonterminal::Expression),
            "glueright" => Ok(Nonterminal::GlueRight),
            "loweroverlay" => Ok(Nonterminal::LowerOverlay),
            "upperoverlay" => Ok(Nonterminal::UpperOverlay),
            "resize" => Ok(Nonterminal::Resize),
            "primitive" => Ok(Nonterminal::Primitive),
            "gluebelowoperator" => Ok(Nonterminal::GlueBelowOperator),
            "filename" => Ok(Nonterminal::Filename),
            "captionsurround" => Ok(Nonterminal::CaptionSurround),
            "caption" => Ok(Nonterminal::Caption),
            "number" => Ok(Nonterminal::Number),
            "question" => Ok(Nonterminal::Question),
            "whitespace" => Ok(Nonterminal::Whitespace),
            any => Err(format!("'{:?}' is not a valid Nonterminal", any)),
        }
    }
}

fn main() -> Result<(), String> {
    let grammar = r#"
        @skip whitespace {
            expression ::= glueright (gluebelowoperator glueright)*;
            glueright ::= loweroverlay ('|' loweroverlay)*;
            loweroverlay ::= upperoverlay ('_' upperoverlay)*;
            upperoverlay ::= resize ('^' resize)*;
            resize ::= primitive ('@' (number | question) 'x' (number | question))*;
            primitive ::= filename | captionsurround | '(' expression ')';
        }
        gluebelowoperator ::= '---' '-'*;
        filename ::= [A-Za-z0-9./][A-Za-z0-9./_-]*;
        captionsurround ::= '"' caption '"';
        caption ::= [^\n"]*;
        number ::= [0-9]+;
        question ::= '?';
        whitespace ::= [ \t\r\n]+;
    "#;
    println!("{:#?}", parser::parse("filename1.png ------ \"this is a test\" @?x34", &grammar::definitions(grammar)?, Nonterminal::Expression)?);
    return Ok(());
}
