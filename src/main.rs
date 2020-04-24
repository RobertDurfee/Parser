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
    SumDifference,
    ProductQuotient,
    SumOperator,
    DifferenceOperator,
    ProductOperator,
    QuotientOperator,
    Primary,
    Number,
    Whitespace,
}

fn main() -> Result<(), String> {
    let grammar = r#"
        @skip Whitespace {
            Expression ::= SumDifference;
            SumDifference ::= ProductQuotient ((SumOperator | DifferenceOperator) ProductQuotient)*;
            ProductQuotient ::= Primary ((ProductOperator | QuotientOperator) Primary)*;
            Primary ::= Number | '(' Expression ')';
        }
        SumOperator ::= '+';
        DifferenceOperator ::= '-';
        ProductOperator ::= '*';
        QuotientOperator ::= '/';
        Number ::= [+-]? (([0-9]+ ('.' [0-9]*)?) | ('.' [0-9]+));
        Whitespace ::= [ \t\r\n];
    "#;
    let expression = r#"
        1/(2+-0.3)
    "#;
    println!("{:#?}", parser::parse(expression, &grammar::definitions(grammar)?, Nonterminal::Expression)?);
    return Ok(());
}
