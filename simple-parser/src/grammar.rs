use std::{
    collections::BTreeMap as Map,
    str::FromStr,
};
use lazy_static::lazy_static;
use segment_map;
use regular_expression_bootstrap::{
    sym as rsym,
    neg as rneg,
    con as rcon,
    ast as rast,
    plu as rplu,
    sgl as rsgl,
    rng as rrng,
};
use crate::{
    con as pcon,
    alt as palt,
    ast as past,
    plu as pplu,
    non as pnon,
    tok as ptok,
    que as pque,
    ParseTree,
    Expression,
};

type Result<T> = std::result::Result<T, &'static str>;

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub enum TokenKind {
    NONTERMINAL,
    PRODUCTION_OPERATOR,
    SEMICOLON,
    VERTICAL_BAR,
    ASTERISK,
    PLUS_SIGN,
    QUESTION_MARK,
    TOKEN_KIND,
    LEFT_PARENTHESIS,
    RIGHT_PARENTHESIS,
    LEFT_CURLY_BRACKET,
    RIGHT_CURLY_BRACKET,
    INTEGER,
    COMMA,
}
use TokenKind::*;

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub enum Nonterminal {
    Root,
    Production,
    Alternation,
    Concatenation,
    Repetition,
    Atom,
    Exact,
    Minimum,
    Maximum,
    Range,
}
use Nonterminal::*;

pub fn as_productions<N: FromStr + Ord, T: FromStr>(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> Result<Map<N, Expression<N, T>>> {
    if let ParseTree::Nonterminal { nonterminal, children, .. } = parse_tree {
        match nonterminal {
            // Root ::= Production*;
            Root => {
                let mut productions = Map::new();
                for child in children {
                    productions.extend(as_productions(child)?);
                }
                Ok(productions)
            },
            // Production ::= NONTERMINAL PRODUCTION_OPERATOR Alternation SEMICOLON;
            Production => {
                Ok(map![as_nonterminal(&children[0])? => as_expression(&children[2])?])
            },
            _ => Err("no productions")
        }
    } else { Err("no productions") }
}

fn as_nonterminal<N: FromStr>(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> Result<N> {
    if let ParseTree::Token { token } = parse_tree {
        // /[A-Z][0-9A-Z]*[a-z][0-9a-zA-Z]*/ => NONTERMINAL;
        if let NONTERMINAL = token.kind() {
            if let Ok(nonterminal) = N::from_str(token.text()) {
                Ok(nonterminal)
            } else { Err("not nonterminal") }
        } else { Err("not nonterminal") }
    } else { Err("not nonterminal") }
}

fn as_expression<N: FromStr, T: FromStr>(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> Result<Expression<N, T>> {
    match parse_tree {
        ParseTree::Nonterminal { nonterminal, children, .. } => {
            match nonterminal {
                // Alternation ::= Concatenation (VERTICAL_BAR Concatenation)*;
                Alternation => {
                    if children.len() > 1 {
                        let mut expressions = Vec::new();
                        let mut skip = false;
                        for child in children {
                            if !skip {
                                expressions.push(as_expression(child)?);
                            }
                            skip = !skip;
                        }
                        Ok(Expression::Alternation { expressions })
                    } else {
                        Ok(as_expression(&children[0])?)
                    }
                },
                // Concatenation ::= Repetition+;
                Concatenation => {
                    if children.len() > 1 {
                        let mut expressions = Vec::new();
                        for child in children {
                            expressions.push(as_expression(child)?);
                        }
                        Ok(Expression::Concatenation { expressions })
                    } else {
                        Ok(as_expression(&children[0])?)
                    }
                },
                // Repetition ::= Atom (ASTERISK | PLUS_SIGN | QUESTION_MARK | Exact | Minimum | Maximum | Range)?;
                Repetition => {
                    if children.len() > 1 {
                        let expression = Box::new(as_expression(&children[0])?);
                        let (min, max) = as_range(&children[1])?;
                        Ok(Expression::Repetition { expression, min, max })
                    } else {
                        Ok(as_expression(&children[0])?)
                    }
                },
                // Atom ::= NONTERMINAL | TOKEN_KIND | LEFT_PARENTHESIS Alternation RIGHT_PARENTHESIS;
                Atom => {
                    if children.len() > 1 {
                        Ok(as_expression(&children[1])?)
                    } else {
                        Ok(as_expression(&children[0])?)
                    }
                },
                _ => Err("not expression")
            }
        },
        ParseTree::Token { token } => {
            match token.kind() {
                // /[A-Z][0-9A-Z]*[a-z][0-9a-zA-Z]*/ => NONTERMINAL;
                NONTERMINAL => {
                    if let Ok(nonterminal) = N::from_str(token.text()) {
                        Ok(Expression::Nonterminal { nonterminal })
                    } else { Err("not expression") }
                },
                // /[A-Z][0-9A-Z_]*/ => TOKEN_KIND;
                TOKEN_KIND => {
                    if let Ok(token_kind) = T::from_str(token.text()) {
                        Ok(Expression::TokenKind { token_kind })
                    } else { Err("not expression") }
                },
                _ => Err("not expression")
            }
        }
        _ => Err("not expression")
    }
}

fn as_range(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> Result<(Option<u32>, Option<u32>)> {
    match parse_tree {
        ParseTree::Nonterminal { nonterminal, children, .. } => {
            match nonterminal {
                // Exact ::= LEFT_CURLY_BRACKET INTEGER RIGHT_CURLY_BRACKET;
                Exact => {
                    Ok((Some(as_integer(&children[1])?), Some(as_integer(&children[1])?)))
                },
                // Minimum ::= LEFT_CURLY_BRACKET INTEGER COMMA RIGHT_CURLY_BRACKET;
                Minimum => {
                    Ok((Some(as_integer(&children[1])?), None))
                },
                // Maximum ::= LEFT_CURLY_BRACKET COMMA INTEGER RIGHT_CURLY_BRACKET;
                Maximum => {
                    Ok((None, Some(as_integer(&children[2])?)))
                },
                // Range ::= LEFT_CURLY_BRACKET INTEGER COMMA INTEGER RIGHT_CURLY_BRACKET:
                Range => {
                    Ok((Some(as_integer(&children[1])?), Some(as_integer(&children[3])?)))
                },
                _ => Err("not range")
            }
        },
        ParseTree::Token { token } => {
            match token.kind() {
                // /\*/ => ASTERISK;
                ASTERISK => {
                    Ok((None, None))
                },
                // /\+/ => PLUS_SIGN;
                PLUS_SIGN => {
                    Ok((Some(1), None))
                },
                // /\?/ => QUESTION_MARK;
                QUESTION_MARK => {
                    Ok((None, Some(1)))
                },
                _ => Err("not range")
            }
        }
        _ => Err("not range")
    }
}

fn as_integer(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> Result<u32> {
    if let ParseTree::Token { token } = parse_tree {
        // /[0-9]+/ => INTEGER;
        if let INTEGER = token.kind() {
            if let Ok(integer) = token.text().parse::<u32>() {
                Ok(integer)
            } else { Err("not integer") }
        } else { Err("not integer") }
    } else { Err("not integer") }
}

lazy_static! {
    // /[A-Z][0-9A-Z]*[a-z][0-9a-zA-Z]*/ => NONTERMINAL;
    // /::=/ => PRODUCTION_OPERATOR;
    // /;/ => SEMICOLON;
    // /\|/ => VERTICAL_BAR;
    // /\*/ => ASTERISK;
    // /\+/ => PLUS_SIGN;
    // /\?/ => QUESTION_MARK;
    // /[A-Z][0-9A-Z_]*/ => TOKEN_KIND;
    // /\(/ => LEFT_PARENTHESIS;
    // /\)/ => RIGHT_PARENTHESIS;
    // /\{/ => LEFT_CURLY_BRACKET;
    // /\}/ => RIGHT_CURLY_BRACKET;
    // /[0-9]+/ => INTEGER;
    // /,/ => COMMA;
    // /[\n\r\t ]/ => ;
    // /\/\/[^\n\r]*/ => ;
    pub(crate) static ref LEXER_PRODUCTIONS: Map<regular_expression_bootstrap::Expression, Option<TokenKind>> = map![
        rcon![
            rsym![rrng!('A', 'Z')], 
            rast!(rsym![
                rrng!('0', '9'),
                rrng!('A', 'Z')
            ]),
            rsym![rrng!('a', 'z')],
            rast!(rsym![
                rrng!('0', '9'),
                rrng!('a', 'z'),
                rrng!('A', 'Z')
            ])
        ] => Some(NONTERMINAL),
        rcon![
            rsym![rsgl!(':')],
            rsym![rsgl!(':')],
            rsym![rsgl!('=')]
        ] => Some(PRODUCTION_OPERATOR),
        rsym![rsgl!(';')] => Some(SEMICOLON),
        rsym![rsgl!('|')] => Some(VERTICAL_BAR),
        rsym![rsgl!('*')] => Some(ASTERISK),
        rsym![rsgl!('+')] => Some(PLUS_SIGN),
        rsym![rsgl!('?')] => Some(QUESTION_MARK),
        rcon![
            rsym![rrng!('A', 'Z')], 
            rast!(rsym![
                rrng!('0', '9'),
                rrng!('A', 'Z'),
                rsgl!('_')
            ])
        ] => Some(TOKEN_KIND),
        rsym![rsgl!('(')] => Some(LEFT_PARENTHESIS),
        rsym![rsgl!(')')] => Some(RIGHT_PARENTHESIS),
        rsym![rsgl!('{')] => Some(LEFT_CURLY_BRACKET),
        rsym![rsgl!('}')] => Some(RIGHT_CURLY_BRACKET),
        rplu!(rsym![rrng!('0', '9')]) => Some(INTEGER),
        rsym![rsgl!(',')] => Some(COMMA),
        rsym![
            rsgl!('\n'),
            rsgl!('\r'),
            rsgl!('\t'),
            rsgl!(' ')
        ] => None,
        rcon![
            rsym![rsgl!('/')],
            rsym![rsgl!('/')],
            rast!(rneg![
                rsgl!('\n'),
                rsgl!('\r')
            ])
        ] => None
    ];

    // Root ::= Production*;
    // Production ::= NONTERMINAL PRODUCTION_OPERATOR Alternation SEMICOLON;
    // Alternation ::= Concatenation (VERTICAL_BAR Concatenation)*;
    // Concatenation ::= Repetition+;
    // Repetition ::= Atom (ASTERISK | PLUS_SIGN | QUESTION_MARK | Exact | Minimum | Maximum | Range)?;
    // Atom ::= NONTERMINAL | TOKEN_KIND | LEFT_PARENTHESIS Alternation RIGHT_PARENTHESIS;
    // Exact ::= LEFT_CURLY_BRACKET INTEGER RIGHT_CURLY_BRACKET;
    // Minimum ::= LEFT_CURLY_BRACKET INTEGER COMMA RIGHT_CURLY_BRACKET;
    // Maximum ::= LEFT_CURLY_BRACKET COMMA INTEGER RIGHT_CURLY_BRACKET;
    // Range ::= LEFT_CURLY_BRACKET INTEGER COMMA INTEGER RIGHT_CURLY_BRACKET:
    pub(crate) static ref PARSER_PRODUCTIONS: Map<Nonterminal, Expression<Nonterminal, TokenKind>> = map![
        Root => past!(pnon!(Production)),
        Production => pcon![
            ptok!(NONTERMINAL),
            ptok!(PRODUCTION_OPERATOR),
            pnon!(Alternation),
            ptok!(SEMICOLON)
        ],
        Alternation => pcon![
            pnon!(Concatenation), 
            past!(pcon![
                ptok!(VERTICAL_BAR),
                pnon!(Concatenation)
            ])
        ],
        Concatenation => pplu!(pnon!(Repetition)),
        Repetition => pcon![
            pnon!(Atom), pque!(palt![
                ptok!(ASTERISK),
                ptok!(PLUS_SIGN),
                ptok!(QUESTION_MARK),
                pnon!(Exact),
                pnon!(Minimum),
                pnon!(Maximum),
                pnon!(Range)
            ])
        ],
        Atom => palt![
            ptok!(NONTERMINAL),
            ptok!(TOKEN_KIND),
            pcon![
                ptok!(LEFT_PARENTHESIS),
                pnon!(Alternation),
                ptok!(RIGHT_PARENTHESIS)
            ]
        ],
        Exact => pcon![
            ptok!(LEFT_CURLY_BRACKET),
            ptok!(INTEGER),
            ptok!(RIGHT_CURLY_BRACKET)
        ],
        Minimum => pcon![
            ptok!(LEFT_CURLY_BRACKET),
            ptok!(INTEGER),
            ptok!(COMMA),
            ptok!(RIGHT_CURLY_BRACKET)
        ],
        Maximum => pcon![
            ptok!(LEFT_CURLY_BRACKET),
            ptok!(COMMA),
            ptok!(INTEGER),
            ptok!(RIGHT_CURLY_BRACKET)
        ],
        Range => pcon![
            ptok!(LEFT_CURLY_BRACKET),
            ptok!(INTEGER),
            ptok!(COMMA),
            ptok!(INTEGER),
            ptok!(RIGHT_CURLY_BRACKET)
        ]
    ];
}
