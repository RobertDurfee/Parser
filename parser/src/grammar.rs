use std::collections::BTreeMap as Map;
use lazy_static::lazy_static;
use interval_map;
use re_bootstrap::{
    sym as rsym,
    con as rcon,
    ast as rast,
    plu as rplu,
    sgl as rsgl,
    rng as rrng,
    Re,
};
use parser_bootstrap::{
    error::Result,
    ParseTree,
    Expression,
    tok as ptok,
    non as pnon,
    alt as palt,
    con as pcon,
    ast as past,
    plu as pplu,
    que as pque,
    map,
};

#[allow(non_camel_case_types)]
#[derive(Eq, Ord, PartialEq, PartialOrd)]
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

#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub enum Nonterminal {
    Grammar,
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

pub enum Grammar<N, T> {
    PhantomN(N),
    PhantomT(T),
}

impl<N, T> Grammar<N, T> {
    pub fn new(_parse_tree: ParseTree<Nonterminal, TokenKind>) -> Result<Grammar<N, T>> {
        panic!("Not implemented")
    }

    pub fn productions(&self) -> Result<Map<N, Expression<N, T>>> {
        panic!("Not implemented")
    }

    fn nonterminal(&self) -> Result<N> {
        panic!("Not implemented")
    }

    fn expression(&self) -> Result<Expression<N, T>> {
        panic!("Not implemented")
    }
}

lazy_static! {
    // "[A-Z][0-9a-zA-Z]*" => NONTERMINAL;
    // "::=" => PRODUCTION_OPERATOR;
    // ";" => SEMICOLON;
    // "\|" => VERTICAL_BAR;
    // "\*" => ASTERISK;
    // "\+" => PLUS_SIGN;
    // "\?" => QUESTION_MARK;
    // "[A-Z][0-9A-Z_]*" => TOKEN_KIND;
    // "\(" => LEFT_PARENTHESIS;
    // "\)" => RIGHT_PARENTHESIS;
    // "\{" => LEFT_CURLY_BRACKET;
    // "\}" => RIGHT_CURLY_BRACKET;
    // "[0-9]+" => INTEGER;
    // "," => COMMA;
    static ref LEXER_PRODUCTIONS: Map<Re, Option<TokenKind>> = map![
        rcon![
            rsym![rrng!('A', 'Z')], 
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
        rsym![rsgl!(',')] => Some(COMMA)
    ];

    // Grammar ::= Production*;
    // Production ::= NONTERMINAL PRODUCTION_OPERATOR Alternation SEMICOLON;
    // Alternation ::= Concatenation (VERTICAL_BAR Concatenation)*;
    // Concatenation ::= Repetition+;
    // Repetition ::= Atom (ASTERISK | PLUS_SIGN | QUESTION_MARK | Exact | Minimum | Maximum | Range)?;
    // Atom ::= NONTERMINAL | TOKEN_KIND | LEFT_PARENTHESIS Alternation RIGHT_PARENTHESIS;
    // Exact ::= LEFT_CURLY_BRACKET INTEGER RIGHT_CURLY_BRACKET;
    // Minimum ::= LEFT_CURLY_BRACKET INTEGER COMMA RIGHT_CURLY_BRACKET;
    // Maximum ::= LEFT_CURLY_BRACKET COMMA INTEGER RIGHT_CURLY_BRACKET;
    // Range ::= LEFT_CURLY_BRACKET INTEGER COMMA INTEGER RIGHT_CURLY_BRACKET:
    static ref PARSER_PRODUCTIONS: Map<Nonterminal, Expression<Nonterminal, TokenKind>> = map![
        Grammar => past!(pnon!(Production)),
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
