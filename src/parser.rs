use std::collections::BTreeMap as Map;
use lexer::Token;
use crate::error::{
    Error,
    ErrorKind,
    Result,
};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum ParseTree<N, T> {
    Token { token: Token<T> },
    Nonterminal { nonterminal: N, tokens: Vec<Token<T>>, children: Vec<ParseTree<N, T>> },
    Ephemeral { tokens: Vec<Token<T>>, children: Vec<ParseTree<N, T>> },
}

impl<N, T> ParseTree<N, T> {
    pub fn tokens_len(&self) -> usize {
        match self {
            ParseTree::Token { .. } => 1,
            ParseTree::Nonterminal { tokens, .. } => tokens.len(),
            ParseTree::Ephemeral { tokens, .. } => tokens.len(),
        }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Expression<N, T> {
    TokenKind { token_kind: T },
    Nonterminal { nonterminal: N },
    Alternation { expressions: Vec<Expression<N, T>> },
    Concatenation { expressions: Vec<Expression<N, T>> },
    Repetition { expression: Box<Expression<N, T>>, min: Option<u32>, max: Option<u32> },
}

impl<N: Clone + Ord, T: Clone + PartialEq> Expression<N, T> {
    pub fn parse(&self, tokens: &[Token<T>], productions: &Map<N, Expression<N, T>>) -> Result<ParseTree<N, T>> {
        match self {
            Expression::TokenKind { token_kind } => {
                if let Some(current_token) = tokens.get(0) {
                    if token_kind == current_token.kind() {
                        Ok(ParseTree::Token {
                            token: current_token.clone(),
                        })
                    } else { Err(Error::from(ErrorKind::UnexpectedToken)) }
                } else { Err(Error::from(ErrorKind::UnexpectedEOF)) }
            },
            Expression::Nonterminal { nonterminal } => {
                if let Some(expression) = productions.get(nonterminal) {
                    let parse_tree = expression.parse(tokens, productions)?;
                    match parse_tree {
                        ParseTree::Token { token } => {
                            Ok(ParseTree::Nonterminal {
                                nonterminal: nonterminal.clone(),
                                tokens: vec![token.clone()],
                                children: vec![ParseTree::Token { token }],
                            })
                        },
                        ParseTree::Nonterminal { nonterminal: child_nonterminal, tokens, children } => {
                            Ok(ParseTree::Nonterminal {
                                nonterminal: nonterminal.clone(),
                                tokens: tokens.clone(),
                                children: vec![ParseTree::Nonterminal {
                                    nonterminal: child_nonterminal,
                                    tokens,
                                    children,
                                }],
                            })
                        },
                        ParseTree::Ephemeral { tokens, children } => {
                            Ok(ParseTree::Nonterminal {
                                nonterminal: nonterminal.clone(),
                                tokens,
                                children,
                            })
                        },
                    }
                } else { Err(Error::from(ErrorKind::UndefinedNonterminal)) }
            },
            Expression::Alternation { expressions } => {
                let mut parse_trees = Vec::new();
                for expression in expressions {
                    if let Ok(parse_tree) = expression.parse(tokens, productions) {
                        parse_trees.push(parse_tree);
                    }
                }
                if let Some(mut parse_tree) = parse_trees.pop() {
                    while let Some(current_parse_tree) = parse_trees.pop() {
                        if parse_tree.tokens_len() < current_parse_tree.tokens_len() {
                            parse_tree = current_parse_tree;
                        }
                    }
                    match parse_tree {
                        ParseTree::Token { token } => {
                            Ok(ParseTree::Ephemeral {
                                tokens: vec![token.clone()],
                                children: vec![ParseTree::Token { token }],
                            })
                        },
                        ParseTree::Nonterminal { nonterminal, tokens, children } => {
                            Ok(ParseTree::Ephemeral {
                                tokens: tokens.clone(),
                                children: vec![ParseTree::Nonterminal {
                                    nonterminal,
                                    tokens,
                                    children,
                                }],
                            })
                        },
                        ParseTree::Ephemeral { tokens, children } => {
                            Ok(ParseTree::Ephemeral {
                                tokens,
                                children,
                            })
                        },
                    }
                } else { Err(Error::from(ErrorKind::NoMatch)) }
            },
            Expression::Concatenation { expressions } => {
                let mut offset = 0;
                let mut matched_tokens = Vec::new();
                let mut children = Vec::new();
                for expression in expressions {
                    if let Ok(parse_tree) = expression.parse(&tokens[offset..], productions) {
                        offset += parse_tree.tokens_len();
                        match parse_tree {
                            ParseTree::Token { token } => {
                                matched_tokens.push(token.clone());
                                children.push(ParseTree::Token { token });
                            },
                            ParseTree::Nonterminal { nonterminal, tokens, children: child_children } => {
                                matched_tokens.extend(tokens.clone());
                                children.push(ParseTree::Nonterminal {
                                    nonterminal,
                                    tokens,
                                    children: child_children,
                                });
                            },
                            ParseTree::Ephemeral { tokens, children: child_children } => {
                                matched_tokens.extend(tokens);
                                children.extend(child_children);
                            },
                        }
                    } else { return Err(Error::from(ErrorKind::NoMatch)); }
                }
                Ok(ParseTree::Ephemeral {
                    tokens: matched_tokens,
                    children,
                })
            },
            Expression::Repetition { expression, min, max } => {
                let mut count = 0;
                let mut offset = 0;
                let mut matched_tokens = Vec::new();
                let mut children = Vec::new();
                while match max { Some(max) => count < *max, None => true } {
                    if let Ok(parse_tree) = expression.parse(&tokens[offset..], productions) {
                        count += 1;
                        offset += parse_tree.tokens_len();
                        match parse_tree {
                            ParseTree::Token { token } => {
                                matched_tokens.push(token.clone());
                                children.push(ParseTree::Token { token });
                            },
                            ParseTree::Nonterminal { nonterminal, tokens, children: child_children } => {
                                matched_tokens.extend(tokens.clone());
                                children.push(ParseTree::Nonterminal {
                                    nonterminal,
                                    tokens,
                                    children: child_children,
                                });
                            },
                            ParseTree::Ephemeral { tokens, children: child_children } => {
                                matched_tokens.extend(tokens);
                                children.extend(child_children);
                            },
                        }
                    } else if match min { Some(min) => count >= *min, None => true } {
                        return Ok(ParseTree::Ephemeral {
                            tokens: matched_tokens,
                            children,
                        });
                    } else { return Err(Error::from(ErrorKind::PartialMatch)); }
                }
                Ok(ParseTree::Ephemeral {
                    tokens: matched_tokens,
                    children,
                })
            },
        }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Parser<N, T> {
    productions: Map<N, Expression<N, T>>,
    root: N,
}

impl<N: Clone + Ord, T: Clone + PartialEq> Parser<N, T> {
    pub fn new(_productions: &str) -> Parser<N, T> {
        panic!("Not implemented")
    }

    pub fn parse(&self, tokens: &[Token<T>]) -> Result<ParseTree<N, T>> {
        if let Some(expression) = self.productions.get(&self.root) {
            let mut parse_tree = expression.parse(tokens, &self.productions)?;
            if parse_tree.tokens_len() == tokens.len() {
                if let ParseTree::Ephemeral { tokens, children } = parse_tree {
                    parse_tree = ParseTree::Nonterminal {
                        nonterminal: self.root.clone(),
                        tokens,
                        children,
                    }
                } else { unreachable!("root of parse tree is not ephemeral"); }
                Ok(parse_tree)
            } else { Err(Error::from(ErrorKind::PartialMatch)) }
        } else { Err(Error::from(ErrorKind::UndefinedRootNonterminal)) }
    }

    pub fn from_productions(productions: Map<N, Expression<N, T>>, root: N) -> Parser<N, T> {
        Parser { productions, root }
    }
}

#[macro_export]
macro_rules! tok {
    ($x:expr) => {{
        $crate::parser::Expression::TokenKind { token_kind: $x }
    }}
}

#[macro_export]
macro_rules! non {
    ($x:expr) => {{
        $crate::parser::Expression::Nonterminal { nonterminal: $x }
    }}
}

#[macro_export]
macro_rules! alt {
    ($($x:expr),*) => {{
        #[allow(unused_mut)]
        let mut temp_vec = Vec::new();
        $(temp_vec.push($x);)*
        $crate::parser::Expression::Alternation { expressions: temp_vec }
    }}
}

#[macro_export]
macro_rules! con {
    ($($x:expr),*) => {{
        #[allow(unused_mut)]
        let mut temp_vec = Vec::new();
        $(temp_vec.push($x);)*
        $crate::parser::Expression::Concatenation { expressions: temp_vec }
    }}
}

#[macro_export]
macro_rules! rep {
    ($x:expr, $y:expr, $z:expr) => {{
        $crate::parser::Expression::Repetition { expression: Box::new($x), min: $y, max: $z }
    }}
}

#[macro_export]
macro_rules! ast { // asterisk
    ($x:expr) => {{
        $crate::parser::Expression::Repetition { expression: Box::new($x), min: None, max: None }
    }}
}

#[macro_export]
macro_rules! plu { // plus sign
    ($x:expr) => {{
        $crate::parser::Expression::Repetition { expression: Box::new($x), min: Some(1), max: None }
    }}
}

#[macro_export]
macro_rules! que { // question mark
    ($x:expr) => {{
        $crate::parser::Expression::Repetition { expression: Box::new($x), min: None, max: Some(1) }
    }}
}

#[cfg(test)]
mod tests {
    use lexer::Token;
    use crate::{
        error::Result,
        ParseTree,
        Parser,
    };

    #[test]
    fn test_expression() -> Result<()> {
        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
        enum TokenKind {
            PLUS_SIGN,
            HYPHEN,
            ASTERISK,
            SLASH,
            NUMBER,
            LEFT_PARENTHESIS,
            RIGHT_PARENTHESIS,
        }
        use TokenKind::*;
        #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
        enum Nonterminal {
            Addition,
            Multiplication,
            Atom,
            Number,
        }
        use Nonterminal::*;
        let expected = ParseTree::Nonterminal {
            nonterminal: Addition,
            tokens: vec![
                Token::new(NUMBER, "1"),
                Token::new(SLASH, "/"),
                Token::new(LEFT_PARENTHESIS, "("),
                Token::new(NUMBER, "2"),
                Token::new(PLUS_SIGN, "+"),
                Token::new(HYPHEN, "-"),
                Token::new(NUMBER, "3"),
                Token::new(RIGHT_PARENTHESIS, ")")
            ],
            children: vec![
                ParseTree::Nonterminal {
                    nonterminal: Multiplication,
                    tokens: vec![
                        Token::new(NUMBER, "1"),
                        Token::new(SLASH, "/"),
                        Token::new(LEFT_PARENTHESIS, "("),
                        Token::new(NUMBER, "2"),
                        Token::new(PLUS_SIGN, "+"),
                        Token::new(HYPHEN, "-"),
                        Token::new(NUMBER, "3"),
                        Token::new(RIGHT_PARENTHESIS, ")")
                    ],
                    children: vec![
                        ParseTree::Nonterminal {
                            nonterminal: Atom,
                            tokens: vec![Token::new(NUMBER, "1")],
                            children: vec![
                                ParseTree::Nonterminal {
                                    nonterminal: Number,
                                    tokens: vec![Token::new(NUMBER, "1")],
                                    children: vec![
                                        ParseTree::Token { token: Token::new(NUMBER, "1") }
                                    ],
                                },
                            ],
                        },
                        ParseTree::Token { token: Token::new(SLASH, "/") },
                        ParseTree::Nonterminal {
                            nonterminal: Atom,
                            tokens: vec![
                                Token::new(LEFT_PARENTHESIS, "("),
                                Token::new(NUMBER, "2"),
                                Token::new(PLUS_SIGN, "+"),
                                Token::new(HYPHEN, "-"),
                                Token::new(NUMBER, "3"),
                                Token::new(RIGHT_PARENTHESIS, ")")
                            ],
                            children: vec![
                                ParseTree::Token { token: Token::new(LEFT_PARENTHESIS, "(") },
                                ParseTree::Nonterminal {
                                    nonterminal: Addition,
                                    tokens: vec![
                                        Token::new(NUMBER, "2"),
                                        Token::new(PLUS_SIGN, "+"),
                                        Token::new(HYPHEN, "-"),
                                        Token::new(NUMBER, "3"),
                                    ],
                                    children: vec![
                                        ParseTree::Nonterminal {
                                            nonterminal: Multiplication,
                                            tokens: vec![Token::new(NUMBER, "2")],
                                            children: vec![
                                                ParseTree::Nonterminal {
                                                    nonterminal: Atom,
                                                    tokens: vec![Token::new(NUMBER, "2")],
                                                    children: vec![
                                                        ParseTree::Nonterminal {
                                                            nonterminal: Number,
                                                            tokens: vec![Token::new(NUMBER, "2")],
                                                            children: vec![
                                                                ParseTree::Token { token: Token::new(NUMBER, "2") }
                                                            ],
                                                        }
                                                    ]
                                                },
                                            ]
                                        },
                                        ParseTree::Token { token: Token::new(PLUS_SIGN, "+") },
                                        ParseTree::Nonterminal {
                                            nonterminal: Multiplication,
                                            tokens: vec![
                                                Token::new(HYPHEN, "-"),
                                                Token::new(NUMBER, "3")
                                            ],
                                            children: vec![
                                                ParseTree::Nonterminal {
                                                    nonterminal: Atom,
                                                    tokens: vec![
                                                        Token::new(HYPHEN, "-"),
                                                        Token::new(NUMBER, "3")
                                                    ],
                                                    children: vec![
                                                        ParseTree::Nonterminal {
                                                            nonterminal: Number,
                                                            tokens: vec![
                                                                Token::new(HYPHEN, "-"),
                                                                Token::new(NUMBER, "3")
                                                            ],
                                                            children: vec![
                                                                ParseTree::Token { token: Token::new(HYPHEN, "-") },
                                                                ParseTree::Token { token: Token::new(NUMBER, "3") },
                                                            ],
                                                        }
                                                    ]
                                                },
                                            ]
                                        },
                                    ]
                                },
                                ParseTree::Token { token: Token::new(RIGHT_PARENTHESIS, ")") },
                            ]
                        }
                    ],
                }
            ]
        };
        let parser = Parser::from_productions(map![
            Addition => con![non!(Multiplication), ast!(con![alt![tok!(PLUS_SIGN), tok!(HYPHEN)], non!(Multiplication)])],
            Multiplication => con![non!(Atom), ast!(con![alt![tok!(ASTERISK), tok!(SLASH)], non!(Atom)])],
            Atom => alt![non!(Number), con![tok!(LEFT_PARENTHESIS), non!(Addition), tok!(RIGHT_PARENTHESIS)]],
            Number => con![que!(alt![tok!(PLUS_SIGN), tok!(HYPHEN)]), tok!(NUMBER)]
        ], Addition);
        // 1 / (2 + -3)
        let actual = parser.parse(&[
            Token::new(NUMBER, "1"),
            Token::new(SLASH, "/"),
            Token::new(LEFT_PARENTHESIS, "("),
            Token::new(NUMBER, "2"),
            Token::new(PLUS_SIGN, "+"),
            Token::new(HYPHEN, "-"),
            Token::new(NUMBER, "3"), 
            Token::new(RIGHT_PARENTHESIS, ")")
        ])?;
        assert_eq!(expected, actual);
        Ok(())
    }
}
