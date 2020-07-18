use std::collections::BTreeMap as Map;
use std::fmt::Debug;
use crate::error::{
    Error,
    ErrorKind,
    Result,
};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum ParseTreeKind<N, T> {
    Nonterminal(N),
    Token(T),
    Ephemeral,
}

impl<N, T> ParseTreeKind<N, T> {
    pub fn is_nonterminal(&self) -> bool {
        match self {
            ParseTreeKind::Nonterminal(..) => true,
            _ => false,
        }
    }

    pub fn is_token(&self) -> bool {
        match self {
            ParseTreeKind::Token(..) => true,
            _ => false,
        }
    }

    pub fn is_ephemeral(&self) -> bool {
        match self {
            ParseTreeKind::Ephemeral => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ParseTree<N, T> {
    pub kind: ParseTreeKind<N, T>,
    pub tokens: Vec<T>,
    pub children: Vec<ParseTree<N, T>>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Expression<N, T> {
    Token { token: T },
    Nonterminal { nonterminal: N },
    Alternation { expressions: Vec<Expression<N, T>> },
    Concatenation { expressions: Vec<Expression<N, T>> },
    Repetition { expression: Box<Expression<N, T>>, min: Option<u32>, max: Option<u32> },
}

impl<N: Clone + Debug + Ord, T: Clone + Debug + PartialEq> Expression<N, T> {
    pub fn parse(&self, tokens: &[T], productions: &Map<N, Expression<N, T>>) -> Result<ParseTree<N, T>> {
        match self {
            Expression::Token { token } => {
                if let Some(current_token) = tokens.get(0) {
                    if token == current_token {
                        Ok(ParseTree {
                            kind: ParseTreeKind::Token(current_token.clone()),
                            tokens: vec![current_token.clone()],
                            children: Vec::new(),
                        })
                    } else {
                        Err(Error::from(ErrorKind::UnexpectedToken))
                    }
                } else {
                    Err(Error::from(ErrorKind::UnexpectedEOF))
                }
            },
            Expression::Nonterminal { nonterminal } => {
                if let Some(expression) = productions.get(nonterminal) {
                    let parse_tree = expression.parse(tokens, productions)?;
                    if parse_tree.kind.is_ephemeral() {
                        Ok(ParseTree {
                            kind: ParseTreeKind::Nonterminal(nonterminal.clone()),
                            tokens: parse_tree.tokens,
                            children: parse_tree.children,
                        })
                    } else {
                        Ok(ParseTree {
                            kind: ParseTreeKind::Nonterminal(nonterminal.clone()),
                            tokens: parse_tree.tokens.clone(),
                            children: vec![parse_tree],
                        })
                    }
                } else {
                    Err(Error::from(ErrorKind::UndefinedNonterminal))
                }
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
                        if parse_tree.tokens.len() < current_parse_tree.tokens.len() {
                            parse_tree = current_parse_tree;
                        }
                    }
                    if parse_tree.kind.is_ephemeral() {
                        Ok(ParseTree {
                            kind: ParseTreeKind::Ephemeral,
                            tokens: parse_tree.tokens,
                            children: parse_tree.children,
                        })
                    } else {
                        Ok(ParseTree {
                            kind: ParseTreeKind::Ephemeral,
                            tokens: parse_tree.tokens.clone(),
                            children: vec![parse_tree],
                        })
                    }
                } else {
                    Err(Error::from(ErrorKind::NoMatch))
                }
            },
            Expression::Concatenation { expressions } => {
                let mut offset = 0;
                let mut matched_tokens = Vec::new();
                let mut children = Vec::new();
                for expression in expressions {
                    if let Ok(parse_tree) = expression.parse(&tokens[offset..], productions) {
                        offset += parse_tree.tokens.len();
                        matched_tokens.extend(parse_tree.tokens.clone());
                        if parse_tree.kind.is_ephemeral() {
                            children.extend(parse_tree.children);
                        } else {
                            children.push(parse_tree);
                        }
                    } else {
                        return Err(Error::from(ErrorKind::NoMatch));
                    }
                }
                Ok(ParseTree {
                    kind: ParseTreeKind::Ephemeral,
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
                        offset += parse_tree.tokens.len();
                        matched_tokens.extend(parse_tree.tokens.clone());
                        if parse_tree.kind.is_ephemeral() {
                            children.extend(parse_tree.children);
                        } else {
                            children.push(parse_tree);
                        }
                    } else if match min { Some(min) => count >= *min, None => true } {
                        return Ok(ParseTree {
                            kind: ParseTreeKind::Ephemeral,
                            tokens: matched_tokens,
                            children,
                        });
                    } else {
                        return Err(Error::from(ErrorKind::PartialMatch));
                    }
                }
                Ok(ParseTree {
                    kind: ParseTreeKind::Ephemeral,
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

impl<N: Clone + Debug + Ord, T: Clone + Debug + PartialEq> Parser<N, T> {
    pub fn new(_productions: &str) -> Parser<N, T> {
        panic!("Not implemented")
    }

    pub fn parse(&self, tokens: &[T]) -> Result<ParseTree<N, T>> {
        if let Some(expression) = self.productions.get(&self.root) {
            let mut parse_tree = expression.parse(tokens, &self.productions)?;
            if parse_tree.tokens.len() == tokens.len() {
                parse_tree.kind = ParseTreeKind::Nonterminal(self.root.clone());
                Ok(parse_tree)
            } else {
                Err(Error::from(ErrorKind::PartialMatch))
            }
        } else {
            Err(Error::from(ErrorKind::UndefinedRootNonterminal))
        }
    }

    pub fn from_productions(productions: Map<N, Expression<N, T>>, root: N) -> Parser<N, T> {
        Parser { productions, root }
    }
}

#[macro_export]
macro_rules! tok {
    ($x:expr) => {{
        $crate::parser::Expression::Token { token: $x }
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
    use crate::{
        error::Result,
        ParseTreeKind,
        ParseTree,
        Parser,
    };

    #[test]
    fn test_expression() -> Result<()> {
        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
        enum Token {
            PLUS_SIGN,
            HYPHEN,
            ASTERISK,
            SLASH,
            NUMBER,
            LEFT_PARENTHESIS,
            RIGHT_PARENTHESIS,
        }
        use Token::*;
        #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
        enum Nonterminal {
            Addition,
            Multiplication,
            Atom,
            Number,
        }
        use Nonterminal::*;
        let expected = ParseTree {
            kind: ParseTreeKind::Nonterminal(Addition),
            tokens: vec![NUMBER, SLASH, LEFT_PARENTHESIS, NUMBER, PLUS_SIGN, HYPHEN, NUMBER, RIGHT_PARENTHESIS],
            children: vec![
                ParseTree {
                    kind: ParseTreeKind::Nonterminal(Multiplication),
                    tokens: vec![NUMBER, SLASH, LEFT_PARENTHESIS, NUMBER, PLUS_SIGN, HYPHEN, NUMBER, RIGHT_PARENTHESIS],
                    children: vec![
                        ParseTree {
                            kind: ParseTreeKind::Nonterminal(Atom),
                            tokens: vec![NUMBER],
                            children: vec![
                                ParseTree {
                                    kind: ParseTreeKind::Nonterminal(Number),
                                    tokens: vec![NUMBER],
                                    children: vec![
                                        ParseTree {
                                            kind: ParseTreeKind::Token(NUMBER),
                                            tokens: vec![NUMBER],
                                            children: Vec::new(),
                                        }
                                    ],
                                },
                            ],
                        },
                        ParseTree {
                            kind: ParseTreeKind::Token(SLASH),
                            tokens: vec![SLASH],
                            children: Vec::new(),
                        },
                        ParseTree {
                            kind: ParseTreeKind::Nonterminal(Atom),
                            tokens: vec![LEFT_PARENTHESIS, NUMBER, PLUS_SIGN, HYPHEN, NUMBER, RIGHT_PARENTHESIS],
                            children: vec![
                                ParseTree {
                                    kind: ParseTreeKind::Token(LEFT_PARENTHESIS),
                                    tokens: vec![LEFT_PARENTHESIS],
                                    children: Vec::new(),
                                },
                                ParseTree {
                                    kind: ParseTreeKind::Nonterminal(Addition),
                                    tokens: vec![NUMBER, PLUS_SIGN, HYPHEN, NUMBER],
                                    children: vec![
                                        ParseTree {
                                            kind: ParseTreeKind::Nonterminal(Multiplication),
                                            tokens: vec![NUMBER],
                                            children: vec![
                                                ParseTree {
                                                    kind: ParseTreeKind::Nonterminal(Atom),
                                                    tokens: vec![NUMBER],
                                                    children: vec![
                                                        ParseTree {
                                                            kind: ParseTreeKind::Nonterminal(Number),
                                                            tokens: vec![NUMBER],
                                                            children: vec![
                                                                ParseTree {
                                                                    kind: ParseTreeKind::Token(NUMBER),
                                                                    tokens: vec![NUMBER],
                                                                    children: Vec::new(),
                                                                }
                                                            ],
                                                        }
                                                    ]
                                                },
                                            ]
                                        },
                                        ParseTree {
                                            kind: ParseTreeKind::Token(PLUS_SIGN),
                                            tokens: vec![PLUS_SIGN],
                                            children: Vec::new(),
                                        },
                                        ParseTree {
                                            kind: ParseTreeKind::Nonterminal(Multiplication),
                                            tokens: vec![HYPHEN, NUMBER],
                                            children: vec![
                                                ParseTree {
                                                    kind: ParseTreeKind::Nonterminal(Atom),
                                                    tokens: vec![HYPHEN, NUMBER],
                                                    children: vec![
                                                        ParseTree {
                                                            kind: ParseTreeKind::Nonterminal(Number),
                                                            tokens: vec![HYPHEN, NUMBER],
                                                            children: vec![
                                                                ParseTree {
                                                                    kind: ParseTreeKind::Token(HYPHEN),
                                                                    tokens: vec![HYPHEN],
                                                                    children: Vec::new(),
                                                                },
                                                                ParseTree {
                                                                    kind: ParseTreeKind::Token(NUMBER),
                                                                    tokens: vec![NUMBER],
                                                                    children: Vec::new(),
                                                                },
                                                            ],
                                                        }
                                                    ]
                                                },
                                            ]
                                        },
                                    ]
                                },
                                ParseTree {
                                    kind: ParseTreeKind::Token(RIGHT_PARENTHESIS),
                                    tokens: vec![RIGHT_PARENTHESIS],
                                    children: Vec::new(),
                                },
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
        let actual = parser.parse(&[NUMBER, SLASH, LEFT_PARENTHESIS, NUMBER, PLUS_SIGN, HYPHEN, NUMBER, RIGHT_PARENTHESIS])?;
        assert_eq!(expected, actual);
        Ok(())
    }
}
