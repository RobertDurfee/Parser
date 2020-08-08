use std::str::FromStr;
use lexer_bootstrap::{
    Token,
    Lexer,
};
use parser_bootstrap::{
    error::Result,
    Parser as ParserBootstrap,
    ParseTree,
};
use crate::grammar::{
    LEXER_PRODUCTIONS,
    PARSER_PRODUCTIONS,
    Nonterminal,
    as_productions,
};

pub struct Parser<N, T> {
    parser: ParserBootstrap<N, T>,
}

impl<N: FromStr + Ord, T: FromStr> Parser<N, T> {
    pub fn new(productions: &str, root: N) -> Result<Parser<N, T>> {
        let mut lexer = Lexer::new(LEXER_PRODUCTIONS.clone()); lexer.compile();
        let parser = ParserBootstrap::new(PARSER_PRODUCTIONS.clone(), Nonterminal::Root);
        let tokens = lexer.lex(productions)?;
        let parse_tree = parser.parse(&tokens)?;
        let productions = as_productions(&parse_tree)?;
        Ok(Parser { parser: ParserBootstrap::new(productions, root) })
    }
}

impl<N: Clone + Ord, T: Clone + PartialEq> Parser<N, T> {
    pub fn parse(&self, tokens: &[Token<T>]) -> Result<ParseTree<N, T>> {
        self.parser.parse(tokens)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use lexer_bootstrap::Token;
    use parser_bootstrap::{
        error::{
            Result,
            Error,
            ErrorKind,
        },
        ParseTree,
    };
    use crate::Parser;

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

    impl FromStr for TokenKind {
        type Err = Error;

        fn from_str(text: &str) -> Result<Self> {
            use TokenKind::*;
            match text {
                "PLUS_SIGN" => Ok(PLUS_SIGN),
                "HYPHEN" => Ok(HYPHEN),
                "ASTERISK" => Ok(ASTERISK),
                "SLASH" => Ok(SLASH),
                "NUMBER" => Ok(NUMBER),
                "LEFT_PARENTHESIS" => Ok(LEFT_PARENTHESIS),
                "RIGHT_PARENTHESIS" => Ok(RIGHT_PARENTHESIS),
                _ => Err(Error::from(ErrorKind::NotTokenKind))
            }
        }
    }

    #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    enum Nonterminal {
        Addition,
        Multiplication,
        Atom,
        Number,
    }

    impl FromStr for Nonterminal {
        type Err = Error;

        fn from_str(text: &str) -> Result<Self> {
            use Nonterminal::*;
            match text {
                "Addition" => Ok(Addition),
                "Multiplication" => Ok(Multiplication),
                "Atom" => Ok(Atom),
                "Number" => Ok(Number),
                _ => Err(Error::from(ErrorKind::NotNonterminal))
            }
        }
    }

    #[test]
    fn test_expression() -> Result<()> {
        use TokenKind::*;
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
        let parser = Parser::new(r#"
            Addition ::= Multiplication ((PLUS_SIGN | HYPHEN) Multiplication)*;
            Multiplication ::= Atom ((ASTERISK | SLASH) Atom)*;
            Atom ::= Number | LEFT_PARENTHESIS Addition RIGHT_PARENTHESIS;
            Number ::= (PLUS_SIGN | HYPHEN)? NUMBER;
        "#, Addition)?;
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
