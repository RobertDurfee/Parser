use lexer_bootstrap::Token;
use parser_bootstrap::{
    error::Result,
    Parser as ParserBootstrap,
    ParseTree,
};

pub struct Parser<N, T> {
    parser: ParserBootstrap<N, T>,
}

impl<N, T> Parser<N, T> {
    pub fn new(_productions: &str, _root: N) -> Parser<N, T> {
        panic!("Not implemented")
    }
}

impl<N: Clone + Ord, T: Clone + PartialEq> Parser<N, T> {
    pub fn parse(&self, tokens: &[Token<T>]) -> Result<ParseTree<N, T>> {
        self.parser.parse(tokens)
    }
}
