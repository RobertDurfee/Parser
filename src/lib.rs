#[macro_use]
extern crate lazy_static;
extern crate enum_utils;

mod parser;
mod util;
mod character_class;
mod grammar;

use std::fmt::Debug;
use std::hash::Hash;
use std::collections::HashMap;
use std::str::FromStr;
use parser::{Term, Tree};

pub fn parse<'a, T: Clone + Debug + Eq + Hash + PartialEq>(input: &'a str, definitions: &HashMap<T, Box<Term<T>>>, root: T) -> Result<Tree<T>, String> {
    return parser::parse(input, definitions, root);
}

pub fn definitions<T: Clone + Debug + Eq + FromStr + Hash + Sync + 'static>(input: &str) -> Result<HashMap<T, Box<parser::Term<T>>>, String> {
    return grammar::definitions(input);
}
