use std::collections::HashMap;
use std::hash::Hash;
use std::fmt::Debug;

#[derive(Clone, Debug)]
pub struct ParseTree<T> {
    pub name: T,
    pub contents: String,
    all_contents: String,
    pub children: Vec<ParseTree<T>>,
    is_nonterminal: bool,
}

impl<T: Clone> ParseTree<T> {
    pub fn new(name: &T, contents: &str, all_contents: &str, children: Vec<ParseTree<T>>, is_nonterminal: bool) -> Self {
        return ParseTree {
            name: name.clone(),
            contents: String::from(contents),
            all_contents: String::from(all_contents),
            children,
            is_nonterminal,
        };
    }
}

pub trait Parsable<T> {
    fn parse(&self, input: &str, definitions: &HashMap<T, Box<dyn Parsable<T> + Sync>>) -> Result<ParseTree<T>, String>;
}

pub struct Alternation<T> {
    parsables: Vec<Box<dyn Parsable<T> + Sync>>,
}

impl<T> Alternation<T> {
    pub fn new(parsables: Vec<Box<dyn Parsable<T> + Sync>>) -> Self {
        return Alternation { parsables };
    }
}

impl<T: Clone + Default> Parsable<T> for Alternation<T> {
    fn parse(&self, input: &str, definitions: &HashMap<T, Box<dyn Parsable<T> + Sync>>) -> Result<ParseTree<T>, String> {
        for parsable in &self.parsables {
            match parsable.parse(input, definitions) {
                Result::Ok(tree) => {
                    if tree.is_nonterminal {
                        return Result::Ok(ParseTree::new(&Default::default(), &tree.contents, &tree.all_contents, vec![tree.clone()], false));
                    } else {
                        return Result::Ok(ParseTree::new(&Default::default(), &tree.contents, &tree.all_contents, tree.children, false));
                    }
                },
                Result::Err(_) => continue,
            }
        }
        return Result::Err(format!("Input '{}' does not match any alternation", input));
    }
}

pub struct Concatenation<T> {
    parsables: Vec<Box<dyn Parsable<T> + Sync>>,
}

impl<T> Concatenation<T> {
    pub fn new(parsables: Vec<Box<dyn Parsable<T> + Sync>>) -> Self {
        return Concatenation { parsables };
    }
}

impl<T: Clone + Default> Parsable<T> for Concatenation<T> {
    fn parse(&self, input: &str, definitions: &HashMap<T, Box<dyn Parsable<T> + Sync>>) -> Result<ParseTree<T>, String> {
        let mut offset = 0;
        let mut contents = String::new();
        let mut children = Vec::new();
        for parsable in &self.parsables {
            match parsable.parse(&input[offset..], definitions) {
                Result::Ok(tree) => {
                    offset += tree.all_contents.len();
                    contents.push_str(&tree.contents);
                    if tree.is_nonterminal {
                        children.push(tree);
                    } else {
                        children.extend(tree.children);
                    }
                },
                Result::Err(msg) => return Result::Err(msg),
            }
        }
        return Result::Ok(ParseTree::new(&Default::default(), &contents, &input[..offset], children, false));
    }
}

pub struct Literal {
    value: String,
} 

impl Literal {
    pub fn new(value: &str) -> Self {
        return Literal { value: String::from(value) };
    }
}

impl<T: Clone + Default> Parsable<T> for Literal {
    fn parse(&self, input: &str, _definitions: &HashMap<T, Box<dyn Parsable<T> + Sync>>) -> Result<ParseTree<T>, String> {
        if input.len() < self.value.len() {
            return Result::Err(format!("Input '{}' is shorter than value '{}' to match", input, self.value));
        } else if &input[..self.value.len()] == self.value {
            return Result::Ok(ParseTree::new(&Default::default(), &self.value, &self.value, Vec::new(), false));
        } else {
            return Result::Err(format!("Input '{}' does not match value '{}'", input, self.value));
        }
    }
}

pub struct Nonterminal<T> {
    name: T,
}

impl<T> Nonterminal<T> {
    pub fn new(name: T) -> Self {
        return Nonterminal { name };
    }
}

impl<T: Debug + Clone + Hash + PartialEq + Eq> Parsable<T> for Nonterminal<T> {
    fn parse(&self, input: &str, definitions: &HashMap<T, Box<dyn Parsable<T> + Sync>>) -> Result<ParseTree<T>, String> {
        match definitions.get(&self.name) {
            Some(parsable) => {
                match parsable.parse(input, definitions) {
                    Result::Ok(tree) => {
                        if tree.is_nonterminal {
                            return Result::Ok(ParseTree::new(&self.name, &tree.contents, &tree.all_contents, vec![tree.clone()], true)); 
                        } else {
                            return Result::Ok(ParseTree::new(&self.name, &tree.contents, &tree.all_contents, tree.children, true));
                        }
                    },
                    Result::Err(msg) => return Result::Err(msg),
                }
            },
            None => return Result::Err(format!("Nonterminal '{:?}' has no matching definition", self.name)),
        }
    }
}

pub struct Repetition<T> {
    parsable: Box<dyn Parsable<T> + Sync>,
    min: Option<u32>,
    max: Option<u32>,
}

impl<T> Repetition<T> {
    pub fn new(parsable: Box<dyn Parsable<T> + Sync>, min: Option<u32>, max: Option<u32>) -> Self {
        return Repetition { 
            parsable,
            min,
            max,
        };
    }
}

impl<T: Sync + Clone + Debug + Default + Hash + PartialEq + Eq> Parsable<T> for Repetition<T> {
    fn parse(&self, input: &str, definitions: &HashMap<T, Box<dyn Parsable<T> + Sync>>) -> Result<ParseTree<T>, String> {
        let mut offset = 0;
        let mut contents = String::new();
        let mut children = Vec::new();
        let mut count = 0;
        while match self.max { Some(max) => count < max, None => true } {
            match self.parsable.parse(&input[offset..], definitions) {
                Result::Ok(tree) => {
                    offset += tree.all_contents.len();
                    contents.push_str(&tree.contents);
                    if tree.is_nonterminal {
                        children.push(tree);
                    } else {
                        children.extend(tree.children);
                    }
                    count += 1;
                },
                Result::Err(_) => {
                    if match self.min { Some(min) => count >= min, None => true } {
                        return Result::Ok(ParseTree::new(&Default::default(), &contents, &input[..offset], children, false));
                    } else {
                        return Result::Err(format!("Expected at least {} matches in '{}' but only had {}", self.min.unwrap(), input, count));
                    }
                }
            }
        }
        return Result::Ok(ParseTree::new(&Default::default(), &contents, &input[..offset], children, false));
    }
}

pub struct Skip<T> {
    parsable: Box<dyn Parsable<T> + Sync>,
}

impl<T> Skip<T> {
    pub fn new(parsable: Box<dyn Parsable<T> + Sync>) -> Self {
        return Skip { parsable };
    }
}

impl<T: Sync + Clone + Debug + Default + Hash + PartialEq + Eq> Parsable<T> for Skip<T> {
    fn parse(&self, input: &str, definitions: &HashMap<T, Box<dyn Parsable<T> + Sync>>) -> Result<ParseTree<T>, String> {
        let mut offset = 0;
        loop {
            match self.parsable.parse(&input[offset..], definitions) {
                Result::Ok(tree) => offset += tree.all_contents.len(),
                Result::Err(_) => return Result::Ok(ParseTree::new(&Default::default(), "", &input[..offset], Vec::new(), false)),
            }
        }
    }
}

#[macro_export]
macro_rules! alt {
    ($($x:expr),*) => {{
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push($x);
        )*
        let ret: Box<dyn crate::parser::Parsable<_> + Sync> = Box::new(crate::parser::Alternation::new(temp_vec));
        ret
    }}
}

#[macro_export]
macro_rules! cat {
    ($($x:expr),*) => {{
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push($x);
        )*
        let ret: Box<dyn crate::parser::Parsable<_> + Sync> = Box::new(crate::parser::Concatenation::new(temp_vec));
        ret
    }}
}

#[macro_export]
macro_rules! lit {
    ($x:expr) => {{
        let ret: Box<dyn crate::parser::Parsable<_> + Sync> = Box::new(crate::parser::Literal::new($x));
        ret
    }}
}

#[macro_export]
macro_rules! nt {
    ($x:expr) => {{
        let ret: Box<dyn crate::parser::Parsable<_> + Sync> = Box::new(crate::parser::Nonterminal::new($x));
        ret
    }}
}

#[macro_export]
macro_rules! rep {
    ($x:expr, $y:expr, $z:expr) => {{
        let ret: Box<dyn crate::parser::Parsable<_> + Sync> = Box::new(crate::parser::Repetition::new($x, $y, $z));
        ret
    }}
}

#[macro_export]
macro_rules! skp {
    ($x:expr) => {{
        let ret: Box<dyn crate::parser::Parsable<_> + Sync> = Box::new(crate::parser::Skip::new($x));
        ret
    }}
}
