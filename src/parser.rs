use std::collections::HashMap;
use std::hash::Hash;
use std::fmt::Debug;
use std::fmt;

use crate::character_class;

#[derive(Clone, Debug)]
pub struct Tree<T> {
    pub name: Option<T>,
    pub contents: String,
    pub children: Vec<Tree<T>>,
}

pub struct CharacterClass<T> {
    term: Box<Term<T>>,
    expression: String,
}

impl<T> CharacterClass<T> {
    pub fn new(expression: &str) -> Self {
        let mut terms = Vec::new();
        for character in character_class::characters(expression).unwrap() {
            terms.push(Box::new(Term::Literal(character.to_string())));
        }
        return CharacterClass {
            expression: String::from(expression),
            term: Box::new(Term::Alternation(terms)),
        };
    }
}

pub enum Term<T> {
    Alternation(Vec<Box<Term<T>>>),
    CharacterClass(CharacterClass<T>),
    Concatenation(Vec<Box<Term<T>>>),
    Literal(String),
    Nonterminal(T),
    Repetition(Box<Term<T>>, Option<u32>, Option<u32>),
    Skip(Box<Term<T>>),
}

impl<'a, T: Clone + Debug + Eq + PartialEq + Hash> Term<T> {
    fn parse(&self, input: &'a str, definitions: &HashMap<T, Box<Term<T>>>) -> Result<(Tree<T>, usize, bool), String> {
        match *self {
            Term::Alternation(ref terms) => {
                for term in terms {
                    if let Ok((tree, offset, is_nonterminal)) = term.parse(input, definitions) {
                        if is_nonterminal {
                            return Ok((Tree {
                                name: None,
                                contents: tree.contents.clone(),
                                children: vec![tree],
                            }, offset, false));
                        } else {
                            return Ok((Tree {
                                name: None,
                                contents: tree.contents,
                                children: tree.children,
                            }, offset, false));
                        }
                    }
                }
                return Err(format!("Input '{:?}' does not match '{:?}'", input, self));
            },
            Term::CharacterClass(CharacterClass { ref term, .. }) => {
                return term.parse(input, definitions);
            },
            Term::Concatenation(ref terms) => {
                let mut contents = String::new();
                let mut total_offset = 0;
                let mut children = Vec::new();
                for term in terms {
                    let (tree, offset, is_nonterminal) = term.parse(&input[total_offset..], definitions)?;
                    total_offset += offset;
                    contents.push_str(&tree.contents);
                    if is_nonterminal {
                        children.push(tree);
                    } else {
                        children.extend(tree.children);
                    }
                }
                return Ok((Tree {
                    name: None,
                    contents,
                    children,
                }, total_offset, false));
            },
            Term::Literal(ref value) => {
                if input.len() < value.len() {
                    return Err(format!("Input '{:?}' is shorter than value '{:?}' to match", input, value));
                }
                if &input[..value.len()] == value {
                    return Ok((Tree {
                        name: None,
                        contents: String::from(&input[..value.len()]),
                        children: Vec::new(),
                    }, value.len(), false));
                } else {
                    return Err(format!("Input '{:?}' does not match value '{:?}'", &input[..value.len()], value));
                }
            },
            Term::Nonterminal(ref name) => {
                match definitions.get(name) {
                    Some(term) => {
                        let (tree, offset, is_nonterminal) = term.parse(input, definitions)?;
                        if is_nonterminal {
                            return Ok((Tree {
                                name: Some(name.clone()),
                                contents: tree.contents.clone(),
                                children: vec![tree],
                            }, offset, true));
                        } else {
                            return Ok((Tree {
                                name: Some(name.clone()),
                                contents: tree.contents,
                                children: tree.children,
                            }, offset, true));
                        }
                    },
                    None => return Err(format!("Nonterminal '{:?}' has no matching definition", name)),
                }
            },
            Term::Repetition(ref term, ref min, ref max) => {
                let mut count = 0;
                let mut contents = String::new();
                let mut total_offset = 0;
                let mut children = Vec::new();
                while match max { Some(max) => count < *max, None => true } {
                    match term.parse(&input[total_offset..], definitions) {
                        Ok((tree, offset, is_nonterminal)) => {
                            count += 1;
                            total_offset += offset;
                            contents.push_str(&tree.contents);
                            if is_nonterminal {
                                children.push(tree);
                            } else {
                                children.extend(tree.children);
                            }
                        },
                        Err(_) => {
                            if match min { Some(min) => count >= *min, None => true } {
                                return Ok((Tree {
                                    name: None,
                                    contents,
                                    children,
                                }, total_offset, false));
                            } else {
                                return Err(format!("Input '{:?}' has only {:?} matches of '{:?}', expected at least {:?}", input, count, term, min.unwrap()));
                            }
                        }
                    }
                }
                return Ok((Tree {
                    name: None,
                    contents,
                    children,
                }, total_offset, false));
            },
            Term::Skip(ref term) => {
                let mut total_offset = 0;
                loop {
                    match term.parse(&input[total_offset..], definitions) {
                        Ok((_, offset, _)) => {
                            total_offset += offset;
                        },
                        Err(_) => {
                            return Ok((Tree {
                                name: None,
                                contents: String::new(),
                                children: Vec::new(),
                            }, total_offset, false));
                        },
                    }
                }
            },
        }
    }
}

impl<T: Debug> Debug for Term<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Term::Alternation(ref terms) => {
                write!(f, "(")?;
                if terms.len() > 0 {
                    for term in terms.iter().take(terms.len()-1) {
                        write!(f, "{:?}|", term)?;
                    }
                    write!(f, "{:?}", terms.last().unwrap())?;
                }
                return write!(f, ")");
            },
            Term::Concatenation(ref terms) => {
                write!(f, "(")?;
                if terms.len() > 0 {
                    for term in terms.iter().take(terms.len()-1) {
                        write!(f, "{:?} ", term)?;
                    }
                    write!(f, "{:?}", terms.last().unwrap())?;
                }
                return write!(f, ")");
            },
            Term::CharacterClass(CharacterClass { ref expression, .. }) => write!(f, "{}", expression),
            Term::Literal(ref value) => write!(f, "'{:?}'", value),
            Term::Nonterminal(ref name) => write!(f, "{:?}", name),
            Term::Repetition(ref term, ref min, ref max) => {
                match (min, max) {
                    (None, None) |
                    (Some(0), None) => write!(f, "({:?}*)", term),
                    (Some(1), None) => write!(f, "({:?}+)", term),
                    (Some(0), Some(1)) => write!(f, "({:?}?)", term),
                    (Some(min), Some(max)) => write!(f, "({:?}{{{:?},{:?}}})", term, min, max),
                    (Some(min), None) => write!(f, "({:?}{{{:?},}})", term, min),
                    (None, Some(max)) => write!(f, "({:?}{{,{:?}}})", term, max),
                }
            },
            Term::Skip(ref term) => write!(f, "(skip{:?})", term),
        }
    }
}

pub fn parse<'a, T: Clone + Debug + Eq + Hash + PartialEq>(input: &'a str, definitions: &HashMap<T, Box<Term<T>>>, root: T) -> Result<Tree<T>, String> {
    let (tree, offset, _) = Term::Nonterminal(root).parse(input, definitions)?;
    if offset != input.len() {
        return Err(format!("Whole input '{:?}' not matched, only matched '{:?}'", input, &input[..offset]));
    }
    return Ok(tree);
}

#[macro_export]
macro_rules! alt {
    ($($x:expr),*) => {{
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push($x);
        )*
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::Alternation(temp_vec));
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
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::Concatenation(temp_vec));
        ret
    }}
}

#[macro_export]
macro_rules! chc {
    ($x:expr) => {{
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::CharacterClass(crate::parser::CharacterClass::new($x)));
        ret
    }}
}

#[macro_export]
macro_rules! lit {
    ($x:expr) => {{
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::Literal(String::from($x)));
        ret
    }}
}

#[macro_export]
macro_rules! nt {
    ($x:expr) => {{
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::Nonterminal($x));
        ret
    }}
}

#[macro_export]
macro_rules! rep {
    ($x:expr, $y:expr, $z:expr) => {{
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::Repetition($x, $y, $z));
        ret
    }}
}

#[macro_export]
macro_rules! star {
    ($x:expr) => {{
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::Repetition($x, Some(0), None));
        ret
    }}
}
 
#[macro_export]
macro_rules! plus {
    ($x:expr) => {{
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::Repetition($x, Some(1), None));
        ret
    }}
}
 
#[macro_export]
macro_rules! qm {
    ($x:expr) => {{
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::Repetition($x, Some(0), Some(1)));
        ret
    }}
}

#[macro_export]
macro_rules! skp {
    ($x:expr) => {{
        let ret: Box<crate::parser::Term<_>> = Box::new(crate::parser::Term::Skip($x));
        ret
    }}
}
