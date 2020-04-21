use std::collections::HashMap;
use std::hash::Hash;
use std::fmt::Display;
use std::fmt;

#[derive(Clone, Debug)]
pub struct Tree<'a, T> {
    pub name: Option<T>,
    pub contents: Vec<&'a str>,
    pub children: Vec<Tree<'a, T>>,
}

enum Term<T> {
    Alternation(Vec<Box<Term<T>>>),
    CharacterClass(Box<Term<T>>, String),
    Concatenation(Vec<Box<Term<T>>>),
    Literal(String),
    Nonterminal(T),
    Repetition(Box<Term<T>>, Option<u32>, Option<u32>),
    Skip(Box<Term<T>>),
}

impl<'a, T: Clone + Display + Eq + PartialEq + Hash> Term<T> {
    fn parse(&self, input: &'a str, definitions: &HashMap<T, Term<T>>) -> Result<(Tree<'a, T>, usize, bool), String> {
        match *self {
            Term::Alternation(ref terms) => {
                for term in terms {
                    let (tree, offset, is_nonterminal) = term.parse(input, definitions)?;
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
                return Err(format!("Input '{}' does not match '{}'", input, self));
            },
            Term::CharacterClass(ref term, _) => {
                return term.parse(input, definitions);
            },
            Term::Concatenation(ref terms) => {
                let mut contents = Vec::new();
                let mut total_offset = 0;
                let mut children = Vec::new();
                for term in terms {
                    let (tree, offset, is_nonterminal) = term.parse(input, definitions)?;
                    total_offset += offset;
                    contents.extend(&tree.contents);
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
                    return Err(format!("Input '{}' is shorter than value '{}' to match", input, value));
                }
                if &input[..value.len()] == value {
                    return Ok((Tree {
                        name: None,
                        contents: vec![&input[..value.len()]],
                        children: Vec::new(),
                    }, value.len(), false));
                } else {
                    return Err(format!("Input '{}' does not match value '{}'", &input[..value.len()], value));
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
                    None => return Err(format!("Nonterminal '{}' has no matching definition", name)),
                }
            },
            Term::Repetition(ref term, ref min, ref max) => {
                let mut count = 0;
                let mut contents = Vec::new();
                let mut total_offset = 0;
                let mut children = Vec::new();
                while match max { Some(max) => count < *max, None => true } {
                    match term.parse(&input[total_offset..], definitions) {
                        Ok((tree, offset, is_nonterminal)) => {
                            count += 1;
                            total_offset += offset;
                            contents.extend(&tree.contents);
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
                                return Err(format!("Input '{}' has only {} matches of '{}', expected {}", input, count, term, min.unwrap()));
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
                                contents: Vec::new(),
                                children: Vec::new(),
                            }, total_offset, false));
                        },
                    }
                }
            },
        }
    }
}

impl<T: Display> Display for Term<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Term::Alternation(ref terms) |
            Term::Concatenation(ref terms) => {
                write!(f, "(")?;
                if terms.len() > 0 {
                    for term in terms.iter().take(terms.len()-1) {
                        write!(f, "{}|", term)?;
                    }
                    write!(f, "{}", terms.last().unwrap())?;
                }
                return write!(f, ")");
            },
            Term::CharacterClass(_, ref representation) => write!(f, "{}", representation),
            Term::Literal(ref value) => write!(f, "'{}'", value),
            Term::Nonterminal(ref name) => write!(f, "{}", name),
            Term::Repetition(ref term, ref min, ref max) => {
                match (min, max) {
                    (None, None) |
                    (Some(0), None) => write!(f, "({}*)", term),
                    (Some(1), None) => write!(f, "({}+)", term),
                    (Some(0), Some(1)) => write!(f, "({}?)", term),
                    (Some(min), Some(max)) => write!(f, "({}{{{},{}}})", term, min, max),
                    (Some(min), None) => write!(f, "({}{{{},}})", term, min),
                    (None, Some(max)) => write!(f, "({}{{,{}}})", term, max),
                }
            },
            Term::Skip(ref term) => write!(f, "(skip{})", term),
        }
    }
}

fn parse<'a, T: Clone + Display + Eq + Hash + PartialEq>(input: &'a str, definitions: &HashMap<T, Term<T>>, root: T) -> Result<Tree<'a, T>, String> {
    let (tree, offset, _) = Term::Nonterminal(root).parse(input, definitions)?;
    if offset != input.len() {
        return Err(format!("Whole input '{}' not matched, only matched '{}'", input, &input[..offset]));
    }
    return Ok(tree);
}
