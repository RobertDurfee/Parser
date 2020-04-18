#[derive(Debug)]
struct ParseTree<'a> {
    #[allow(dead_code)]
    contents: &'a str,
    #[allow(dead_code)]
    children: Vec<ParseTree<'a>>
}

trait Parsable<'a> {
    fn parse(&self, input: &'a str) -> Result<ParseTree, &'static str>;
}

struct Literal<'a> {
    value: &'a str,
} 

impl<'a> Literal<'a> {
    #[allow(dead_code)]
    fn new(value: &'a str) -> Self {
        return Literal { value }
    }
}

impl<'a> Parsable<'a> for Literal<'a> {
    fn parse(&self, input: &'a str) -> Result<ParseTree, &'static str> {
        if input.len() < self.value.len() {
            return Err("Failed to match: input is shorter than value to match");
        } else if &input[..self.value.len()] == self.value {
            return Ok(ParseTree {
                contents: &input[..self.value.len()],
                children: vec![],
            })
        } else {
            return Err("Failed to match: input doesn't match value");
        }
    }
}

struct Concatenation<'a, T: Parsable<'a>> {
    parsables: &'a [&'a T],
}

impl<'a, T: Parsable<'a>> Concatenation<'a, T> {
    #[allow(dead_code)]
    fn new(parsables: &'a [&'a T]) -> Self {
        return Concatenation { parsables }
    }
}

impl<'a, T: Parsable<'a>> Parsable<'a> for Concatenation<'a, T> {
    fn parse(&self, input: &'a str) -> Result<ParseTree, &'static str> {
        let mut children: Vec<ParseTree<'a>> = vec![];
        let mut offset: usize = 0;
        for parsable in self.parsables {
            match parsable.parse(&input[offset..]) {
                Result::Ok(tree) => {
                    offset += tree.contents.len();
                    children.push(tree);
                },
                Result::Err(msg) => return Err(msg),
            }
        }
        return Ok(ParseTree {
            contents: &input[..offset],
            children,
        });
    }
}

struct Alternation<'a, T: Parsable<'a>> {
    parsables: &'a [&'a T],
}

impl<'a, T: Parsable<'a>> Alternation<'a, T> {
    #[allow(dead_code)]
    fn new(parsables: &'a [&'a T]) -> Self {
        return Alternation { parsables }
    }
}

impl<'a, T: Parsable<'a>> Parsable<'a> for Alternation<'a, T> {
    fn parse(&self, input: &'a str) -> Result<ParseTree, &'static str> {
        for parsable in self.parsables {
            match parsable.parse(input) {
                Result::Ok(tree) => {
                    return Ok(ParseTree {
                        contents: &input[..tree.contents.len()],
                        children: vec![tree],
                    });
                },
                Result::Err(_) => continue,
            }
        }
        return Err("Failed to match: input doesn't match any values");
    }
}

struct Repetition<'a, T: Parsable<'a>> {
    parsable: &'a T,
}

impl<'a, T: Parsable<'a>> Repetition<'a, T> {
    #[allow(dead_code)]
    fn new(parsable: &'a T) -> Self {
        return Repetition { parsable };
    }
}

impl<'a, T: Parsable<'a>> Parsable<'a> for Repetition<'a, T> {
    fn parse(&self, input: &'a str) -> Result<ParseTree, &'static str> {
        let mut children: Vec<ParseTree<'a>> = vec![];
        let mut offset: usize = 0;
        loop {
            match self.parsable.parse(&input[offset..]) {
                Result::Ok(tree) => {
                    offset += tree.contents.len();
                    children.push(tree);
                },
                Result::Err(_) => {
                    return Ok(ParseTree {
                        contents: &input[..offset],
                        children,
                    });
                },
            }
        }
    }
}

fn main() {
    let literal = Literal::new("test");
    let repetition = Repetition::new(&literal);
    match repetition.parse("test") {
        Result::Ok(tree) => println!("{:?}", tree),
        Result::Err(msg) => println!("{}", msg),
    }
}
