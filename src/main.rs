#[macro_use]
extern crate lazy_static;

mod parser;
mod util;
mod char_class;

fn main() {
    match char_class::characters("abc]") {
        Ok(characters) => println!("{:?}", characters),
        Err(msg) => println!("{}", msg),
    }
}
