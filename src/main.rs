//! This is a prolog interpreter in a single file. It is based on the repository
//! https://github.com/tanishqg5325/Prolog-Interpreter

use std::io::{stdin, stdout, Write};

use prolog::solve_program;

fn main() {
    let arg = std::env::args().nth(1).expect("No file provided");
    let str = std::fs::read_to_string(arg).expect("Could not read file");
    let parsed = prolog::parser::ProgramParser::new().parse(&str).expect("Could not parse file");
    let program = prolog::value::Program::from(parsed);

    print!("?- ");
    stdout().flush().unwrap();
    
    for line in stdin().lines() {

        let query = prolog::parser::AtomParser::new().parse(&line.unwrap()).expect("Could not parse query");
        solve_program(&program, query);

        print!("?- "); 
        stdout().flush().unwrap();   
    }
}