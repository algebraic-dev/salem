//! This is the library module for the prolog compiler. It declares some structures and functions
//! that are used in the compiler.

use std::collections::HashMap;

use lalrpop_util::lalrpop_mod;
use puddle::Puddle;
use value::Value;

lalrpop_mod!(pub parser);

pub mod tree;
pub mod puddle;
pub mod unify;
pub mod value;

#[derive(Debug, PartialEq, Eq)]
pub enum Control {
    Continue,
    Halt
}

impl From<char> for Control {
    fn from(c: char) -> Self {
        match c {
            '.' => Control::Halt,
            ';' => Control::Continue,
            _ => panic!("Invalid control character: {}", c),
        }
    }
}

pub fn read_control() -> Control {
    Control::Continue
}

pub fn solve_goal(program: &value::Program, puddle: &mut Puddle<Value>, mut goal: value::Goal, holes: &im_rc::HashMap<String, Value>) -> Control {
    if let Some(atom) = goal.shift() {
        let rules = program.get(&atom.signature());

        if let Some(rules) = rules {
            for rule in rules {
                let instantiated = rule.instantiate(puddle);

                puddle.stage();

                if instantiated.head.unify(puddle, &atom) {
                    let goal = goal.preppend_vec(instantiated.body.unwrap_or_default());
                    
                    let control = solve_goal(program, puddle, goal, holes);

                    if control == Control::Halt {
                        puddle.commit();
                        return Control::Halt;
                    }
                }

                puddle.rollback()
            }  
        }

        Control::Continue
    } else {
        println!("");

        for (key, value) in holes {
            println!("{} = {}", key, value.with(puddle));
        }

        println!("\nNext? (;/.)");
        read_control() 
    }
}

pub fn solve_program(program: &value::Program, goal: tree::Atom) {
    let mut puddle = Puddle::new();
    let vars = goal.get_vars(&mut puddle);
    let goal = value::Goal::singleton(goal.substitute(vars.clone()));
    let control = solve_goal(program, &mut puddle, goal, &vars);

    if control == Control::Continue {
        println!("\nNo.");
    }

}