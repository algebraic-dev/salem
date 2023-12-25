//! This is the library module for the prolog compiler. It declares some structures and functions
//! that are used in the compiler.

use std::io;

use im_rc::HashMap;
use value::Value;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);

/// This module defines the abstract syntax tree for the prolog language. It is used to represent
/// the program that is being compiled.
pub mod tree {
    use im_rc::HashMap;

    use crate::value::{Value, self};

    /// This structure represents an atom in the prolog language. An atom is a predicate with a
    /// name and a list of arguments. The arguments are terms, which can be numbers, variables or
    /// atoms.
    /// 
    /// # Example
    /// ```prolog
    /// parent(john, mary)
    /// ```
    pub struct Atom {
        pub name: String,
        pub args: Vec<Term>,
    }
    
    impl Atom {
        /// Returns the set of free variables in the atom.
        pub fn free_variables(&self) -> im_rc::HashSet<String> {
            self.args.iter().fold(im_rc::HashSet::new(), |mut acc, term| {
                acc.extend(term.free_variables());
                acc
            })
        }

        pub fn instantiate_with(&self, subs: HashMap<String, Value>) -> value::Atom {
            value::Atom {
                name: self.name.clone(),
                args: self.args.iter().map(|term| term.instantiate_with(subs.clone())).collect(),
            }
        }

        pub fn get_vars(&self) -> im_rc::HashMap<String, Value> {
            let fvs = self.free_variables();
            let mut subs = HashMap::new();
            
            for fv in fvs {
                subs.insert(fv, Value::hole());
            }

            subs
        }

        pub fn instantiate(&self) -> value::Atom {
            let subs = self.get_vars();
            self.instantiate_with(subs)
        }
    }

    /// This enum represents a term in the prolog language. A term can be a number, a variable or
    /// an atom.
    pub enum Term {
        Number(i32),
        Variable(String),
        Atom(Atom),
    }

    impl Term {
        pub fn instantiate_with(&self, subs: HashMap<String, Value>) -> Value {
            match self {
                Term::Number(n) => Value::number(*n),
                Term::Variable(name) => subs.get(name).unwrap_or(&Value::hole()).clone(),
                Term::Atom(atom) => Value::atom(atom.instantiate_with(subs)),
            }
        }

        /// Returns the set of free variables in the term.
        pub fn free_variables(&self) -> im_rc::HashSet<String> {
            match self {
                Term::Number(_) => im_rc::HashSet::new(),
                Term::Variable(name) => im_rc::hashset![name.clone()],
                Term::Atom(atom) => atom.free_variables(),
            }
        }
    }

    /// A clause is either a fact or a rule. A fact is an atom without any arguments. A rule is an
    /// atom with a list of atoms as arguments.
    /// 
    /// # Example
    /// ```prolog
    /// parent(john, mary).
    /// parent(X, Y) :- father(X, Y).
    /// ```
    pub enum Clause {
        Fact(Atom),
        Rule(Atom, Vec<Atom>),
    }

    impl Clause {
        pub fn name(&self) -> &str {
            match self {
                Clause::Fact(atom) => &atom.name,
                Clause::Rule(head, _) => &head.name,
            }
        }

        pub fn arity(&self) -> usize {
            match self {
                Clause::Fact(atom) => atom.args.len(),
                Clause::Rule(head, _) => head.args.len(),
            }
        }

        pub fn instantiate_with(&self, subs: HashMap<String, Value>) -> value::Clause {
            match self {
                Clause::Fact(atom) => value::Clause {
                    head: atom.instantiate_with(subs),
                    body: None,
                },
                Clause::Rule(head, body) => value::Clause {
                    head: head.instantiate_with(subs.clone()),
                    body: Some(body.iter().map(|atom| atom.instantiate_with(subs.clone())).collect()),
                },
            }
        }

        pub fn free_variables(&self) -> im_rc::HashSet<String> {
            match self {
                Clause::Fact(atom) => atom.free_variables(),
                Clause::Rule(head, body) => {
                    let mut fvs = head.free_variables();
                    for atom in body {
                        fvs.extend(atom.free_variables());
                    }
                    fvs
                },
            }
        }

        pub fn instantiate(&self) -> value::Clause {
            let fvs = self.free_variables();
            let mut subs = HashMap::new();
            
            for fv in fvs {
                subs.insert(fv, Value::hole());
            }

            self.instantiate_with(subs)
        }
    }

    /// A program is a list of clauses.
    pub struct Program {
        pub clauses: Vec<Clause>,
    }
}

pub mod value {
    use std::{cell::RefCell, rc::Rc, collections::HashMap, fmt::{Formatter, Display}};

    use crate::tree;

    /// A hole is a placeholder for a value that is not yet known. It is used to represent a
    /// variable in the prolog language.
    #[derive(Clone)]
    pub struct Hole(Rc<RefCell<Option<Term>>>);

    impl Hole {
        pub fn new() -> Self {
            Self(Rc::new(RefCell::new(None)))
        }

        pub fn fill(&self, value: Term) {
            *self.0.borrow_mut() = Some(value);
        }
    }

    /// An atom is a predicate with a name and a list of arguments. The arguments are values, which
    /// can be numbers or holes.
    #[derive(Clone)]
    pub struct Atom {
        pub name: String,
        pub args: Vec<Value>,        
    }

    impl Display for Atom {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.name)?;
            if !self.args.is_empty() {
                write!(f, "(")?;
                for (i, arg) in self.args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")?;
            }
            Ok(())
        }
    }

    impl Atom {
        pub fn arity(&self) -> usize {
            self.args.len()
        }

        pub fn quote(&self, counter: &mut usize) -> Atom {
            Atom {
                name: self.name.clone(),
                args: self.args.iter().map(|value| value.quote(counter)).collect(),
            }
        }

        pub fn fork(&self) -> Self {
            let mut counter = 0;
            
            let values = self.args.iter().map(|value| value.quote(&mut counter)).collect::<Vec<_>>();
            let holes = (0..counter).map(|_| Value::hole()).collect::<Vec<_>>();
            let evaluated = values.into_iter().map(|value| value.0.eval_with(&holes)).collect::<Vec<_>>();

            Atom {
                name: self.name.clone(),
                args: evaluated,
            }
        }

        pub fn signature(&self) -> Signature {
            Signature {
                name: self.name.clone(),
                arity: self.arity(),
            }
        }

        pub fn eval_with(&self, subs: &[Value]) -> Atom {
            Atom {
                name: self.name.clone(),
                args: self.args.iter().map(|value| value.0.eval_with(subs)).collect(),
            }
        }

        pub fn unify(&self, other: &Atom) -> bool {
            if self.name != other.name || self.args.len() != other.args.len() {
                false
            } else {
                self.args.iter().zip(other.args.iter()).all(|(arg1, arg2)| arg1.0.unify(&arg2.0))
            }
        }
    }
    
    /// A value is either a number, a hole or an atom.
    #[derive(Clone)]
    pub enum Term {
        Number(i32),
        Hole(Hole),
        Variable(usize),
        Atom(Atom),
    }

        
    impl Display for Term {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &*self {
                Term::Number(n) => write!(f, "{}", n),
                Term::Hole(hole) => {
                    match &*hole.0.borrow() {
                        Some(term) => write!(f, "{}", term),
                        None => write!(f, "_"),
                    }
                }
                Term::Variable(n) => write!(f, "X{}", n),
                Term::Atom(atom) => {
                    write!(f, "{}", atom.name)?;
                    if !atom.args.is_empty() {
                        write!(f, "(")?;
                        for (i, arg) in atom.args.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", arg.0)?;
                        }
                        write!(f, ")")?;
                    }
                    Ok(())
                },
            }
        }
    }

    impl Term {
        pub fn quote(&self, counter: &mut usize) -> Value {
            match self {
                Term::Number(n) => Value::number(*n),
                Term::Hole(hole) => {
                    match hole.0.borrow().clone() {
                        Some(term) => term.quote(counter),
                        None => {
                            let n = *counter;
                            *counter += 1;
                            Value::variable(n)
                        },
                    }
                }
                Term::Variable(n) => Value::variable(*n),
                Term::Atom(atom) => Value::atom(Atom {
                    name: atom.name.clone(),
                    args: atom.args.iter().map(|value| value.quote(counter)).collect(),
                }),
            }
        }

        pub fn eval_with(&self, subs: &[Value]) -> Value {
            match self {
                Term::Hole(_) => todo!(),
                Term::Number(n) => Value::number(*n),
                Term::Variable(n) => subs[*n].clone(),
                Term::Atom(atom) => Value::atom(Atom {
                    name: atom.name.clone(),
                    args: atom.args.iter().map(|value| value.0.eval_with(subs)).collect(),
                }),
            }
        }

        pub fn unify(&self, other: &Term) -> bool {
            match (self, other) {
                (Term::Number(n1), Term::Number(n2)) => n1 == n2,
                (Term::Variable(n), Term::Variable(m)) => n == m,
                (Term::Hole(hole), other) | (other, Term::Hole(hole)) => {
                    let borrow = hole.0.borrow().clone();
                    match borrow {
                        Some(term) => term.unify(other),
                        None => {
                            hole.fill(other.clone());
                            true
                        },
                    }
                },
                (Term::Atom(atom1), Term::Atom(atom2)) => {
                    if atom1.name != atom2.name || atom1.args.len() != atom2.args.len() {
                        false
                    } else {
                        atom1.args.iter().zip(atom2.args.iter()).all(|(arg1, arg2)| arg1.0.unify(&arg2.0))
                    }
                },
                _ => false,
            }
        }
    }

    #[derive(Clone)]
    pub struct Value(Rc<Term>);

    impl Display for Value {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl Value {
        pub fn number(n: i32) -> Self {
            Self(Rc::new(Term::Number(n)))
        }

        pub fn hole() -> Self {
            Self(Rc::new(Term::Hole(Hole::new())))
        }

        pub fn atom(atom: Atom) -> Self {
            Self(Rc::new(Term::Atom(atom)))
        }

        pub fn variable(n: usize) -> Self {
            Self(Rc::new(Term::Variable(n)))
        }

        pub fn quote(&self, counter: &mut usize) -> Value {
            self.0.quote(counter)
        }
    }

    /// A signature is a predicate with a name and an arity.
    #[derive(Hash, Eq, PartialEq)]
    pub struct Signature {
        name: String,
        arity: usize,
    }

    /// A clause is either a fact or a rule. A fact is an atom without any arguments. A rule is an
    /// atom with a list of atoms as arguments.
    pub struct Clause {
        pub head: Atom,
        pub body: Option<Vec<Atom>>,
    }

    impl Clause {
        pub fn name(&self) -> &str {
            &self.head.name
        }

        pub fn arity(&self) -> usize {
            self.head.arity()
        }

        pub fn body(&self) -> Option<&[Atom]> {
            self.body.as_deref()
        }
        
    }

    /// A program is a list of clauses. It is represented as a map from signatures to clauses. This
    /// allows for fast lookup of clauses with a given signature.
    pub struct Program {
        signatures: HashMap<Signature, Vec<tree::Clause>>,
    }

    impl Program {
        pub fn new() -> Self {
            Self {
                signatures: HashMap::new(),
            }
        }

        pub fn get(&self, signature: &Signature) -> Option<&[tree::Clause]> {
            self.signatures.get(signature).map(|clauses| &**clauses)
        }

        pub fn add(&mut self, clause: tree::Clause) {
            self.signatures.entry(Signature {
                name: clause.name().to_string(),
                arity: clause.arity(),
            }).or_insert_with(Vec::new).push(clause);
        }
    }

    impl From<tree::Program> for Program {
        fn from(program: tree::Program) -> Self {
            let mut new_program = Self::new();

            for clause in program.clauses {
                new_program.add(clause);
            }

            new_program
        }
    }

    #[derive(Clone)]
    pub struct Goal(im_rc::Vector<Atom>);

    impl Goal {
        pub fn new() -> Self {
            Self(im_rc::Vector::new())
        }

        pub fn singleton(atom: Atom) -> Self {
            Self(im_rc::vector![atom])
        }

        pub fn shift(&mut self) -> Option<Atom> {
            self.0.pop_front()
        }

        pub fn preppend(&self, atom: Atom) -> Self {
            let mut new_goal = self.clone();
            new_goal.0.push_back(atom);
            new_goal
        }

        pub fn preppend_vec(&self, atoms: Vec<Atom>) -> Self {
            let mut new_goal = Goal(atoms.into());
            new_goal.0.append(self.0.clone());
            new_goal
        }
        
        pub fn fork(&self) -> (Self, Vec<Value>) {
            let mut counter = 0;
            
            let values = self.0.iter().map(|atom| atom.quote(&mut counter)).collect::<Vec<_>>();
            let holes = (0..counter).map(|_| Value::hole()).collect::<Vec<_>>();
            let evaluated = values.into_iter().map(|atom| atom.eval_with(&holes)).collect::<Vec<_>>();

            Goal(evaluated.into())
        }
    }
}

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

pub fn solve_goal(program: &value::Program, mut goal: value::Goal, holes: &HashMap<String, Value>) -> Control {
    if let Some(atom) = goal.shift() {
        let rules = program.get(&atom.signature());

        if let Some(rules) = rules {
            for rule in rules {
                let instantiated = rule.instantiate();

                if instantiated.head.unify(&atom.fork()) {
                    let goal = goal.fork().preppend_vec(instantiated.body.unwrap_or_default());
                    let control = solve_goal(program, goal, holes);

                    if control == Control::Halt {
                        return Control::Halt;
                    }
                }
            }  
        }

        Control::Continue
    } else {
        println!("");

        for (key, value) in holes {
            println!("{} = {}", key, value);
        }

        println!("\nNext? (;/.)");
        read_control() 
    }
}

pub fn solve_program(program: &value::Program, goal: tree::Atom) {
    let vars = goal.get_vars();
    let goal = value::Goal::singleton(goal.instantiate_with(vars.clone()));

    let control = solve_goal(program, goal, &vars);

    if control == Control::Continue {
        println!("\nNo.");
    }

}