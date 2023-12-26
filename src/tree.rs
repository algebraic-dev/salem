//! This module defines the abstract syntax tree for the prolog language. It is used to represent
//! the program that is being compiled.

/// This structure represents an atom in the prolog language. An atom is a predicate with a
/// name and a list of arguments. The arguments are terms, which can be numbers, variables or
/// atoms.
///
/// # Example
/// ```prolog
/// parent(john, mary)
/// ```
#[derive(Debug)]
pub struct Atom {
    pub name: String,
    pub args: Vec<Term>,
}

impl Atom {
    /// Returns the set of free variables in the atom.
    pub fn free_variables(&self) -> im_rc::HashSet<String> {
        self.args
            .iter()
            .fold(im_rc::HashSet::new(), |mut acc, term| {
                acc.extend(term.free_variables());
                acc
            })
    }
}

/// This enum represents a term in the prolog language. A term can be a number, a variable or
/// an atom.
#[derive(Debug)]
pub enum Term {
    Number(i32),
    Variable(String),
    Atom(Atom),
}

impl Term {
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
#[derive(Debug)]
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

    pub fn free_variables(&self) -> im_rc::HashSet<String> {
        match self {
            Clause::Fact(atom) => atom.free_variables(),
            Clause::Rule(head, body) => {
                let mut fvs = head.free_variables();
                for atom in body {
                    fvs.extend(atom.free_variables());
                }
                fvs
            }
        }
    }
}

/// A program is a list of clauses.
#[derive(Debug)]
pub struct Program {
    pub clauses: Vec<Clause>,
}
