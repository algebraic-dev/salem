//! A value is something that can be unified with another value.

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::puddle::{HoleRef, Puddle};
use crate::tree;

pub struct WithEnv<'a, T>(&'a Puddle<Value>, &'a T);

#[derive(Clone, Debug)]
pub struct Atom {
    pub name: String,
    pub args: Vec<Value>,
}

impl Atom {
    pub fn with<'a>(&'a self, w: &'a Puddle<Value>) -> WithEnv<'a, Atom> {
        WithEnv(w, self)
    }

    pub fn signature(&self) -> Signature {
        Signature {
            name: self.name.clone(),
            arity: self.args.len(),
        }
    }
}

impl<'a> Display for WithEnv<'a, Atom> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.1.name)?;
        if !self.1.args.is_empty() {
            write!(f, "(")?;
            for (i, arg) in self.1.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg.0.with(self.0))?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Number(i32),
    Hole(HoleRef),
    Atom(Atom),
}

impl Term {
    fn with<'a>(&'a self, w: &'a Puddle<Value>) -> WithEnv<'a, Term> {
        WithEnv(w, self)
    }
}

impl<'a> Display for WithEnv<'a, Term> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.1 {
            Term::Number(n) => write!(f, "{}", n),
            Term::Hole(hole) => match self.0.get(*hole) {
                Some(term) => write!(f, "{}", term.with(self.0)),
                None => write!(f, "_{}", hole.0),
            },
            Term::Atom(atom) => {
                write!(f, "{}", atom.name)?;
                if !atom.args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in atom.args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg.0.with(self.0))?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Value(pub Rc<Term>);

impl Value {
    pub fn with<'a>(&'a self, w: &'a Puddle<Value>) -> WithEnv<'a, Value> {
        WithEnv(w, self)
    }
}


impl<'a> Display for WithEnv<'a, Value> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        //                              self.1.2.3.4.5.6.as_ref()
        write!(f, "{}", WithEnv(self.0, self.1.0.as_ref()))
    }
}

impl Value {
    pub fn number(n: i32) -> Self {
        Self(Rc::new(Term::Number(n)))
    }

    pub fn atom(atom: Atom) -> Self {
        Self(Rc::new(Term::Atom(atom)))
    }
    
    pub fn hole(hole: HoleRef) -> Self {
        Self(Rc::new(Term::Hole(hole)))
    }
}

// A signature is a predicate with a name and an arity.
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
        self.signatures
            .entry(Signature {
                name: clause.name().to_string(),
                arity: clause.arity(),
            })
            .or_insert_with(Vec::new)
            .push(clause);
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

impl tree::Term {
    pub fn instantiate(&self, puddle: &mut Puddle<Value>) -> Value {
        let fvs = self.free_variables();
        let mut subs = im_rc::HashMap::new();

        for fv in fvs {
            subs.insert(fv, Value::hole(puddle.hole()));
        }

        self.substitute(subs)
    }
    
    pub fn substitute(&self, env: im_rc::HashMap<String, Value>) -> Value {
        match self {
            tree::Term::Variable(name) => {
                env.get(name).unwrap().clone()
            },
            tree::Term::Number(num) => Value::number(*num),
            tree::Term::Atom(tree::Atom { name, args }) => {
                let mut avocado = vec![];
                for arg in args {
                    let val = arg.substitute(env.clone());
                    avocado.push(val)
                }
                let cheese = Atom {
                    name: name.clone(),
                    args: avocado,
                };
                Value::atom(cheese)
            },
        }
    }
}

impl tree::Clause {
    pub fn substitute(&self, subs: im_rc::HashMap<String, Value>) -> Clause {
        match self {
            tree::Clause::Fact(atom) => Clause {
                head: atom.substitute(subs),
                body: None,
            },
            tree::Clause::Rule(head, body) => Clause {
                head: head.substitute(subs.clone()),
                body: Some(body.iter().map(|atom| atom.substitute(subs.clone())).collect()),
            },
        }
    }

    pub fn instantiate(&self, puddle: &mut Puddle<Value>) -> Clause {
        let fvs = self.free_variables();
        let mut subs = im_rc::HashMap::new();
        
        for fv in fvs {
            subs.insert(fv, Value::hole(puddle.hole()));
        }

        self.substitute(subs)
    }
}

impl tree::Atom {
    pub fn substitute(&self, subs: im_rc::HashMap<String, Value>) -> Atom {
        Atom {
            name: self.name.clone(),
            args: self.args.iter().map(|term| term.substitute(subs.clone())).collect(),
        }
    }

    pub fn get_vars(&self, puddle: &mut Puddle<Value>) -> im_rc::HashMap<String, Value> {
        let fvs = self.free_variables();
        let mut subs = im_rc::HashMap::new();
        
        for fv in fvs {
            subs.insert(fv, Value::hole(puddle.hole()));
        }

        subs
    }

    pub fn instantiate(&self, puddle: &mut Puddle<Value>) -> Atom {
        let subs = self.get_vars(puddle);
        self.substitute(subs)
    }
}

#[derive(Clone)]
pub struct Goal(im_rc::Vector<Atom>);

impl<'a> Display for WithEnv<'_, Goal> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, atom) in self.1.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", atom.with(self.0))?;
        }
        Ok(())
    }
}

impl Goal {
    pub fn with<'a>(&'a self, w: &'a Puddle<Value>) -> WithEnv<'a, Goal> {
        WithEnv(w, self)
    }

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
}