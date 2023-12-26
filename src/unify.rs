use crate::{value::{self, Term, Value}, puddle::Puddle};

impl value::Value {
    pub fn occur(&self, puddle: &Puddle<Value>, hole: &crate::puddle::HoleRef) -> bool {
        match &*self.0 {
            Term::Number(_) => false,
            Term::Hole(hole2) if hole == hole2 => true,
            Term::Hole(hole2) => {
                let hole_inner = puddle.get(*hole2);
                match hole_inner {
                    Some(value) => value.occur(puddle, hole),
                    None => false
                }
            }
            Term::Atom(value::Atom { args, .. }) => {
                let mut ret = false;
                for arg in args {
                    ret |= arg.occur(puddle, hole);
                }
                ret
            }
        }
    }

    pub fn unify(&self, puddle: &mut Puddle<Value>, other: &Value) -> bool {
        match (&*self.0, &*other.0) {
            (Term::Number(n1), Term::Number(n2)) => n1 == n2,
            (Term::Hole(hole), _) => {
                if other.occur(puddle, hole) {
                    panic!("occur checking")
                }

                match puddle.get(*hole) {
                    Some(term) => term.unify(puddle, other),
                    None => {
                        puddle.fill(*hole, other.clone());
                        true
                    },
                }
            },
            (_, Term::Hole(hole)) => {
                if self.occur(puddle, hole) {
                    panic!("occur checking")
                }

                match puddle.get(*hole) {
                    Some(term) => self.unify(puddle, &term.clone()),
                    None => {
                        puddle.fill(*hole, self.clone());
                        true
                    },
                }
            },
            (Term::Atom(atom1), Term::Atom(atom2)) => {
                atom1.unify(puddle, atom2)
            },
            _ => false,
        }
    }
}

impl value::Atom {
    pub fn unify(&self, puddle: &mut Puddle<Value>, other: &value::Atom) -> bool {
        if self.name != other.name || self.args.len() != other.args.len() {
            false
        } else {
            self.args.iter().zip(other.args.iter()).all(|(arg1, arg2)| arg1.unify(puddle, arg2))
        }
    }
}