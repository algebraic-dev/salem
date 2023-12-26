//! This module introduces a [Puddle] that is a structure that is useful to share values between
//! terms. It is used to implement the unification algorithm.
use std::{rc::Rc, collections::VecDeque};

/// A reference to a hole in a puddle.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct HoleRef(pub usize);

/// The inner value of a hole in a puddle.
pub enum HoleInner<T> {
    Empty,
    Filled(T),
}

/// A hole is a value that is shared between terms and can be filled in later.
#[derive(Clone)]
pub struct Hole<T>(Rc<HoleInner<T>>);

/// A puddle is a collection of holes.
#[derive(Default)]
pub struct Puddle<T> {
    holes: Vec<VecDeque<Hole<T>>>,
}

impl<T: Clone> Puddle<T> {
    pub fn new() -> Self {
        Self {
            holes: Vec::new(),
        }
    }

    /// Creates a new empty hole and returns its reference.
    pub fn hole(&mut self) -> HoleRef {
        let index = self.holes.len();
        self.holes.push(vec![Hole(Rc::new(HoleInner::Empty))].into());
        HoleRef(index)
    }

    /// Sets the value of a hole.
    pub fn fill(&mut self, HoleRef(id): HoleRef, value: T) {
        self.holes[id].back_mut().unwrap().0 = Rc::new(HoleInner::Filled(value));
    }

    pub fn get(&self, HoleRef(id): HoleRef) -> Option<T> {
        match &*self.holes[id].back().unwrap().clone().0 {
            HoleInner::Empty => None,
            HoleInner::Filled(value) => Some(value.clone()),
        }
    }

    pub fn stage(&mut self) {
        for hole in self.holes.iter_mut() {
            hole.push_back(hole.back().unwrap().clone());
        }
    }

    pub fn commit(&mut self) {
        for hole in self.holes.iter_mut() {
            hole.pop_front();
        }
    }
    
    pub fn rollback(&mut self) {
        for hole in self.holes.iter_mut() {
            hole.pop_back();
        }
    }
}