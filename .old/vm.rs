use std::collections::HashMap;

use super::{Stack, Matrix, List, Tuple, StackData};
use rand::Rng;

pub struct VM {
    stack: Stack,
    strings: Vec<String>,
    matrices: HashMap<usize, Matrix>,
    lists: HashMap<usize, List>,
    tuples: HashMap<usize, Tuple>,
}

impl VM {
    pub fn push(&mut self, data: StackData) {
        self.stack.push(data);
    }

    pub fn pop(&mut self) -> StackData {
        self.stack.pop()
    }

    pub fn pop_frame(&mut self) {
        self.stack.pop_frame()
    }

    pub fn add_matrix(&mut self, index: usize, matrix: Matrix){
        self.matrices.insert(index, matrix);
    }

    pub fn get_matrix(&mut self, index: usize) -> Option<&mut Matrix> {
        self.matrices.get_mut(&index)
    }

    pub fn add_list(&mut self, index: usize, list: List) {
        self.lists.insert(index, list);
    }

    pub fn get_list(&self, index:usize) -> Option<&List> {
        self.lists.get(&index)
    }

    pub fn remove_list(&mut self, index:usize) -> Option<List> {
        self.lists.remove(&index)
    }

    pub fn get_list_mut(&mut self, index:usize) -> Option<&mut List> {
        self.lists.get_mut(&index)
    }

    pub fn get_list_index(&self) -> usize {
        let mut index = rand::thread_rng().gen_range(usize::MIN..usize::MAX);
        while self.lists.contains_key(&index) {
            index = rand::thread_rng().gen_range(usize::MIN..usize::MAX);
        }
        index
    }

    pub fn add_tuple(&mut self, index: usize, tuple: Tuple) {
        self.tuples.insert(index, tuple);
    }

    pub fn get_tuple(&self, index: usize) -> Option<&Tuple> {
        self.tuples.get(&index)
    }

    pub fn get_tuple_mut(&mut self, index: usize) -> Option<&mut Tuple> {
        self.tuples.get_mut(&index)
    }

    pub fn get_tuple_index(&self) -> usize {
        let mut index = rand::thread_rng().gen_range(usize::MIN..usize::MAX);
        while self.tuples.contains_key(&index) {
            index = rand::thread_rng().gen_range(usize::MIN..usize::MAX);
        }
        index
    }

}