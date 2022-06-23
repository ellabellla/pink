use super::{Stack, Matrix, StackFrame, Tuple, StackData};

pub struct VM {
    stack: Stack,
    strings: Vec<String>,
    matrices: Vec<Matrix>,
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

    pub fn add_matrix(&mut self, matrix: Matrix) -> usize{
        self.matrices.push(matrix);
        self.matrices.len() - 1
    }

    pub fn get_matrix(&mut self, index: usize) -> Option<&mut Matrix> {
        self.matrices.get_mut(index)
    }
}