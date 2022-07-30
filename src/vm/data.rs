use std::{str::Chars, iter::Peekable};

use wasm_bindgen::prelude::*;

use super::{InstrError, parse_number, Reference, VM};


#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Data {
    Reference(Reference),
    Frame(usize, usize, usize),
}


impl Data {
    pub fn to_number(&self, vm: &mut VM) -> Result<f64, InstrError>{
        match self {
            Data::Reference(reference) => Ok(reference.to_number(0, 0, vm)?),
            Data::Frame(_, _, _) => Err(InstrError::new("reached bottom of stack")),
        }
    }
}


impl Data {
    #[allow(dead_code)]
    pub fn from_str(chars: &mut Peekable<Chars>) -> Result<Self, InstrError> {
        if let Some(c) = chars.peek() {
            if *c == 'F' {
                chars.next();
                let index = parse_number(chars)?;
                if let Some(c) = chars.next() {
                    if c == ',' {
                        let index2 = parse_number(chars)?;
                        return Ok(Data::Frame(index.floor() as usize, 0, index2.floor() as usize))
                    }
                }

            } else {
                return Reference::from_str(chars).and_then(|reference| Ok(Data::Reference(reference)))
            }
        } 

        Err(InstrError::new("couldn't parse data"))
    }
}

impl ToString for Data {
    fn to_string(&self) -> String {
        match self {
            Data::Reference(reference) => reference.to_string(),
            Data::Frame(argc, _, instr_pointer) => format!("F{},{}", argc, instr_pointer),
        }
    }
}


pub struct Stack {
    stack: Vec<Data>,
    frame_index: usize,
}


impl Stack {
    pub fn new(capacity: usize) -> Stack {
        let mut stack =  Vec::<Data>::with_capacity(capacity);
        stack.push(Data::Frame(0, 0, 0));
        Stack { stack, frame_index: 0 }
    }

    pub fn push(&mut self, data: Data) {
        match data {
            Data::Reference(_) => self.stack.push(data),
            Data::Frame(args, _, instr_pointer) => {
                while self.stack.len() - self.frame_index - 1 < args {
                    self.push(Data::Reference(Reference::None));
                }
                let prev_frame_index = self.frame_index;
                self.frame_index = self.stack.len();
                self.stack.push(Data::Frame(args, prev_frame_index, instr_pointer));
            },
        }
    }

    pub fn get(&mut self, index: usize) -> Option<Data> {
        let index = self.stack.len() -1 - index;
        if index > self.frame_index {
            self.stack.get(index).map(|d| *d)
        } else {
            None
        }
    }

    pub fn pop(&mut self) -> Option<Data> {
        if matches!(self.stack.last(), Some(Data::Frame(_, _, _))) {
            None
        } else {
            self.stack.pop()
        }
    }

    pub fn peek(&self) -> Option<Data> {
        if matches!(self.stack.last(), Some(Data::Frame(_, _, _))) {
            None
        } else {
            self.stack.last().and_then(|last| Some(*last))
        }
    }

    pub fn pop_frame(&mut self) -> Option<usize>{
        while !self.stack.is_empty() && !matches!(self.stack.last(), Some(Data::Frame(_, _, _))) {
            self.stack.pop();
        }


        if self.stack.len() > 1 {
            if let Some(last) = self.stack.pop() {
                match last {
                    Data::Reference(_) => (),
                    Data::Frame(argc, prev_frame_index, instr_pointer) => {
                        for _ in 0..argc {
                            self.stack.pop();
                        }
                        self.frame_index = prev_frame_index;

                        return Some(instr_pointer)
                    },
                }
            }
        }

        None
    }

    pub fn get_arg(&mut self, index: usize) -> Option<Reference> {
        if index >= self.get_frame_argc() || index > self.frame_index || self.frame_index == 0 {
            None
        } else {
            if let Data::Reference(reference) = self.stack[self.frame_index - 1 - index] {
                Some(reference)
            } else {
                None
            }
        }
    }

    pub fn set_arg(&mut self, index: usize, reference: Reference) -> Option<Reference> {
        if !(index >= self.get_frame_argc() || index > self.frame_index || self.frame_index == 0) {
            self.stack[self.frame_index - 1 - index] = Data::Reference(reference);
            Some(reference)
        } else {
            None
        }
    }

    fn get_frame_argc(&self) -> usize {
        match self.stack[self.frame_index] {
            Data::Reference(_) => 0,
            Data::Frame(argc, _, _) => argc,
        }
    }
}

pub struct ExprStack {
    stack: Vec<Reference>,
    clear_stack: Vec<usize>,
}

impl ExprStack {
    pub fn new(capacity: usize) -> ExprStack {
        let stack =  Vec::<Reference>::with_capacity(capacity);
        let mut clear_stack =  Vec::<usize>::with_capacity(capacity);
        clear_stack.push(0);
        ExprStack { stack,  clear_stack}
    } 

    pub fn push(&mut self, reference: Reference) {
        self.stack.push(reference);
    }

    pub fn push_expr(&mut self)  {
        self.clear_stack.push(self.stack.len());
    }

    pub fn peek(&mut self) -> Option<Reference> {
        if let Some(last) = self.clear_stack.last() {
            if self.stack.len() > *last {
                self.stack.last().map(|r| r.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn pop(&mut self) -> Option<Reference> {
        if let Some(last) = self.clear_stack.last() {
            if self.stack.len() > *last {
                self.stack.pop()
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn pop_expr(&mut self) {
        if let Some(last) = self.clear_stack.last() {
            while  self.stack.len() > *last {
                self.stack.pop();
            }
            if self.clear_stack.len() > 1 {
                self.clear_stack.pop();
            }
        }
    }
}

pub struct TupleStack {
    stack: Vec<usize>,
    clear_stack: Vec<usize>,
}

impl TupleStack {
    pub fn new(capacity: usize) -> TupleStack {
        let stack =  Vec::<usize>::with_capacity(capacity);
        let mut clear_stack =  Vec::<usize>::with_capacity(capacity);
        clear_stack.push(0);
        TupleStack { stack,  clear_stack}
    } 

    pub fn push(&mut self, tuple: usize) {
        self.stack.push(tuple);
    }

    pub fn push_frame(&mut self)  {
        self.clear_stack.push(self.stack.len());
    }

    pub fn pop_frame(&mut self) -> Option<Vec<usize>> {
        if let Some(last) = self.clear_stack.last() {
            let mut out = Vec::with_capacity(self.stack.len() - *last);
            while  self.stack.len() > *last {
                if let Some(tuple) = self.stack.pop() {
                    out.push(tuple);
                }
            }
            if self.clear_stack.len() > 1 {
                self.clear_stack.pop();
            }

            Some(out)
        } else {
            None
        }
    }
}
#[wasm_bindgen]
pub struct Matrix {
    memory: Vec<f64>,
    width: usize,
    height: usize,
}

#[wasm_bindgen]
impl Matrix {
    pub fn new(width: usize, height: usize) -> Matrix {
        Matrix{memory: vec![0.0; width*height], width, height}
    }

    fn float_to_scalar(num: f64) -> f64 {
        if num == 0.0 {
            num
        } else {
            let num = num % 1.0;
            if num ==0.0 {
                1.0
            } else {
                num
            }
        }
    }

    #[wasm_bindgen(getter)]
    pub fn memory(&self) -> Vec<u8> {
        let mut mem_out = Vec::with_capacity(self.memory.len()*4);
        for i in 0..self.memory.len() {
            mem_out.push((255.0 * Matrix::float_to_scalar(self.memory[i])) as u8);
            mem_out.push((192.0 * Matrix::float_to_scalar(self.memory[i])) as u8);
            mem_out.push((203.0 * Matrix::float_to_scalar(self.memory[i])) as u8);
            mem_out.push(255);
        }

        mem_out
    }

    pub fn get(&self, x:usize, y:usize) -> Option<f64> {
        if x >= self.width || y >= self.height {
            None
        } else {
            Some(self.memory[y*self.width + x])
        }
    }

    pub fn set(&mut self, x:usize, y:usize, number:f64) -> Option<f64> {
        if x >= self.width || y >= self.height {
            None
        } else {
            self.memory[y*self.width + x] = number;
            Some(number)
        }
    }

    pub fn get_flat(&self, x:usize) -> Option<f64> {
        if x >= self.memory.len() {
            None
        } else {
            Some(self.memory[x])
        }
    }

    pub fn set_flat(&mut self, x:usize, number:f64) -> Option<f64> {
        if x >= self.memory.len() {
            None
        } else {
            self.memory[x] = number;
            Some(number)
        }
    }

    #[wasm_bindgen(getter)]
    pub fn width(&self) -> usize {
        self.width
    }

    #[wasm_bindgen(getter)]
    pub fn height(&self) -> usize {
        self.height
    }

    pub fn len(&self) -> usize {
        self.memory.len()
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::Reference;

    use super::{Matrix, Stack, Data};

    #[test]
    fn test_matrix() {
        let mut mat = Matrix::new(100, 100);

        for y in 0..mat.height {
            for x in 0..mat.width {
                mat.set(x, y, (x*y) as f64);
            }
        }

        for y in 0..mat.height {
            for x in 0..mat.width {
                assert_eq!(mat.get(x, y).unwrap(), (x*y) as f64);
            }
        }


        assert_eq!(mat.get(mat.width, 0), None);
        assert_eq!(mat.get(0, mat.height), None);
        assert_eq!(mat.get(mat.width, mat.height), None);
    }

    #[test]
    fn test_stack() {
        let mut stack = Stack::new(10);

        for i in 0..10 {
            stack.push(Data::Reference(Reference::Literal(i as f64)));
        }

        for i in (0..10).rev() {
            assert_eq!(stack.pop().unwrap(), Data::Reference(Reference::Literal(i as f64)));
        }

        assert_eq!(stack.pop(), None);
    }

    #[test]
    fn test_stack_frame() {
        let mut stack = Stack::new(20);

        assert_eq!(stack.get_frame_argc(), 0);

        stack.push(Data::Reference(Reference::Literal(1.0)));
        stack.push(Data::Reference(Reference::Literal(2.0)));

        stack.push(Data::Frame(2, 0, 0));

        assert_eq!(stack.stack.len(), 4);
        assert_eq!(stack.get_arg(0).unwrap(), Reference::Literal(2.0));
        assert_eq!(stack.get_arg(1).unwrap(), Reference::Literal(1.0));

        assert_eq!(stack.pop(), None);

        stack.push(Data::Reference(Reference::Literal(-1.0)));
        
        assert_eq!(stack.pop().unwrap(), Data::Reference(Reference::Literal(-1.0)));

        stack.push(Data::Reference(Reference::Literal(3.0)));
        stack.push(Data::Frame(3, 0, 0));

        assert_eq!(stack.stack.len(), 8);
        assert_eq!(stack.get_arg(0).unwrap(), Reference::Literal(0.0));
        assert_eq!(stack.get_arg(1).unwrap(), Reference::Literal(0.0));
        assert_eq!(stack.get_arg(2).unwrap(), Reference::Literal(3.0));

        stack.push(Data::Reference(Reference::Literal(6.0)));
        stack.push(Data::Reference(Reference::Literal(5.0)));
        stack.push(Data::Reference(Reference::Literal(4.0)));

        stack.push(Data::Frame(1, 0, 0));

        assert_eq!(stack.get_arg(0).unwrap(), Reference::Literal(4.0));

        stack.pop_frame();

        assert_eq!(stack.pop().unwrap(), Data::Reference(Reference::Literal(5.0)));

        stack.pop_frame();

        assert_eq!(stack.pop(), None);

    }
}