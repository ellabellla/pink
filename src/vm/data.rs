use std::{str::Chars, iter::Peekable};

use super::{InstrError, parse_number};

#[allow(dead_code)]
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Data {
    Number(f64),
    Frame(usize, usize),
}

#[allow(dead_code)]
impl Data {
    pub fn to_number(&self) -> Option<f64>{
        match self {
            Data::Number(num) => Some(*num),
            Data::Frame(_, _) => None,
        }
    }
}

impl Default for Data {
    fn default() -> Self {
        Data::Number(0.0)
    }
}

impl Data {

    pub fn from_str(chars: &mut Peekable<Chars>) -> Result<Self, InstrError> {
        if let Some(c) = chars.peek() {
            if *c == 'F' {
                chars.next();
                let index = parse_number(chars)?;
                if let Some(c) = chars.next() {
                    if c == ',' {
                        let index2 = parse_number(chars)?;
                        return Ok(Data::Frame(index.floor() as usize, index2.floor() as usize))
                    }
                }

            } else {
                let index = parse_number(chars)?;
                return Ok(Data::Number(index))
            }
        } 

        Err(InstrError::new("couldn't parse data".to_string()))
    }
}

impl ToString for Data {
    fn to_string(&self) -> String {
        match self {
            Data::Number(num) => format!("{}", num),
            Data::Frame(argc, _) => format!("F{}", argc),
        }
    }
}

#[allow(dead_code)]
pub struct Stack {
    stack: Vec<Data>,
    frame_index: usize,
}

#[allow(dead_code)]
impl Stack {
    pub fn new(capacity: usize) -> Stack {
        let mut stack =  Vec::<Data>::with_capacity(capacity);
        stack.push(Data::Frame(0, 0));
        Stack { stack, frame_index: 0 }
    }

    pub fn push(&mut self, data: Data) {
        match data {
            Data::Number(_) => self.stack.push(data),
            Data::Frame(args, _) => {
                while self.stack.len() - self.frame_index - 1 < args {
                    self.push(Data::Number(0.0));
                }
                self.frame_index = self.stack.len();
                self.stack.push(Data::Frame(args, self.frame_index));
            },
        }
    }

    pub fn pop(&mut self) -> Option<Data> {
        if matches!(self.stack.last(), Some(Data::Frame(_, _))) {
            None
        } else {
            self.stack.pop()
        }
    }

    pub fn peek(&self) -> Option<Data> {
        if matches!(self.stack.last(), Some(Data::Frame(_, _))) {
            None
        } else {
            self.stack.last().and_then(|last| Some(*last))
        }
    }

    pub fn pop_frame(&mut self) {
        while !self.stack.is_empty() && !matches!(self.stack.last(), Some(Data::Frame(_, _))) {
            self.stack.pop();
        }


        if self.stack.len() > 1 {
            if let Some(last) = self.stack.pop() {
                match last {
                    Data::Number(_) => (),
                    Data::Frame(argc, _) => {
                        for _ in 0..argc {
                            self.stack.pop();
                        }
                    },
                }
            }
        }
    }

    pub fn get_arg(&mut self, index: usize) -> Option<Data> {
        if index >= self.get_frame_argc() || index > self.frame_index || self.frame_index == 0 {
            None
        } else {
            Some(self.stack[self.frame_index - 1 - index])
        }
    }

    pub fn set_arg(&mut self, index: usize, number: f64) {
        if !(index >= self.get_frame_argc() || index > self.frame_index || self.frame_index == 0) {
            self.stack[self.frame_index - 1 - index] = Data::Number(number);
        }
    }

    fn get_frame_argc(&self) -> usize {
        match self.stack[self.frame_index] {
            Data::Number(_) => 0,
            Data::Frame(argc, _) => argc,
        }
    }
}

#[allow(dead_code)]
pub struct Matrix {
    pub memory: Box<[f64]>,
    width: usize,
    height: usize,
}

#[allow(dead_code)]
impl Matrix {
    pub fn new(width: usize, height: usize) -> Matrix {
        Matrix{memory: vec![0.0; width*height].into_boxed_slice(), width, height}
    }

    pub fn get(&self, x:usize, y:usize) -> Option<f64> {
        if x >= self.width || y >= self.height {
            None
        } else {
            Some(self.memory[y*self.width + x])
        }
    }

    pub fn set(&mut self, x:usize, y:usize, number:f64) {
        if x >= self.width || y >= self.height {
            return
        } else {
            self.memory[y*self.width + x] = number;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Matrix, Stack, Data};

    #[test]
    fn test_matrix() {
        let mut mat = Matrix::new(100, 100);

        for y in 0..mat.height {
            for x in 0..mat.width {
                mat.set(x, y, (x*y) as f64)
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
            stack.push(Data::Number(i as f64));
        }

        for i in (0..10).rev() {
            assert_eq!(stack.pop().unwrap(), Data::Number(i as f64));
        }

        assert_eq!(stack.pop(), None);
    }

    #[test]
    fn test_stack_frame() {
        let mut stack = Stack::new(20);

        assert_eq!(stack.get_frame_argc(), 0);

        stack.push(Data::Number(1.0));
        stack.push(Data::Number(2.0));

        stack.push(Data::Frame(2, 0));

        assert_eq!(stack.stack.len(), 4);
        assert_eq!(stack.get_arg(0).unwrap(), Data::Number(2.0));
        assert_eq!(stack.get_arg(1).unwrap(), Data::Number(1.0));

        assert_eq!(stack.pop(), None);

        stack.push(Data::Number(-1.0));
        
        assert_eq!(stack.pop().unwrap(), Data::Number(-1.0));

        stack.push(Data::Number(3.0));
        stack.push(Data::Frame(3, 0));

        assert_eq!(stack.stack.len(), 8);
        assert_eq!(stack.get_arg(0).unwrap(), Data::Number(0.0));
        assert_eq!(stack.get_arg(1).unwrap(), Data::Number(0.0));
        assert_eq!(stack.get_arg(2).unwrap(), Data::Number(3.0));

        stack.push(Data::Number(6.0));
        stack.push(Data::Number(5.0));
        stack.push(Data::Number(4.0));

        stack.push(Data::Frame(1, 0));

        assert_eq!(stack.get_arg(0).unwrap(), Data::Number(4.0));

        stack.pop_frame();

        assert_eq!(stack.pop().unwrap(), Data::Number(5.0));

        stack.pop_frame();

        assert_eq!(stack.pop(), None);

    }
}