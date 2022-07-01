#[derive(PartialEq, Debug)]
pub enum Data {
    Number(f64),
    Frame(usize),
}

impl Data {
    pub fn to_number(&self) -> Option<f64>{
        match self {
            Data::Number(num) => Some(*num),
            Data::Frame(_) => None,
        }
    }
}


pub struct Stack {
    stack: Vec<Data>,
}

impl Stack {
    pub fn new(capacity: usize) -> Stack {
        Stack { stack: Vec::<Data>::with_capacity(capacity) }
    }

    pub fn push(&mut self, data: Data) {
        self.stack.push(data);
    }

    pub fn pop(&mut self) -> Option<Data> {
        self.stack.pop()
    }
}

pub struct Matrix {
    pub memory: Box<[f64]>,
    width: usize,
    height: usize,
}

impl<'a> Matrix {
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
}