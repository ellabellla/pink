use super::{instrs::*, VM, StackData};

#[derive(Clone, Copy)]
pub enum Data {
    STRING(usize),
    NUMBER(f64),
    NOTHING,
}


pub type Expr = Vec<Instr>;

impl ExprImpl for Expr {

}
trait ExprImpl {
    
}

pub type List = Vec<Expr>;

pub type Tuple = Box<[Data]>;

pub trait TupleImpl {
    fn new_tuple(size: usize, vm: &mut VM) -> Tuple;
    fn new_tuple_inline(data: Vec<Data>) -> Tuple;
}

impl TupleImpl for Tuple {
    fn new_tuple(size: usize, vm: &mut VM) -> Tuple{
        let mut data = Vec::<Data>::with_capacity(size);

        for _ in 0..size {
            if let StackData::DATA(stack_data) = vm.pop() {
                data.push(stack_data);
            } else {
                data.push(Data::NOTHING)
            }
        }

        data.into_boxed_slice()
    }

    fn new_tuple_inline(data: Vec<Data>) -> Tuple {
        data.into_boxed_slice()
    }
}

pub struct Matrix {
    pub memory: Box<[f64]>,
    size: usize,
    total_size: usize
}

impl<'a> Matrix {
    pub fn new(size: usize) -> Matrix {
        Matrix{memory: Vec::<f64>::with_capacity(size*size).into_boxed_slice(), size, total_size: size*size}
    }

    pub fn get(&self, x:usize, y:usize) -> Option<f64> {
        let index = y*self.size + x;
        if index >= self.total_size {
            None
        } else {
            Some(self.memory[index])
        }
    }

    pub fn set(&mut self, x:usize, y:usize, number:f64) {
        let index = y*self.size + x;
        if index >= self.total_size {
            return
        } else {
            self.memory[index] = number;
        }
    }
}

/*struct Block {
    memory: [u8; MATRIX_SIZE*MATRIX_SIZE]
}

impl Block {
    pub fn new() -> Block {
        Block{memory: [0;MATRIX_SIZE*MATRIX_SIZE]}
    }

    pub fn get_f64(self, x: usize, y: usize) -> Option<f64> {
        if y * MATRIX_SIZE + x + 7 >= MATRIX_SIZE * MATRIX_SIZE {
            None
        } else {
            let index = y*MATRIX_SIZE + x;
            Some(f64::from_be_bytes(self.memory[index..index+8].try_into().expect("Wrong array size for f64")))
        }
    }

    pub fn get_str(self, x: usize, y: usize, len: usize) -> Option<String> {
        if y * MATRIX_SIZE + x + len-1 >= MATRIX_SIZE * MATRIX_SIZE {
            None
        } else {
            let index = y*MATRIX_SIZE + x;
            Some(String::from_utf8_lossy(self.memory[index..index+len].into()).to_string())
        }
    }

    pub fn set_f64(mut self, x: usize, y: usize, number: f64) {
        if y * MATRIX_SIZE + x + 7 >= MATRIX_SIZE * MATRIX_SIZE {
            return;
        } else {
            let index = y*MATRIX_SIZE + x;
            let bytes = number.to_be_bytes();
            for i in 0..8 {
                self.memory[i+index] = bytes[i];
            }
        }
    }

    pub fn set_str(mut self, x: usize, y: usize, string: &str) {
        let bytes = string.as_bytes();
        if y * MATRIX_SIZE + x + bytes.len()-1 >= MATRIX_SIZE * MATRIX_SIZE {
            return;
        } else {
            let index = y*MATRIX_SIZE + x;
            let bytes = string.as_bytes();
            for i in 0..bytes.len() {
                self.memory[i+index] = bytes[i];
            }
        }

    }
    
}*/
