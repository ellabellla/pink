use std::cell::Ref;

use super::{Stack, Data, Matrix};

pub enum Reference {
    Stack,
    Matrix(usize, usize),
    Literal(f64),
}

impl Reference {
    pub fn to_number(&self, stack:  &mut Stack, matrix: &Matrix) -> Option<f64> {
        match self {
            Reference::Stack => stack.pop().and_then(|data| data.to_number()),
            Reference::Matrix(x, y) => matrix.get(*x, *y),
            Reference::Literal(num) => Some(*num),
        }
    }
}

pub enum Instr {
    Add(Reference, Reference),
    Sub(Reference, Reference),
    Multiply(Reference, Reference),
    Divide(Reference, Reference),
    
    Equal(Reference, Reference),
    Lesser(Reference, Reference),
    Greater(Reference, Reference),
    LesserEqual(Reference, Reference),
    GreaterEqual(Reference, Reference),

    Not(Reference),
    And(Reference, Reference),
    Or(Reference, Reference),
    Xor(Reference, Reference),

    Conditional(Reference, Reference, Reference),

    Push(Data)
}

mod instr_ops {
    pub fn add(a: f64, b: f64) -> f64 {
        a + b
    }

    pub fn sub(a: f64, b: f64) -> f64 {
        a - b
    }

    pub fn multiply(a: f64, b: f64) -> f64 {
        a * b
    }

    pub fn divide(a: f64, b: f64) -> f64 {
        a / b
    }

    pub fn equal(a: f64, b: f64) -> f64 {
        if a == b { 1.0 } else { 0.0 }
    }

    pub fn lesser(a: f64, b: f64) -> f64 {
        if a < b { 1.0 } else { 0.0 }
    }

    pub fn greater(a: f64, b: f64) -> f64 {    
        if a > b { 1.0 } else { 0.0 }
    }

    pub fn lesser_equal(a: f64, b: f64) -> f64 {
        if a <= b { 1.0 } else { 0.0 }
    }

    pub fn greater_equal(a: f64, b: f64) -> f64 {
        if a >= b { 1.0 } else { 0.0 }
    }

    pub fn not(a: f64, b: f64) -> f64 {
        if a != b { 1.0 } else { 0.0 }
    }

    pub fn and(a: f64, b: f64) -> f64 {
        if (a != 0.0) && (b != 0.0) { 1.0 } else { 0.0 }
    }

    pub fn or(a: f64, b: f64) -> f64 {
        if (a != 1.0) || (b != 1.0) { 1.0 } else { 0.0 }
    }

    pub fn xor(a: f64, b: f64) -> f64 {
        if or(a,b) != 0.0 && and(a,b) == 0.0  { 1.0 } else { 0.0 }
    }
}

pub struct VM {
    instrs: Vec<Instr>,
    stack: Stack,
    matrix: Matrix
}

impl VM {
    pub fn new(instrs: Vec<Instr>, matrix_size: (usize, usize), stack_capacity: usize) -> VM {
        VM { instrs: instrs, matrix: Matrix::new(matrix_size.0, matrix_size.1), stack: Stack::new(stack_capacity) }
    }

    pub fn run(&mut self) {
        while !self.instrs.is_empty() {
            self.eval_instr();
        }
    }

    fn eval_instr(&mut self) {
        if let Some(instr) = self.instrs.pop() {
            match  instr {
                Instr::Push(data) => {
                    self.stack.push(data);
                    return;
                },
                _ => (),
            }
    
            
            let res = match instr {
                Instr::Add(a, b) => self.eval_binary_op(a, b, &instr_ops::add),
                Instr::Sub (a,b) => self.eval_binary_op(a, b, &instr_ops::sub),
                Instr::Multiply (a,b) => self.eval_binary_op(a, b, &instr_ops::multiply),
                Instr::Divide (a,b) => self.eval_binary_op(a, b, &instr_ops::divide),
                Instr::Equal (a,b) => self.eval_binary_op(a, b, &instr_ops::equal),
                Instr::Lesser (a,b) => self.eval_binary_op(a, b, &instr_ops::lesser),
                Instr::Greater (a,b) => self.eval_binary_op(a, b, &instr_ops::greater),
                Instr::LesserEqual (a,b) => self.eval_binary_op(a, b, &instr_ops::lesser_equal),
                Instr::GreaterEqual (a,b) => self.eval_binary_op(a, b, &instr_ops::greater_equal),
                Instr::Not(a) => {
                    let a = a.to_number(&mut self.stack, &self.matrix);
                    if let Some(a) = a {
                        Some(if a != 0.0 { 0.0 } else { 1.0 })
                    } else {
                        None
                    }
                },
                Instr::And (a,b) => self.eval_binary_op(a, b, &instr_ops::and),
                Instr::Or (a,b) => self.eval_binary_op(a, b, &instr_ops::or),
                Instr::Xor (a,b) => self.eval_binary_op(a, b, &instr_ops::xor),
                Instr::Conditional(a, b, c) => {
                    let a = a.to_number(&mut self.stack, &self.matrix);
                    let b = b.to_number(&mut self.stack, &self.matrix);
                    let c = c.to_number(&mut self.stack, &self.matrix);
                    
                    if a == None || b == None || c == None {
                        None
                    } else {
                        let a = a.unwrap();
                        let b = b.unwrap();
                        let c = c.unwrap();

                        if a != 0.0 {
                            Some(b)
                        } else {
                            Some(c)
                        }
                    }
                },
                _ => None,
            };

            if let Some(res) = res {
                self.stack.push(Data::Number(res));
            } else {
                panic!("VM failed to eval instr");
            }
        }
    }

    fn eval_binary_op(&mut self, a: Reference, b: Reference, op: &dyn Fn(f64, f64) -> f64) -> Option<f64> {
        let a = a.to_number(&mut self.stack, &self.matrix);
        let b = b.to_number(&mut self.stack, &self.matrix);

        if a == None || b == None {
            None
        } else {
            let a = a.unwrap();
            let b = b.unwrap();
            
            Some(op(a,b))
        }
    }
}


#[cfg(test)]
mod test {
    use std::cell::Ref;

    use crate::vm::{Data, Reference};

    use super::{Instr, VM};

    #[test]
    fn test_vm() {
        let instrs = vec![
            Instr::Push(Data::Number(2.0)),
            Instr::Push(Data::Number(2.0)),
            Instr::Push(Data::Number(1.0)),
            Instr::Add(Reference::Stack, Reference::Stack), 
            Instr::Multiply(Reference::Stack, Reference::Stack), 
            Instr::Push(Data::Number(1.0)),
            Instr::Push(Data::Number(0.0)),
            Instr::Conditional(Reference::Stack, Reference::Stack, Reference::Stack)
        ].into_iter().rev().collect();
        let mut vm = VM::new(instrs, (100, 100), 10);
        vm.run();

        assert_eq!(vm.stack.pop().unwrap(), Data::Number(6.0));
    }
}