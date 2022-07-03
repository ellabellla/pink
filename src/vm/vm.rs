use super::{Stack, Data, Matrix};

#[derive(Clone, Copy)]
pub enum Reference {
    Stack,
    Bank(usize),
    Matrix(usize, usize),
    Literal(f64),
    Tuple(usize),
}

impl Reference {
    pub fn to_number(&self, index: usize, vm:  &mut VM) -> Option<f64> {
        match self {
            Reference::Stack => vm.stack.pop().and_then(|data| data.to_number()),
            Reference::Bank(index) => vm.bank.get(*index).and_then(|num|Some(*num)),
            Reference::Matrix(x, y) => vm.matrix.get(*x, *y),
            Reference::Literal(num) => Some(*num),
            Reference::Tuple(tuple_index) => {
                if let Some(tuple) = vm.tuples.get(*tuple_index) {
                    tuple.get(index).and_then(|number| Some(*number))
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Clone, Copy)]
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

    Push(Data),

    Set_Matrix(Reference, Reference, Reference),

    Frame(Reference),
    Get_Arg(Reference),
    Set_Arg(Reference, Reference),

    Get_Bank(Reference),
    Set_Bank(Reference, Reference),

    Jump(usize),
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

    pub fn conditional(a: f64, b: f64, c: f64) -> f64 {
        if a != 0.0 { b } else { c }
    }
}

pub struct VM {
    instrs: Vec<Instr>,
    stack: Stack,
    matrix: Matrix,
    bank: Box<[f64]>,
    tuples: Box<[Box<[f64]>]>,
    instr_pointer: usize,
}

impl VM {
    pub fn new(instrs: Vec<Instr>, tuples: Box<[Box<[f64]>]>, bank_size: usize, matrix_size: (usize, usize), stack_capacity: usize) -> VM {
        VM { 
            instrs: instrs, 
            tuples,
            bank: vec![0.0; bank_size].into_boxed_slice(), 
            matrix: Matrix::new(matrix_size.0, matrix_size.1), 
            stack: Stack::new(stack_capacity),
            instr_pointer:  0,
        }
    }

    pub fn run(&mut self) {
        while self.instr_pointer < self.instrs.len() {
            self.eval_instr();
        }
    }

    fn eval_instr(&mut self) {
        if let Some(instr) = self.instrs.get(self.instr_pointer) {
            let instr = instr.clone();
            self.instr_pointer+=1;
            match  instr {
                Instr::Push(data) => {
                    self.stack.push(data);
                    return;
                },
                Instr::Frame(argc) => {
                    let argc= argc.to_number(0, self);

                    if let Some(argc) = argc {
                        self.stack.push(Data::Frame(argc.floor() as usize, 0));
                    }
                    
                    return;
                },
                Instr::Set_Matrix(a, b, c) => {
                    let a = a.to_number(0, self);
                    let b = b.to_number(1, self);
                    let c = c.to_number(2, self);
                    
                    if !(a == None || b == None || c == None) {
                        let a = a.unwrap().floor() as usize;
                        let b = b.unwrap().floor() as usize;
                        let c = c.unwrap();
            
                        self.matrix.set(a, b, c);
                    }

                    return;
                },
                Instr::Set_Bank(index, number) => {
                    let index = index.to_number(0, self);
                    let number = number.to_number(1, self);
                    
                    if !(index == None || number == None) {
                        let index = index.unwrap().floor() as usize;
                        let number = number.unwrap();
            
                        if index < self.bank.len() {
                            self.bank[index] = number;
                        }
                    }

                    return;
                },
                Instr::Set_Arg(index, number) => {
                    let index = index.to_number(0, self);
                    let number = number.to_number(1, self);


                    if !(index == None || number == None) {
                        let index = index.unwrap().floor() as usize;
                        let number = number.unwrap();
            
                        if index < self.bank.len() {
                            self.bank[index] = number;
                        }
                    }

                    return;
                },
                Instr::Jump(pointer) => {
                    self.instr_pointer = pointer;

                    return;
                }
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
                    let a = a.to_number(0, self);
                    if let Some(a) = a {
                        Some(if a != 0.0 { 0.0 } else { 1.0 })
                    } else {
                        None
                    }
                },
                Instr::And (a,b) => self.eval_binary_op(a, b, &instr_ops::and),
                Instr::Or (a,b) => self.eval_binary_op(a, b, &instr_ops::or),
                Instr::Xor (a,b) => self.eval_binary_op(a, b, &instr_ops::xor),
                Instr::Conditional(a, b, c) => self.eval_trinary_op(a, b, c, &instr_ops::conditional),
                Instr::Get_Arg(a) => a.to_number(0, self).and_then(|index|
                    self.stack.get_arg(index.floor() as usize).and_then(|data| data.to_number())
                ),
                Instr::Get_Bank(a) => a.to_number(0, self).and_then(|index|
                    self.bank.get(index.floor() as usize).and_then(|number| Some(*number))
                ),
                _ => None,
            };

            if let Some(res) = res {
                self.stack.push(Data::Number(res));
            } else {
                panic!("VM failed to eval instr");
            }
        } else {
            self.instr_pointer = self.instrs.len();
        }
    }

    fn eval_binary_op(&mut self, a: Reference, b: Reference, op: &dyn Fn(f64, f64) -> f64) -> Option<f64> {
        let a = a.to_number(0, self);
        let b = b.to_number(1, self);

        if a == None || b == None {
            None
        } else {
            let a = a.unwrap();
            let b = b.unwrap();
            
            Some(op(a,b))
        }
    }

    fn eval_trinary_op(&mut self, a: Reference, b: Reference, c: Reference, op: &dyn Fn(f64, f64, f64) -> f64) -> Option<f64> {
        let a = a.to_number(0, self);
        let b = b.to_number(1, self);
        let c = c.to_number(2, self);
        
        if a == None || b == None || c == None {
            None
        } else {
            let a = a.unwrap();
            let b = b.unwrap();
            let c = c.unwrap();

            Some(op(a,b,c))
        }
    }
}


#[cfg(test)]
mod test {
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
        ];
        let mut vm = VM::new(instrs, vec![vec![0.0; 0].into_boxed_slice(); 0].into_boxed_slice(), 10, (100, 100), 10);
        vm.run();

        assert_eq!(vm.stack.pop().unwrap(), Data::Number(6.0));
    }
}