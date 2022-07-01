use super::data::*;

#[derive(Clone, Copy)]
pub struct StackFrame {
    scope: usize,
    intrs: usize,
} 

#[derive(Clone, Copy)]
pub enum StackData {
    FRAME(StackFrame),
    DATA(Data),
    MATRIX(usize),
    TUPLE(usize),
    LIST(usize),
}

impl From<Data> for StackData {
    fn from(data: Data) -> Self {
        StackData::DATA(data)
    }
}

pub struct Stack {
    data: Vec<StackData>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack { data: vec![] }
    }

    pub fn push(&mut self, data: StackData) {
        self.data.push(data)
    }

    pub fn pop(&mut self) -> StackData {
        if let Some(StackData::FRAME(_)) = self.data.last() {
            StackData::DATA(Data::NOTHING)
        } else {
            if let Some(data) = self.data.pop() {
                data
            } else {
                StackData::DATA(Data::NOTHING)
            }
        }
    }

    pub fn pop_frame(&mut self) {
        while matches!(self.data.last(), Some(StackData::DATA(_))) ||
            matches!(self.data.last(), Some(StackData::LIST(_))) ||
            matches!(self.data.last(), Some(StackData::TUPLE(_))) ||
            matches!(self.data.last(), Some(StackData::MATRIX(_))) {
            self.data.pop();
        }

        if matches!(self.data.last(), Some(StackData::FRAME(_))) {
            self.data.pop();
        }
    }
}

#[cfg(test)]
mod test {
    use crate::vm::{Data, StackFrame, Instr};

    use super::{Stack, StackData};

    #[test]
    fn test_push_pop() {
        let mut stack = Stack::new();

        stack.push(StackData::MATRIX(0));
        stack.push(StackData::MATRIX(1));
        stack.push(StackData::MATRIX(2));
        stack.push(StackData::MATRIX(3));
        stack.push(StackData::MATRIX(4));

        let mut count:i32 = 4;
        while let StackData::MATRIX(num) = stack.pop() {
            assert_eq!(count as usize, num);
            count -= 1;
        }
        assert_eq!(count, -1);

        if let StackData::DATA(Data::NOTHING) = stack.pop() {

        } else{
            assert!(false);
        }
    }

    #[test]
    fn test_pop_frame() {
        let mut stack = Stack::new();

        stack.push(StackData::MATRIX(0));
        stack.push(StackData::MATRIX(1));
        stack.push(StackData::FRAME(StackFrame{ scope: 0, intrs: 0 }));
        stack.push(StackData::MATRIX(2));
        stack.push(StackData::MATRIX(3));
        stack.push(StackData::MATRIX(4));

        let mut count:i32 = 4;
        while let StackData::MATRIX(num) = stack.pop() {
            assert_eq!(count as usize, num);
            count -= 1;
        }
        assert_eq!(count, 1);

        if let StackData::DATA(Data::NOTHING) = stack.pop() {

        } else{
            assert!(false);
        }

        stack.push(StackData::MATRIX(2));
        stack.push(StackData::MATRIX(3));
        stack.push(StackData::MATRIX(4));
        stack.pop_frame();

        let mut count:i32 = 1;
        while let StackData::MATRIX(num) = stack.pop() {
            assert_eq!(count as usize, num);
            count -= 1;
        }
        assert_eq!(count, -1);
    }
    
}