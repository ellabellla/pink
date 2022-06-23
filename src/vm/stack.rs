use super::data::*;

#[derive(Clone, Copy)]
pub struct StackFrame {
    scope: usize,
    intrs: usize,
} 

#[derive(Clone)]
pub enum StackData {
    FRAME(StackFrame),
    DATA(Data),
    MATRIX(usize),
    TUPLE(Tuple),
    LIST(List),
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