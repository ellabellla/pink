use std::{collections::{HashMap}, str::{Chars}, fmt, iter::Peekable};

use super::{Stack, Data, Matrix, CALLS, ExprStack};

#[macro_use] 
mod macros {
    #[macro_export]
    macro_rules! create_instr_error {
        ($vm:expr, $err:tt) => {
            {
                Err(InstrError::new(&format!(stringify!($err at line: {}), $vm.instr_pointer - 1)))
            }
        };
    }

    #[macro_export]
    macro_rules! create_unwrapped_instr_error {
        ($vm:expr, $err:tt) => {
            {
                InstrError::new(&format!(stringify!($err at line: {}), $vm.instr_pointer - 1))
            }
        };
    }
}

#[derive(Debug)]
pub struct InstrError {
    msg: String
}


impl InstrError {
    pub fn new(msg: &str) -> InstrError {
        InstrError { msg: msg.to_string() }
    }

    pub fn to_string(&self) -> String {
        self.msg.clone()
    }
}

impl fmt::Display for InstrError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

#[derive(Debug, PartialEq,Clone, Copy)]
pub enum Reference {
    Executable(usize, usize),
    Stack,
    StackIndex(usize),
    StackExpr,
    StackPeek,
    StackPeekExpr,
    Literal(f64),
    Global(usize),
    Argument(usize),
    Heap(usize),
    Matrix,
    Tuple(usize),
    None,
}


impl Reference {
    pub fn resolve(&self, vm:  &mut VM) -> Result<Reference, InstrError> {
        match self {
            Reference::Stack => match vm.stack.pop().ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop from stack"))? {
                Data::Reference(reference) => reference.resolve(vm),
                Data::Frame(_, _, _) => create_instr_error!(vm, "couldn't pop from stack")
            },
            Reference::StackIndex(index) => match vm.stack.get(*index).ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't get from stack"))? {
                Data::Reference(reference) => reference.resolve(vm),
                Data::Frame(_, _, _) => create_instr_error!(vm, "couldn't get from stack")
            },
            Reference::StackPeek => match vm.stack.peek().ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop from stack"))? {
                Data::Reference(reference) => reference.resolve(vm),
                Data::Frame(_, _, _) => create_instr_error!(vm, "couldn't pop from stack")
            },
            Reference::StackExpr => vm.expr_stack.pop().ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop from expr stack"))?.clone().resolve(vm),
            Reference::StackPeekExpr => vm.expr_stack.peek().ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop from expr stack"))?.clone().resolve(vm),
            Reference::Global(id) =>  {
                let reference = vm.globals.get(*id).ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop from stack"))?.clone();
                reference.resolve(vm)
            },
            Reference::Argument(id) => {
                let reference = vm.stack.get_arg(*id).ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop from stack"))?.clone();
                reference.resolve(vm)
            },
            Reference::Heap(id) => {
                let reference = vm.heap.get(id).ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop from stack"))?.clone();
                reference.resolve(vm)
            },
            Reference::Matrix => return Ok(*self),
            Reference::Tuple(_) => return Ok(*self),
            Reference::None => return Ok(*self),
            Reference::Literal(_) => return Ok(*self),
            Reference::Executable(_, _) => return Ok(*self),
        }
    }

    pub fn to_number(&self, x: usize, y: usize, vm:  &mut VM) -> Result<f64, InstrError> {
        match self {
            Reference::Stack => vm.stack.pop().ok_or(create_unwrapped_instr_error!(vm, "couldnt pop from stack")).and_then(|data| data.to_number(vm)),
            Reference::StackIndex(index) => vm.stack.get(*index).ok_or(create_unwrapped_instr_error!(vm, "couldnt get from stack")).and_then(|data| data.to_number(vm)),
            Reference::StackPeek => vm.stack.peek().ok_or(create_unwrapped_instr_error!(vm, "couldnt peep from stack")).and_then(|data| data.to_number(vm)),
            Reference::StackExpr => vm.expr_stack.pop().ok_or(create_unwrapped_instr_error!(vm, "couldnt pop from expr stack"))?.clone().to_number(x,  y, vm),
            Reference::StackPeekExpr => vm.expr_stack.peek().ok_or(create_unwrapped_instr_error!(vm, "couldnt peep from expr stack"))?.clone().to_number(x, y, vm),
            Reference::Global(index) => {
                let reference = vm.globals.get(*index).ok_or(create_unwrapped_instr_error!(vm, "couldnt get global"))?.clone();
                reference.to_number(x, y, vm)
            },
            Reference::Argument(index) => {
                let reference = vm.stack.get_arg(*index).ok_or(create_unwrapped_instr_error!(vm, "couldnt get argument index"))?.clone();
                reference.to_number(x, y, vm)
            },
            Reference::Heap(index) => {
                let reference = vm.heap.get(index).ok_or(create_unwrapped_instr_error!(vm, "couldnt get global index"))?.clone();
                reference.to_number(x, y, vm)
            },
            Reference::Matrix => vm.matrix.get(x, y).ok_or(create_unwrapped_instr_error!(vm, "couldnt get matrix index")),
            Reference::Literal(num) => Ok(*num),
            Reference::Tuple(tuple_index) => {
                let tuple = vm.tuples.get(tuple_index).ok_or(create_unwrapped_instr_error!(vm, "couldnt get tuple"))?;
                let reference = tuple.get(x).ok_or(create_unwrapped_instr_error!(vm, "couldnt get tuple index"))?.clone();
                reference.to_number(x, y, vm)
            },
            Reference::None => Ok(0.0),
            Reference::Executable(argc, instr_pointer) => vm.execute(*argc, *instr_pointer)?.to_number(x, y, vm),
        }
    }

    pub fn from_str(chars: &mut Peekable<Chars>) -> Result<Self, InstrError> {
        while let Some(c) = chars.peek() {
            if *c == ' ' {
                chars.next();
            } else {
                break;
            }
        }
        if let Some(header) = chars.peek() {
            let mut ref_type = Reference::None;
            match header {
                '_' => {
                    chars.next();
                    return Ok(Reference::None)
                },
                '@' => {
                    chars.next();
                    if let Some(header) = chars.peek() {
                        if *header == '@' {
                            chars.next();
                            if let Some(header) = chars.peek() {
                                if *header == 'E' {
                                    chars.next();
                                    return Ok(Reference::StackPeekExpr)
                                } else {
                                    return Ok(Reference::StackPeek)
                                }
                            } else {
                                return Ok(Reference::StackPeek)
                            }
                        } else if *header == 'E' {
                            chars.next();
                            return Ok(Reference::StackExpr)
                        } else if header.is_numeric() {
                            ref_type = Reference::StackIndex(0);
                        } else {
                            return Ok(Reference::Stack)
                        }
                    } else {
                        return Ok(Reference::Stack)
                    }
                },
                'G' => {
                    ref_type = Reference::Global(0);
                },
                'A' => {
                    ref_type = Reference::Argument(0);
                },
                'H' => {
                    ref_type = Reference::Heap(0);
                },
                'M' => {
                    chars.next();
                    return Ok(Reference::Matrix)
                },
                'T' => {
                    ref_type = Reference::Tuple(0);
                },
                'F' => {
                    ref_type = Reference::Executable(0,0);
                },
                _ => (),
            }

            if !matches!(ref_type, Reference::None) {
                if !matches!(ref_type, Reference::StackIndex(_)){
                    chars.next();
                }

                let index = parse_number(chars)?;

                match ref_type {
                    Reference::StackIndex(_) => return Ok(Reference::StackIndex(index.floor() as usize)),
                    Reference::Global(_) => return Ok(Reference::Global(index.floor() as usize)),
                    Reference::Argument(_) => return Ok(Reference::Argument(index.floor() as usize)),
                    Reference::Heap(_) => return Ok(Reference::Heap(index.floor() as usize)),
                    Reference::Executable(_, _) => {
                        if let Some(delimiter) = chars.next() {
                            if delimiter == ','{
                                let index2 = parse_number(chars)?;
                                return Ok(Reference::Executable(index.floor() as usize, index2.floor() as usize))
                            }
                        }
                        return Err(InstrError::new("Expected delimiter ','"))
                    },
                    Reference::Tuple(_) => return Ok(Reference::Tuple(index.floor() as usize)),
                    _ => (),
                }
            }
        }

        let number = parse_number(chars)?;

        Ok(Reference::Literal(number))
    }
}

impl ToString for Reference {
    fn to_string(&self) -> String {
        match self {
            Reference::None => String::from("_"),
            Reference::Stack => String::from("@"),
            Reference::StackIndex(index) => String::from(format!("@{}", index)),
            Reference::StackExpr => String::from("@E"),
            Reference::StackPeek => String::from("@@"),
            Reference::StackPeekExpr => String::from("@@E"),
            Reference::Global(index) => String::from(format!("G{}", index)),
            Reference::Argument(index) => String::from(format!("A{}", index)),
            Reference::Heap(index) => String::from(format!("H{}", index)),
            Reference::Matrix => String::from("M"),
            Reference::Literal(number) => String::from(format!("{}", number)),
            Reference::Tuple(index) => String::from(format!("T{}", index)),
            Reference::Executable(argc, instr_pointer) => String::from(format!("F{},{}", argc, instr_pointer)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]

pub enum Instr {
    Add(Reference, Reference),
    Subtract(Reference, Reference),
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

    Push(Reference),
    PushExpr(Reference),
    Pop(usize),
    PopExpr,
    Duplicate,
    DuplicateExpr,
    Get(usize),
    StartExpr,
    EndExpr,

    PrintLn(Reference),
    PrintLnString(String),

    GetMatrix(Reference, Reference),
    SetMatrix(Reference, Reference, Reference),
    GetFlatMatrix(Reference),
    SetFlatMatrix(Reference, Reference),
    LenMatrix(usize),
    WidthMatrix(usize),
    HeightMatrix(usize),

    PushFrame(usize, usize),
    PushInlineFrame(usize, usize),
    PopFrame(Reference),
    GetArg(usize),
    SetArg(usize, Reference),

    GetGlobal(usize),
    SetGlobal(usize, Reference),

    CreateTuple(usize, Reference),
    RemoveTuple(usize),
    GetTupleReference(Reference, Reference),
    GetTuple(usize, Reference),
    SetTuple(usize, Reference, Reference),
    LenTuple(usize),

    Jump(usize),
    JumpEqual(usize, Reference, Reference),
    JumpNotEqual(usize, Reference, Reference),
    JumpLesser(usize, Reference, Reference),
    JumpGreater(usize, Reference, Reference),
    JumpLesserOrEqual(usize, Reference, Reference),
    JumpGreaterOrEqual(usize, Reference, Reference),
    JumpNotNone(usize, Reference),

    ExecRef(Reference),

    Alloc(usize, Reference),
    Free(usize),
    Point(usize),
    SetPoint(usize, Reference),

    ReduceRange(Reference, Reference, Reference, Reference),
    Reduce(Reference, Reference),
    Into(Reference, Reference),
    ForEach(Reference, Reference),    
    ForEachRange(Reference, Reference, Reference, Reference),    

    Call(usize),
    CallStr(usize, String),
}


impl Instr {

    pub fn from_str(chars: &mut Peekable<Chars>) -> Result<Self, InstrError> {
        let mut instr = vec![];

        const INSTR_LEN: usize = 4;
        for _ in 0..INSTR_LEN {
            if let Some(char) = chars.next() {
                instr.push(char)
            } else {
                return Err(InstrError::new("instr string not long enough"))
            }
        }

        chars.next();
        let instr: String = instr.iter().collect();
        match instr.as_str() {
            "ADDX" => Ok(Instr::Add(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "SUBX" => Ok(Instr::Subtract(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "MULX" => Ok(Instr::Multiply(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "DIVX" => Ok(Instr::Divide(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "EQUL" => Ok(Instr::Equal(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "LESS" => Ok(Instr::Lesser(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "GRET" => Ok(Instr::Greater(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "LSEQ" => Ok(Instr::LesserEqual(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "GREQ" => Ok(Instr::GreaterEqual(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "NOTX" => Ok(Instr::Not(Reference::from_str(chars)?)),
            "ANDX" => Ok(Instr::And(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "ORXX" => Ok(Instr::Or(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "XORX" => Ok(Instr::Xor(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "COND" => Ok(Instr::Conditional(Reference::from_str(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "PUSH" => Ok(Instr::Push(Reference::from_str(chars)?)),
            "PSHE" => Ok(Instr::PushExpr(Reference::from_str(chars)?)),
            "POPX" => Ok(Instr::Pop(parse_usize(chars)?)),
            "POPE" => Ok(Instr::PopExpr),
            "DUPX" => Ok(Instr::Duplicate),
            "DUPE" => Ok(Instr::DuplicateExpr),
            "STEX" => Ok(Instr::StartExpr),
            "EDEX" => Ok(Instr::EndExpr),
            "GETX" => Ok(Instr::Get(parse_usize(chars)?)),
            "PRLN" => Ok(Instr::PrintLn(Reference::from_str(chars)?)),
            "PRSL" => Ok(Instr::PrintLnString(parse_string(chars)?)),
            "GETM" => Ok(Instr::GetMatrix(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "SETM" => Ok(Instr::SetMatrix(Reference::from_str(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "GTFM" => Ok(Instr::GetFlatMatrix(Reference::from_str(chars)?)),
            "STFM" => Ok(Instr::SetFlatMatrix(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "LNMT" => Ok(Instr::LenMatrix(parse_usize(chars)?)),
            "WDMT" => Ok(Instr::WidthMatrix(parse_usize(chars)?)),
            "HTMT" => Ok(Instr::HeightMatrix(parse_usize(chars)?)),
            "PSHF" => Ok(Instr::PushFrame(parse_usize(chars)?, parse_usize(chars)?)),
            "PHIF" => Ok(Instr::PushInlineFrame(parse_usize(chars)?, parse_usize(chars)?)),
            "POPF" => Ok(Instr::PopFrame(Reference::from_str(chars)?)),
            "GETA" => Ok(Instr::GetArg(parse_usize(chars)?)),
            "SETA" => Ok(Instr::SetArg(parse_usize(chars)?, Reference::from_str(chars)?)),
            "GETG" => Ok(Instr::GetGlobal(parse_usize(chars)?)),
            "SETG" => Ok(Instr::SetGlobal(parse_usize(chars)?, Reference::from_str(chars)?)),
            "CRUP" => Ok(Instr::CreateTuple(parse_usize(chars)?, Reference::from_str(chars)?)),
            "RTUP" => Ok(Instr::RemoveTuple(parse_usize(chars)?)),
            "GETT" => Ok(Instr::GetTuple(parse_usize(chars)?, Reference::from_str(chars)?)),
            "GTTR" => Ok(Instr::GetTupleReference(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "SETT" => Ok(Instr::SetTuple(parse_usize(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "LNUP" => Ok(Instr::LenTuple(parse_usize(chars)?)),
            "JMPX" => Ok(Instr::Jump(parse_number(chars)?.floor() as usize)),
            "JPEQ" => Ok(Instr::JumpEqual(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPNQ" => Ok(Instr::JumpNotEqual(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPLS" => Ok(Instr::JumpLesser(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPGR" => Ok(Instr::JumpGreater(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPLE" => Ok(Instr::JumpLesserOrEqual(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPGE" => Ok(Instr::JumpGreaterOrEqual(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPNN" => Ok(Instr::JumpNotNone(parse_usize(chars)?, Reference::from_str(chars)?)),
            "EXEC" => Ok(Instr::ExecRef(Reference::from_str(chars)?)),
            "ALOC" => Ok(Instr::Alloc(parse_usize(chars)?, Reference::from_str(chars)?)),
            "FREE" => Ok(Instr::Free(parse_usize(chars)?)),
            "PONT" => Ok(Instr::Point(parse_usize(chars)?)),
            "STPT" => Ok(Instr::SetPoint(parse_usize(chars)?, Reference::from_str(chars)?)),
            "RDCE" => Ok(Instr::Reduce(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "RDRG" => Ok(Instr::ReduceRange(Reference::from_str(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "INTO" => Ok(Instr::Into(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "FREH" => Ok(Instr::ForEach(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "FRRG" => Ok(Instr::ForEachRange(Reference::from_str(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "CALL" => Ok(Instr::Call(parse_usize(chars)?)),
            "CALS" => Ok(Instr::CallStr(parse_usize(chars)?, parse_string(chars)?)),
            _ => Err(InstrError::new("couldn't parse instr"))
        }

    }
}

impl ToString for Instr {
    fn to_string(&self) -> String {
        match self {
            Instr::Add(a, b) => format!("ADDX {} {}", a.to_string(), b.to_string()),
            Instr::Subtract(a, b) => format!("SUBX {} {}", a.to_string(), b.to_string()),
            Instr::Multiply(a,  b) => format!("MULX {} {}", a.to_string(), b.to_string()),
            Instr::Divide(a,  b) => format!("DIVX {} {}", a.to_string(), b.to_string()),
            Instr::Equal(a,  b) => format!("EQUL {} {}", a.to_string(), b.to_string()),
            Instr::Lesser(a,  b) => format!("LESS {} {}", a.to_string(), b.to_string()),
            Instr::Greater(a,  b) => format!("GRET {} {}", a.to_string(), b.to_string()),
            Instr::LesserEqual(a,  b) => format!("LSEQ {} {}", a.to_string(), b.to_string()),
            Instr::GreaterEqual(a,  b) => format!("GREQ {} {}", a.to_string(), b.to_string()),
            Instr::Not(a) => format!("NOTX {}", a.to_string()),
            Instr::And(a,  b) => format!("ANDX {} {}", a.to_string(), b.to_string()),
            Instr::Or(a,  b) => format!("ORXX {} {}", a.to_string(), b.to_string()),
            Instr::Xor(a,  b) => format!("XORX {} {}", a.to_string(), b.to_string()),
            Instr::Conditional(a,  b, c) => format!("COND {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::Push(a) => format!("PUSH {}", a.to_string()),
            Instr::PushExpr(a) => format!("PSHE {}", a.to_string()),
            Instr::Pop(a) => format!("POPX {}", a.to_string()),
            Instr::PopExpr => "POPE".to_string(),
            Instr::Duplicate => "DUPX".to_string(),
            Instr::DuplicateExpr => "DUPE".to_string(),
            Instr::StartExpr => "STEX".to_string(),
            Instr::EndExpr => "EDEX".to_string(),
            Instr::Get(a) =>  format!("GETX {}", a.to_string()),
            Instr::PrintLn(a) => format!("PRLN {}", a.to_string()),
            Instr::PrintLnString(a) => format!("PRSL \"{}\"", a.to_string()),
            Instr::GetMatrix(a,  b) => format!("GETM {} {}", a.to_string(), b.to_string()),
            Instr::SetMatrix(a,  b, c) => format!("SETM {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::GetFlatMatrix(a) => format!("GTFM {}", a.to_string()),
            Instr::SetFlatMatrix(a,  b) => format!("STFM {} {}", a.to_string(), b.to_string()),
            Instr::LenMatrix(a) => format!("LNMT {}", a.to_string()),
            Instr::WidthMatrix(a) => format!("WDMT {}", a.to_string()),
            Instr::HeightMatrix(a) => format!("HTMT {}", a.to_string()),
            Instr::PushFrame(a, b) => format!("PSHF {} {}", a.to_string(), b.to_string()),
            Instr::PushInlineFrame(a, b) => format!("PHIF {} {}", a.to_string(), b.to_string()),
            Instr::PopFrame(a) => format!("POPF {}", a.to_string()),
            Instr::GetArg(a) => format!("GETA {}", a.to_string()),
            Instr::SetArg(a,  b) => format!("SETA {} {}", a.to_string(), b.to_string()),
            Instr::GetGlobal(a) => format!("GETG {}", a.to_string()),
            Instr::SetGlobal(a,  b) => format!("SETG {} {}", a.to_string(), b.to_string()),
            Instr::CreateTuple(a,  b) => format!("CRUP {} {}", a.to_string(), b.to_string()),
            Instr::RemoveTuple(a) => format!("RTUP {}", a.to_string()),
            Instr::GetTupleReference(a,  b) => format!("GTTR {} {}", a.to_string(), b.to_string()),
            Instr::GetTuple(a,  b) => format!("GETT {} {}", a.to_string(), b.to_string()),
            Instr::SetTuple(a,  b, c) => format!("SETT {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::LenTuple(a) => format!("LNUP {}", a.to_string()),
            Instr::Jump(a) => format!("JMPX {}", a.to_string()),
            Instr::JumpEqual(a,  b, c) => format!("JPEQ {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpNotEqual(a,  b, c) => format!("JPNQ {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpLesser(a,  b, c) => format!("JPLS {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpGreater(a,  b, c) => format!("JPGR {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpLesserOrEqual(a,  b, c) => format!("JPLE {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpGreaterOrEqual(a,  b, c) => format!("JPGE {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpNotNone(a, b) => format!("JPNN {} {}", a.to_string(), b.to_string()),
            Instr::ExecRef(a) => format!("EXEC {}", a.to_string()),
            Instr::Alloc(a,  b) => format!("ALOC {} {}", a.to_string(), b.to_string()),
            Instr::Free(a) => format!("FREE {}", a.to_string()),
            Instr::Point(a) => format!("PONT {}", a.to_string()),
            Instr::SetPoint(a,  b) => format!("STPT {} {}", a.to_string(), b.to_string()),
            Instr::Reduce(a, b) => format!("RDCE {} {}", a.to_string(), b.to_string()),
            Instr::ReduceRange(a, b, c, d) => format!("RDRG {} {} {} {}", a.to_string(), b.to_string(), c.to_string(), d.to_string()),
            Instr::Into(a, b) => format!("INTO {} {}", a.to_string(), b.to_string()),
            Instr::ForEach(a, b) => format!("FREH {} {}", a.to_string(), b.to_string()),
            Instr::ForEachRange(a, b, c, d) => format!("FRRG {} {} {} {}", a.to_string(), b.to_string(), c.to_string(), d.to_string()),
            Instr::Call(a) => format!("CALL {}", a.to_string()),
            Instr::CallStr(a, b) => format!("CALS {} \"{}\"", a.to_string(), b.to_string()),
        }
    }
}

pub fn parse_number(chars: &mut Peekable<Chars>) -> Result<f64, InstrError>{
    while let Some(c) = chars.peek() {
        if *c == ' ' {
            chars.next();
        } else {
            break;
        }
    }
    let mut data = Vec::new();

    let mut negative = false;
    if let Some(c)  = chars.peek() {
        if *c == '-' {
            chars.next();
            negative = true;
        }
    }

    let mut found_dot  = false;

    while let Some(character) = chars.peek() {
        if !found_dot && *character == '.' {
            found_dot = true;
            data.push(*character)
        } else if !character.is_digit(10) {
            break;
        } else {
            data.push(*character)
        }
        chars.next();
    }

    if data.len() == 0 {
        return Err(InstrError::new("Couldn't parse reference, no number was given"))
    }

    let string: String = data.iter().collect();
    match string.parse::<f64>() {
        Ok(number) => {
            if negative {
                Ok(-number)
            } else {
                Ok(number)
            }
        },
        Err(_) => {
            return Err(InstrError::new("Couldn't parse reference, invalid number"))
        }
    }
}

pub fn parse_usize(chars: &mut Peekable<Chars>) -> Result<usize, InstrError>{
    while let Some(c) = chars.peek() {
        if *c == ' ' {
            chars.next();
        } else {
            break;
        }
    }
    let mut data = Vec::new();

    while let Some(character) = chars.peek() {
        if !character.is_digit(10) {
            break;
        } else {
            data.push(*character)
        }
        chars.next();
    }

    if data.len() == 0 {
        return Err(InstrError::new("Couldn't parse reference, no number was given"))
    }

    let string: String = data.iter().collect();
    match string.parse::<usize>() {
        Ok(number) => {
            Ok(number)
        },
        Err(_) => {
            return Err(InstrError::new("Couldn't parse reference, invalid number"))
        }
    }
}

pub fn parse_string(chars: &mut Peekable<Chars>) -> Result<String, InstrError>{
    while let Some(c) = chars.peek() {
        if *c == ' ' {
            chars.next();
        } else {
            break;
        }
    }
    let mut data = Vec::new();

    if let Some(c) = chars.peek() {
        if *c == '"' {
            chars.next();
            while let Some(c) = chars.peek() {
                if *c == '"' {
                    chars.next();
                    break;
                } else {
                    data.push(*c);
                    chars.next();
                }
            }
            return Ok(data.iter().collect())
        }
    } 

    Err(InstrError::new( "couldn't parse string"))
}

mod instr_ops {
    use crate::vm::{Data};

    use super::{VM, Reference, InstrError};
    
    pub fn add(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(a + b))
    }

    pub fn sub(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(a - b))
    }

    pub fn multiply(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(a * b))
    }

    pub fn divide(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(a / b))
    }

    pub fn equal(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if a == b { 1.0 } else { 0.0 }))
    }

    pub fn lesser(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if a < b { 1.0 } else { 0.0 }))
    }

    pub fn greater(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{    
        Ok(Some(if a > b { 1.0 } else { 0.0 }))
    }

    pub fn lesser_equal(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if a <= b { 1.0 } else { 0.0 }))
    }

    pub fn greater_equal(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if a >= b { 1.0 } else { 0.0 }))
    }

    pub fn not(a: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if a == 0.0 { 1.0 } else { 0.0 }))
    }

    pub fn and(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if (a != 0.0) && (b != 0.0) { 1.0 } else { 0.0 }))
    }

    pub fn or(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if (a != 1.0) || (b != 1.0) { 1.0 } else { 0.0 }))
    }

    pub fn xor(a: f64, b: f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        let or = or(a,b,vm)?.ok_or(create_unwrapped_instr_error!(vm, "failed to or values"))?;
        let and = and(a,b,vm)?.ok_or(create_unwrapped_instr_error!(vm, "failed to or values"))?;
        if or != 0.0 && and == 0.0 { Ok(Some(1.0)) } else { Ok(Some(0.0)) }
    }

    pub fn conditional(a: f64, b: f64, c: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if a != 0.0 { b } else { c }))
    }

    pub fn get(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(Data::Reference(reference)) = vm.stack.get(a) {
            vm.expr_stack.push(reference);
            Ok(None)
        } else {
            create_instr_error!(vm, "could not get value in matrix")
            
        }
    }

    pub fn println(a: f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        vm.extern_println.println_num(a);
        Ok(None)
    }

    pub fn get_matrix(b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(number) = vm.matrix.get(b.floor() as usize, c.floor() as usize) {
            Ok(Some(number))
        } else{
            create_instr_error!(vm, "could not get value in matrix")
        }
    }
    
    pub fn set_matrix(b:  f64, c:  f64, d:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(number) = vm.matrix.set(b.floor() as usize, c.floor() as usize, d) {
            Ok(Some(number))
        } else{
            create_instr_error!(vm, "could not get value in matrix")
        }
    }   
    pub fn get_flat_matrix(b:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(number) = vm.matrix.get_flat(b.floor() as usize) {
            Ok(Some(number))
        } else{
            create_instr_error!(vm, "could not get value in matrix")
        }

    }
    
    pub fn set_flat_matrix(b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(number) = vm.matrix.set_flat(b.floor() as usize, c) {
            Ok(Some(number))
        } else{
            create_instr_error!(vm, "could not get value in matrix")
        }
    }   

    pub fn len_matrix(_a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(vm.matrix.len() as f64))
    }

    pub fn width_matrix(_a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(vm.matrix.width() as f64))
    }

    pub fn height_matrix(_a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(vm.matrix.height() as f64))
    }
    pub fn push_frame(a: usize, b: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        vm.stack.push(Data::Frame(a, 0, vm.instr_pointer+1));
        vm.instr_pointer = b;
        Ok(None)
    }
    pub fn push_inline_frame(a: usize, b: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        vm.stack.push(Data::Frame(a, 0, b));
        Ok(None)
    }

    pub fn create_tuple(a: usize, b:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if vm.tuples.contains_key(&a) {
            Ok(None)
        } else {
            vm.tuples.insert(a, vec![Reference::None; b.floor() as usize]);
            Ok(None)
        }
    }
    pub fn remove_tuple(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(_) = vm.tuples.remove(&a) {
            Ok(None)
        } else {
            create_instr_error!(vm, "couldn't remove tuple")
        }
    }
    pub fn len_tuple(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(tuples) = vm.tuples.get(&a) {
            Ok(Some(tuples.len() as f64))
        } else {
            create_instr_error!(vm, "could not get tuple")
        }
    }
    pub fn jump(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        vm.instr_pointer = a;
        Ok(None)
    }
    pub fn jump_equal(a: usize, b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Ok(Some(res)) = equal(b, c, vm) {
            if res != 0.0 {
                vm.instr_pointer = a;
            }
        }

        Ok(None)
    }
    pub fn jump_not_equal(a: usize, b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Ok(Some(res)) = equal(b, c, vm) {
            if res == 0.0 {
                vm.instr_pointer = a;
            }
        }

        Ok(None)
    }
    pub fn jump_lesser(a: usize, b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Ok(Some(res)) = lesser(b, c, vm) {
            if res != 0.0 {
                vm.instr_pointer = a;
            }
        }

        Ok(None)
    }
    pub fn jump_greater(a: usize, b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Ok(Some(res)) = greater(b, c, vm) {
            if res != 0.0 {
                vm.instr_pointer = a;
            }
        }

        Ok(None)

    }
    pub fn jump_lesser_equal(a: usize, b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Ok(Some(res)) = lesser_equal(b, c, vm) {
            if res != 0.0 {
                vm.instr_pointer = a;
            }
        }

        Ok(None)
    }
    pub fn jump_greater_equal(a: usize, b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Ok(Some(res)) = greater_equal(b, c, vm) {
            if res != 0.0 {
                vm.instr_pointer = a;
            }
        }

        Ok(None)
    }
    pub fn free(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(_) = vm.heap.remove(&a) {
            Ok(None)
        } else {
            create_instr_error!(vm, "couldn't remove from heap")
        }

    }
}

pub trait ExternPrintLn {
    fn println_num(&self, msg: f64);
    fn println_str(&self, msg: &str);
}

pub struct VM {
    pub globals: Vec<Reference>,
    matrix: Box<Matrix>,
    tuples: HashMap<usize, Vec<Reference>>,
    pub stack: Stack,
    pub expr_stack: ExprStack,
    instrs: Vec<Instr>,
    pub instr_pointer: usize,
    pub heap: HashMap<usize, Reference>,
    pub extern_println: Box<dyn ExternPrintLn>,
}

impl VM {

    pub fn new(matrix_size: (usize, usize), globals_capacity: usize, stack_capacity: usize, instrs: Vec<Instr>, instr_pointer: usize, extern_println: Box<dyn ExternPrintLn>)-> VM {
        VM { 
            globals: vec![Reference::None; globals_capacity], 
            matrix: Box::new(Matrix::new(matrix_size.0, matrix_size.1)),
            tuples: HashMap::new(), 
            stack: Stack::new(stack_capacity), 
            expr_stack: ExprStack::new(stack_capacity*2),
            instrs: instrs, 
            instr_pointer,
            heap: HashMap::new(),
            extern_println,
        }
    }

    pub fn get_matrix(self) -> Box<Matrix>{
        self.matrix
    }


    pub fn run(&mut self) -> Result<(), InstrError> {
        while self.instr_pointer < self.instrs.len() {
            self.eval_instr()?;
        }
        
        Ok(())
    }

    pub fn eval_instr(&mut self) -> Result<(), InstrError> {
        if let Some(instr) = self.instrs.get(self.instr_pointer) {
            self.instr_pointer+=1;
            let res = match instr.clone() {
                Instr::Add(a, b) => self.eval_binary_op(a, b, &instr_ops::add),
                Instr::Subtract (a,b) => self.eval_binary_op(a, b, &instr_ops::sub),
                Instr::Multiply (a,b) => self.eval_binary_op(a, b, &instr_ops::multiply),
                Instr::Divide (a,b) => self.eval_binary_op(a, b, &instr_ops::divide),
                Instr::Equal (a,b) => self.eval_binary_op(a, b, &instr_ops::equal),
                Instr::Lesser (a,b) => self.eval_binary_op(a, b, &instr_ops::lesser),
                Instr::Greater (a,b) => self.eval_binary_op(a, b, &instr_ops::greater),
                Instr::LesserEqual (a,b) => self.eval_binary_op(a, b, &instr_ops::lesser_equal),
                Instr::GreaterEqual (a,b) => self.eval_binary_op(a, b, &instr_ops::greater_equal),
                Instr::Not(a) => self.eval_unary_op(a, &instr_ops::not),
                Instr::And (a,b) => self.eval_binary_op(a, b, &instr_ops::and),
                Instr::Or (a,b) => self.eval_binary_op(a, b, &instr_ops::or),
                Instr::Xor (a,b) => self.eval_binary_op(a, b, &instr_ops::xor),
                Instr::Conditional(a, b, c) => self.eval_trinary_op(a, b, c, &instr_ops::conditional),
                Instr::Push(a) => {
                    let reference = a.resolve(self)?;
                    self.stack.push(Data::Reference(reference));
                    Ok(None)
                },
                Instr::PushExpr(a) => {
                    let reference = a.resolve(self)?;
                    self.expr_stack.push(reference);
                    Ok(None)
                },
                Instr::Pop(a) => {
                    for _ in 0..a {
                        self.stack.pop();
                    }
                    Ok(None)
                },
                Instr::PopExpr => {self.expr_stack.pop(); Ok(None)},
                Instr::Duplicate => {
                    if let Some(data) = self.stack.peek() {
                        self.stack.push(data);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "stack is empty")
                    }
                },
                Instr::DuplicateExpr => {
                    if let Some(reference) = self.expr_stack.peek() {
                        self.expr_stack.push(reference);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "stack is empty")
                    }
                },
                Instr::StartExpr => {
                    self.expr_stack.push_expr();
                    Ok(None)
                }
                Instr::EndExpr => {
                    self.expr_stack.pop_expr();
                    Ok(None)
                }
                Instr::Get(a) => self.eval_unary_op_fixed(a, &instr_ops::get),
                Instr::PrintLn(a) => self.eval_unary_op(a, &instr_ops::println),
                Instr::PrintLnString(a) => {self.extern_println.println_str(&a); Ok(None)},
                Instr::GetMatrix(a, b) => self.eval_binary_op(a, b, &instr_ops::get_matrix),
                Instr::SetMatrix(a, b, c) => self.eval_trinary_op(a, b, c, &instr_ops::set_matrix),
                Instr::GetFlatMatrix(a) => self.eval_unary_op(a, &instr_ops::get_flat_matrix),
                Instr::SetFlatMatrix(a, b) => self.eval_binary_op(a, b, &instr_ops::set_flat_matrix),
                Instr::LenMatrix(a) => self.eval_unary_op_fixed(a, &instr_ops::len_matrix),
                Instr::WidthMatrix(a) => self.eval_unary_op_fixed(a, &instr_ops::width_matrix),
                Instr::HeightMatrix(a) => self.eval_unary_op_fixed(a, &instr_ops::height_matrix),
                Instr::PushFrame(a, b) => self.eval_binary_op_fixed2(a, b, &instr_ops::push_frame),
                Instr::PushInlineFrame(a, b) => self.eval_binary_op_fixed2(a, b, &instr_ops::push_inline_frame),
                Instr::PopFrame(a) => {
                    let reference = a.resolve(self)?;
                    if let Some(instr_pointer) = self.stack.pop_frame() {
                        self.instr_pointer = instr_pointer;
                        self.expr_stack.push(reference);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "couldn't pop frame from stack")
                    }
                },
                Instr::GetArg(a) => {
                    if let Some(reference) = self.stack.get_arg(a) {
                        self.expr_stack.push(reference);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "args index out of bounds")
                    }
                },
                Instr::SetArg(a, b) => {
                    let b = b.resolve(self)?;
                    if let Some(_) = self.stack.set_arg(a, b) {
                        Ok(None)
                    } else {
                        create_instr_error!(self, "args index out of bounds")
                    }
                },
                Instr::GetGlobal(a) => {
                    if let Some(reference) = self.globals.get(a) {
                        self.expr_stack.push(*reference);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "args index out of bounds")
                    }
                },
                Instr::SetGlobal(a, b) => {
                    let b = b.resolve(self)?;
                    if let Some(reference) = self.globals.get_mut(a) {
                        *reference = b;
                        Ok(None)
                    } else {
                        create_instr_error!(self, "args index out of bounds")
                    }
                },
                Instr::CreateTuple(a, b) => self.eval_binary_op_fixed1(a, b, &instr_ops::create_tuple),
                Instr::RemoveTuple(a) => self.eval_unary_op_fixed(a, &instr_ops::remove_tuple),
                Instr::GetTuple(a, b) => {
                    let b = b.to_number(0, 0, self)?;
                    if let Some(references) = self.tuples.get(&a) {
                        if let Some(reference) = references.get(b.floor() as usize) {
                            self.expr_stack.push(*reference);
                            Ok(None)
                        } else {
                            create_instr_error!(self, "tuple index out of bounds")
                        }
                    } else {
                        create_instr_error!(self, "args index out of bounds")
                    }
                },
                Instr::GetTupleReference(a, b) => {
                    if let Reference::Tuple(id) = self.eval_tuple_or_matrix(a) {
                        let b = b.to_number(0, 0, self)?;
                        if let Some(references) = self.tuples.get(&id) {
                            if let Some(reference) = references.get(b.floor() as usize) {
                                println!("{:?}", reference);
                                self.expr_stack.push(*reference);
                                Ok(None)
                            } else {
                                create_instr_error!(self, "tuple index out of bounds")
                            }
                        } else {
                            create_instr_error!(self, "args index out of bounds")
                        }
                    } else {
                        create_instr_error!(self, "first reference must be a tuple")
                    }
                },
                Instr::SetTuple(a, b, c) => {
                    let b = b.to_number(0, 0, self)?;
                    let c = c.resolve(self)?;
                    if let Some(references) = self.tuples.get_mut(&a) {
                        if let Some(reference) = references.get_mut(b.floor() as usize) {
                            *reference = c;
                            Ok(None)
                        } else {
                            create_instr_error!(self, "tuple index out of bounds")
                        }
                    } else {
                        create_instr_error!(self, "args index out of bounds")
                    }
                },
                Instr::LenTuple(a) => self.eval_unary_op_fixed(a, &instr_ops::len_tuple),
                Instr::Jump(a) => self.eval_unary_op_fixed(a, &instr_ops::jump),
                Instr::JumpEqual(a, b, c) => self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_equal),
                Instr::JumpNotEqual(a, b, c) => self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_not_equal),
                Instr::JumpLesser(a, b, c) => self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_lesser),
                Instr::JumpGreater(a, b, c) =>  self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_greater),
                Instr::JumpLesserOrEqual(a, b, c) =>  self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_lesser_equal),
                Instr::JumpGreaterOrEqual(a, b, c) =>  self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_greater_equal),
                Instr::JumpNotNone(a, b) => {
                    let reference = match b {
                        Reference::Executable(argc, instr_pointer) => self.execute(argc, instr_pointer)?,
                        Reference::Stack => {
                            if let Some(Data::Reference(reference)) = self.stack.peek() {
                               reference
                            } else {
                                Reference::None
                            }
                        },
                        Reference::StackIndex(index ) => {
                            if let Some(Data::Reference(reference)) = self.stack.get(index) {
                               reference
                            } else {
                                Reference::None
                            }
                        },
                        Reference::Literal(_) => b,
                        Reference::Global(id) => {
                            if let Some(reference) = self.globals.get(id) {
                                *reference
                            } else {
                                Reference::None
                            }
                        },
                        Reference::Argument(id) => {
                            if let Some(reference) = self.stack.get_arg(id) {
                                reference
                            } else {
                                Reference::None
                            }
                        },
                        Reference::Heap(id) => {
                            if let Some(reference) = self.heap.get(&id) {
                                *reference
                            } else {
                                Reference::None
                            }
                        },
                        Reference::Matrix => b,
                        Reference::Tuple(_) => b,
                        Reference::None => b,
                        Reference::StackPeek => {
                            if let Some(Data::Reference(reference)) = self.stack.peek() {
                                reference
                             } else {
                                 Reference::None
                             }
                        },
                        Reference::StackExpr => {
                            if let Some(reference) = self.expr_stack.peek() {
                                reference
                             } else {
                                 Reference::None
                             }
                        },
                        Reference::StackPeekExpr => {
                            if let Some(reference) = self.expr_stack.peek() {
                                reference
                             } else {
                                 Reference::None
                             }
                        },
                    };

                    if !matches!(reference, Reference::None) {
                        self.instr_pointer = a;
                    } 
                    Ok(None)
                }
                Instr::ExecRef(a) => {
                    let a = a.resolve(self)?;
                    if let Reference::Executable(argc, instr_pointer) = a{
                        let reference = self.execute(argc, instr_pointer)?;
                        self.expr_stack.push(reference);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "reference mut be executable")
                    }
                },
                Instr::Alloc(a, b) => {
                    if self.heap.contains_key(&a) {
                        Ok(None)
                    } else {
                        self.heap.insert(a, b);
                        Ok(None)
                    }
                },
                Instr::Free(a) => self.eval_unary_op_fixed(a, &instr_ops::free),
                Instr::Point(a) => {
                    if let Some(reference) = self.heap.get(&a) {
                        self.expr_stack.push(*reference);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "couldn't point to heap")
                    }
                },
                Instr::SetPoint(a, b) => {
                    if let Some(reference) = self.heap.get_mut(&a) {
                        *reference = b;
                        Ok(None)
                    } else {
                        create_instr_error!(self, "couldn't set data on heap")
                    }
                },
                Instr::Reduce(a, b) => {
                    if let Reference::Executable(argc, instr_pointer) = a {
                        match self.eval_tuple_or_matrix(b) {
                            Reference::Matrix => {
                                let (width, height) =  {
                                    (self.matrix.width(), self.matrix.height())
                                };

                                let mut acc = 0.0;

                                for y in 0..height {
                                    for x in 0..width {
                                        for _ in 0..argc {
                                            self.stack.push(Data::Reference(Reference::None));
                                        }

                                        let num = self.matrix.get(x, y).unwrap();
                                        let reference = self.execute_inline(argc, instr_pointer, &mut|vm| {
                                            vm.stack.push(Data::Reference(Reference::Literal(y as f64)));
                                            vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                                            vm.stack.push(Data::Reference(Reference::Literal(acc)));
                                            vm.stack.push(Data::Reference(Reference::Literal(num)));
                                        })?;

                                        let num = reference.to_number(0, 0, self)?;

                                        acc += num;
                                    }
                                }

                                self.expr_stack.push(Reference::Literal(acc));
                                Ok(None)
                            },
                            Reference::Tuple(id) => {
                                if !self.tuples.contains_key(&id) {
                                    create_instr_error!(self, "invalid index of matrix")
                                } else {
                                    let len =  {
                                        let tuple = self.tuples.get(&id).unwrap();
                                        tuple.len()
                                    };

                                    let mut acc = 0.0;

                                    for x in 0..len {
                                        for _ in 0..argc {
                                            self.stack.push(Data::Reference(Reference::None));
                                        }

                                        let reference = self.tuples.get(&id).unwrap().get(x).unwrap().clone();
                                        let reference = self.execute_inline(argc, instr_pointer, &mut|vm| {
                                            vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                                            vm.stack.push(Data::Reference(Reference::Literal(acc)));
                                            vm.stack.push(Data::Reference(reference));
                                        })?;

                                        let num = reference.to_number(0, 0, self)?;

                                        acc += num;
                                    }

                                    self.expr_stack.push(Reference::Literal(acc));
                                    Ok(None)
                                }
                            },
                            _ => {
                                create_instr_error!(self, "second ref must be a tuple or matrix")
                            }
                        }
                    } else {
                        create_instr_error!(self, "first ref must be a executable")
                    }
                },
                Instr::ReduceRange(a, b, c, d) => {
                    if let Reference::Executable(argc, instr_pointer) = a {
                        let b = b.to_number(0, 0, self)?;
                        let c = c.to_number(0, 0, self)?;
                        let d = d.to_number(0, 0, self)?;

                        let b = b.floor() as usize;
                        let c = c.floor() as usize;
                        let d = d.floor() as usize;

                        let mut acc = 0.0;
                        for x in (b..c).step_by(d) {
                            for _ in 0..argc {
                                self.stack.push(Data::Reference(Reference::None));
                            }

                            let reference = self.execute_inline(argc, instr_pointer, &mut|vm| {
                                vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                                vm.stack.push(Data::Reference(Reference::Literal(acc)));
                                vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                            })?;

                            let num = reference.to_number(0, 0, self)?;

                            acc += num;
                        }

                        self.expr_stack.push(Reference::Literal(acc));
                        Ok(None)
                    } else {
                        create_instr_error!(self, "first ref must be a executable")
                    }
                },
                Instr::Into(a, b) => {
                    if let Reference::Executable(argc, instr_pointer) = a {
                        match self.eval_tuple_or_matrix(b) {
                            Reference::Matrix => {
                                let (width, height) =  {
                                    (self.matrix.width(), self.matrix.height())
                                };


                                for y in 0..height {
                                    for x in 0..width {
                                        for _ in 0..argc {
                                            self.stack.push(Data::Reference(Reference::None));
                                        }
                                        
                                        let num = self.matrix.get(x, y).unwrap();
                                        let reference = self.execute_inline(argc, instr_pointer, &mut|vm| {
                                            vm.stack.push(Data::Reference(Reference::Literal(y as f64)));
                                            vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                                            vm.stack.push(Data::Reference(Reference::Literal(num)));
                                        })?;

                                        let num = reference.to_number(0, 0, self)?;
                                        self.matrix.set(x, y, num);
                                    }
                                }

                                self.expr_stack.push(Reference::Matrix);
                                Ok(None)
                            },
                            Reference::Tuple(id) => {
                                if !self.tuples.contains_key(&id) {
                                    create_instr_error!(self, "invalid index of matrix")
                                } else {
                                    let len =  {
                                        let tuple = self.tuples.get(&id).unwrap();
                                        tuple.len()
                                    };

                                    for x in 0..len {
                                        for _ in 0..argc {
                                            self.stack.push(Data::Reference(Reference::None));
                                        }

                                        let reference = self.tuples.get(&id).unwrap().get(x).unwrap().clone();
                                        let reference = self.execute_inline(argc, instr_pointer, &mut|vm| {
                                            vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                                            vm.stack.push(Data::Reference(reference));
                                        })?;
                                        
                                        let reference = reference.resolve(self)?;

                                        *self.tuples.get_mut(&id).unwrap().get_mut(x).unwrap() = reference;
                                    }

                                    self.expr_stack.push(Reference::Tuple(id));
                                    Ok(None)
                                }
                            },
                            _ => {
                                create_instr_error!(self, "second ref must be a tuple or matrix")
                            }
                        }
                    } else {
                        create_instr_error!(self, "first ref must be a executable")
                    }
                },
                Instr::ForEach(a, b) => {
                    if let Reference::Executable(argc, instr_pointer) = a {
                        match self.eval_tuple_or_matrix(b) {
                            Reference::Matrix => {
                                let (width, height) =  {
                                    (self.matrix.width(), self.matrix.height())
                                };


                                for y in 0..height {
                                    for x in 0..width {
                                        for _ in 0..argc {
                                            self.stack.push(Data::Reference(Reference::None));
                                        }

                                        let num = self.matrix.get(x, y).unwrap();
                                        self.execute_inline(argc, instr_pointer, &mut|vm| {
                                            vm.stack.push(Data::Reference(Reference::Literal(y as f64)));
                                            vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                                            vm.stack.push(Data::Reference(Reference::Literal(num)));
                                        })?;
                                    }
                                }

                                self.expr_stack.push(Reference::Matrix);
                                Ok(None)
                            },
                            Reference::Tuple(id) => {
                                if !self.tuples.contains_key(&id) {
                                    create_instr_error!(self, "invalid index of matrix")
                                } else {
                                    let len =  {
                                        let tuple = self.tuples.get(&id).unwrap();
                                        tuple.len()
                                    };

                                    for x in 0..len {
                                        for _ in 0..argc {
                                            self.stack.push(Data::Reference(Reference::None));
                                        }

                                        let reference = self.tuples.get(&id).unwrap().get(x).unwrap().clone();
                                        self.execute_inline(argc, instr_pointer, &mut|vm| {
                                            vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                                            vm.stack.push(Data::Reference(reference));
                                        })?;
                                    }

                                    self.expr_stack.push(Reference::Tuple(id));
                                    Ok(None)
                                }
                            },
                            _ => {
                                create_instr_error!(self, "second ref must be a tuple or matrix")
                            }
                        }
                    } else {
                        create_instr_error!(self, "first ref must be a executable")
                    }
                },
                Instr::ForEachRange(a, b, c, d) => {
                    if let Reference::Executable(argc, instr_pointer) = a {
                        let mut last_reference = Reference::None;

                        let b = b.to_number(0, 0, self)?;
                        let c = c.to_number(0, 0, self)?;
                        let d = d.to_number(0, 0, self)?;

                        let b = b.floor() as usize;
                        let c = c.floor() as usize;
                        let d = d.floor() as usize;

                        for x in (b..c).step_by(d) {
                            for _ in 0..argc {
                                self.stack.push(Data::Reference(Reference::None));
                            }

                            last_reference = self.execute_inline(argc, instr_pointer, &mut|vm| {
                                vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                                vm.stack.push(Data::Reference(Reference::Literal(x as f64)));
                            })?;

                            if x < c - 1 {
                                if matches!(last_reference, Reference::Stack) || matches!(last_reference, Reference::StackPeek) {
                                    self.stack.pop();
                                }
                            }
                        }

                        self.expr_stack.push(last_reference);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "first ref must be a executable")
                    }
                },
                Instr::Call(a) => {
                    if let Some(call) = CALLS.get(a) {
                        let reference = call.call(self)?;
                        self.expr_stack.push(reference);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "couldn't resolve call")
                    }
                },
                Instr::CallStr(a, b) => {
                    if let Some(call) = CALLS.get(a) {
                        let reference = call.call_str( &b, self)?;
                        self.expr_stack.push(reference);
                        Ok(None)
                    } else {
                        create_instr_error!(self, "couldn't resolve call")
                    }
                }
            };

            match res {
                Ok(value) => {
                    if let Some(value) = value {
                        self.expr_stack.push(Reference::Literal(value));
                    }
                    Ok(())
                },
                Err(err) => Err(err),
            }
        } else {
            create_instr_error!(self, "instr pointer over ran instrs")
        }
    }

    fn execute(&mut self, argc: usize, instr_pointer: usize) -> Result<Reference, InstrError> {
        let prev_instr_pointer = self.instr_pointer;
        self.stack.push(Data::Frame(argc, 0, self.instr_pointer));
        self.instr_pointer = instr_pointer;
        while self.instr_pointer != prev_instr_pointer && self.instr_pointer < self.instrs.len() {
            self.eval_instr()?;
        }
        Ok(Reference::StackExpr)
    }

    fn execute_inline(&mut self, argc: usize, instr_pointer: usize, op: &mut dyn FnMut(&mut VM)->()) -> Result<Reference, InstrError> {
        let prev_instr_pointer = self.instr_pointer;
        self.stack.push(Data::Frame(argc, 0, self.instr_pointer));
        op(self);
        self.instr_pointer = instr_pointer;
        while self.instr_pointer != prev_instr_pointer && self.instr_pointer < self.instrs.len() {
            self.eval_instr()?;
        }
        self.instr_pointer = prev_instr_pointer;
        Ok(Reference::StackExpr)
    }

    fn eval_tuple_or_matrix(&mut self, reference: Reference) -> Reference {
        match reference {
            Reference::Global(id) => {
                if let Some(reference) = self.globals.get(id) {
                    match reference {
                        Reference::Tuple(_) => *reference,
                        Reference::Matrix => *reference,
                        _ => Reference::None,
                    }
                } else {
                    Reference::None
                }
            },
            Reference::Argument(id) => {
                if let Some(reference) = self.stack.get_arg(id) {
                    match reference {
                        Reference::Tuple(_) => reference,
                        Reference::Matrix => reference,
                        _ => Reference::None,
                    }
                } else {
                    Reference::None
                }
            },
            Reference::Matrix => reference,
            Reference::Tuple(_) => reference,
            _ => Reference::None
        }
    }


    fn eval_unary_op(&mut self, a: Reference,op: &dyn Fn(f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let a = a.to_number(0, 0, self)?;
        op(a, self)
    }

    fn eval_unary_op_fixed(&mut self, a: usize,op: &dyn Fn(usize, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        op(a, self)
    }

    fn eval_binary_op(&mut self, a: Reference, b: Reference,op: &dyn Fn(f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let a = a.to_number(0, 0, self)?;
        let b = b.to_number(1, 0, self)?;

        op(a,b, self)
    }

    fn eval_binary_op_fixed1(&mut self, a: usize, b: Reference,op: &dyn Fn(usize, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let b = b.to_number(1, 0, self)?;

        op(a,b, self)
    }


    fn eval_binary_op_fixed2(&mut self, a: usize, b: usize,op: &dyn Fn(usize, usize, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        op(a,b, self)
    }

    fn eval_trinary_op(&mut self, a: Reference, b: Reference, c: Reference,op: &dyn Fn(f64, f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let a = a.to_number(0, 0, self)?;
        let b = b.to_number(1, 0, self)?;
        let c = c.to_number(2, 0, self)?;
        
        op(a,b,c, self)
    }

    fn eval_trinary_op_fixed1(&mut self, a: usize, b: Reference, c: Reference,op: &dyn Fn(usize, f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let b = b.to_number(1, 0, self)?;
        let c = c.to_number(2, 0, self)?;
        
        op(a, b,c, self)
    }

    #[allow(dead_code)]
    fn eval_trinary_op_fixed2(&mut self, a: usize, b: usize, c: Reference,op: &dyn Fn(usize, usize, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let c = c.to_number(2, 0, self)?;
        op(a, b, c, self)
    }

    #[allow(dead_code)]
    fn eval_quadnary_op(&mut self, a: Reference, b: Reference, c: Reference, d: Reference,op: &dyn Fn(f64, f64, f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let a = a.to_number(0, 0, self)?;
        let b = b.to_number(1, 0, self)?;
        let c = c.to_number(2, 0, self)?;
        let d = d.to_number(2, 0, self)?;
        
        op(a,b,c, d, self)
    }

    #[allow(dead_code)]
    fn eval_quadnary_op_fixed(&mut self, a: usize, b: Reference, c: Reference, d: Reference,op: &dyn Fn(usize, f64, f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let b = b.to_number(1, 0, self)?;
        let c = c.to_number(2, 0, self)?;
        let d = d.to_number(2, 0, self)?;

        op(a,b,c, d, self)
    }
}

#[cfg(test)]
mod tests {
    use crate::Printer;

    use super::{Instr, Reference, VM};
    
    /*
    0  JPNN 3 A0
    1  PUSH 0
    2  SETA 0 @
    3  JPNN 6 A1
    4  PUSH 0
    5  SETA 1 @
    6  POPX
    7  SETA 0 @
    8  SETA 1 @
    9  PUSH A1
    10 POPF @
    11 INTO F2,0 M
    12 POPX
    */
    #[test]
    fn test_eval_instr() {
        let instrs = vec![
            Instr::JumpNotNone(3, Reference::Argument(0)), 
            Instr::Push(Reference::Literal(0.0)), 
            Instr::SetArg(0, Reference::Stack),
            Instr::JumpNotNone(6, Reference::Argument(1)), 
            Instr::Push(Reference::Literal(0.0)), 
            Instr::SetArg(1, Reference::Stack),
            Instr::Pop(1),
            Instr::SetArg(0, Reference::Stack),
            Instr::SetArg(1, Reference::Stack),
            Instr::Push(Reference::Argument(0)), 
            Instr::PopFrame(Reference::Stack),
            Instr::Into(Reference::Executable(2,0), Reference::Matrix),
            Instr::Pop(1),
            ];
        let mut vm = VM::new((250, 250), 0, 100, instrs, 11, Box::new(Printer{}));
        vm.run().unwrap();
    }

    #[test]
    fn test_parse_reference() {

        {
            let reference = Reference::Executable(0, 0);
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::Stack;
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::StackIndex(1);
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::StackPeek;
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::Literal(0.0);
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::Global(0);
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::Argument(0);
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::Heap(0);
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::Matrix;
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::Tuple(0);
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
        {
            let reference = Reference::None;
            let str = reference.to_string();
            assert_eq!(reference, Reference::from_str(&mut str.chars().peekable()).unwrap());
        }
    }

    #[test]
    fn test_parse_instr() {
        {
            let instr = Instr::Add(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Subtract(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Multiply(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Divide(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

            
        {
            let instr = Instr::Equal(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Lesser(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Greater(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::LesserEqual(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::GreaterEqual(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::Not(Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::And(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Or(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Xor(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::Conditional(Reference::None, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::Push(Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Pop(1);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Duplicate;

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::PrintLn(Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::PrintLnString("hello".to_string());

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::GetMatrix(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::SetMatrix(Reference::None, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::GetFlatMatrix(Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::SetFlatMatrix(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::LenMatrix(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::WidthMatrix(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::HeightMatrix(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::PushFrame(0, 0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::PushInlineFrame(0, 0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::PopFrame(Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::GetArg(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::SetArg(0, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::GetGlobal(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::SetGlobal(0, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::CreateTuple(0, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::RemoveTuple(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::GetTupleReference(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::GetTuple(0, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::SetTuple(0, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::LenTuple(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::Jump(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::JumpEqual(0, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::JumpNotEqual(0, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::JumpLesser(0, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::JumpGreater(0, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::JumpLesserOrEqual(0, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::JumpGreaterOrEqual(0, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::JumpNotNone(0, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::ExecRef(Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::Alloc(0, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Free(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Point(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::SetPoint(0, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }


        {
            let instr = Instr::ReduceRange(Reference::None, Reference::None, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Reduce(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::Into(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

        {
            let instr = Instr::ForEach(Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }
            
        {
            let instr = Instr::ForEachRange(Reference::None, Reference::None, Reference::None, Reference::None);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }
            
        {
            let instr = Instr::Call(0);

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }
            
        {
            let instr = Instr::CallStr(0,"hello".to_string());

            let str = instr.to_string();
            assert_eq!(instr, Instr::from_str(&mut str.chars().peekable()).unwrap());
        }

    }
}