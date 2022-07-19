use std::{collections::HashMap, str::{Chars}, fmt, iter::Peekable};

use super::{Stack, Data, Matrix};

pub struct InstrError {
    msg: String
}

#[allow(dead_code)]
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
    Literal(f64),
    Global(usize),
    Matrix(usize),
    Tuple(usize),
    None,
}


impl Reference {
    pub fn to_number(&self, x: usize, y: usize, vm:  &mut VM) -> Option<f64> {
        match self {
            Reference::Stack => vm.stack.pop().and_then(|data| data.to_number(vm)),
            Reference::Global(index) => {
                let reference = vm.globals.get(*index)?.clone();
                reference.to_number(x, y, vm)
            },
            Reference::Matrix(index) => vm.matrices.get(index).and_then(|matrix| matrix.get(x, y)),
            Reference::Literal(num) => Some(*num),
            Reference::Tuple(tuple_index) => {
                if let Some(tuple) = vm.tuples.get(tuple_index) {
                    let reference = tuple.get(x)?.clone();
                    reference.to_number(x, y, vm)
                } else {
                    None
                }
            },
            Reference::None => None,
            Reference::Executable(argc, instr_pointer) => vm.execute(*argc, *instr_pointer).to_number(x, y, vm),
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
                '@' => {
                    return Ok(Reference::Stack)
                },
                'G' => {
                    ref_type = Reference::Global(0);
                },
                'M' => {
                    ref_type = Reference::Matrix(0);
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
                chars.next();

                let index = parse_number(chars)?;

                match ref_type {
                    Reference::Global(_) => return Ok(Reference::Global(index.floor() as usize)),
                    Reference::Executable(_, _) => {
                        if let Some(delimiter) = chars.next() {
                            if delimiter == ','{
                                let index2 = parse_number(chars)?;
                                return Ok(Reference::Executable(index.floor() as usize, index2.floor() as usize))
                            }
                        }
                        return Err(InstrError::new("Expected delimiter ','"));
                    },
                    Reference::Tuple(_) => return Ok(Reference::Tuple(index.floor() as usize)),
                    Reference::Matrix(_) => return Ok(Reference::Matrix(index.floor() as usize)),
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
            Reference::Stack => String::from("@"),
            Reference::Global(index) => String::from(format!("G{}", index)),
            Reference::Matrix(index) => String::from(format!("M{}", index)),
            Reference::Literal(number) => String::from(format!("{}", number)),
            Reference::Tuple(index) => String::from(format!("T{}", index)),
            Reference::None => String::from(""),
            Reference::Executable(argc, instr_pointer) => String::from(format!("F{},{}", argc, instr_pointer)),
        }
    }
}

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
    Pop,
    Duplicate,

    CreateMatrix(usize, Reference, Reference),
    RemoveMatrix(usize),
    GetMatrix(usize, Reference, Reference),
    SetMatrix(usize, Reference, Reference, Reference),
    GetFlatMatrix(usize, Reference),
    SetFlatMatrix(usize, Reference, Reference),
    LenMatrix(usize),
    WidthMatrix(usize),
    HeightMatrix(usize),

    PushFrame(usize),
    PopFrame(Reference),
    GetArg(usize),
    SetArg(usize, Reference),

    GetGlobal(usize),
    SetGlobal(usize, Reference),

    CreateTuple(usize, Reference),
    RemoveTuple(usize),
    GetTuple(usize, Reference),
    SetTuple(usize, Reference, Reference),
    LenTuple(usize),

    Jump(usize),
    JumpEqual(usize, Reference, Reference),
    JumpLesser(usize, Reference, Reference),
    JumpGreater(usize, Reference, Reference),
    JumpLesserOrEqual(usize, Reference, Reference),
    JumpGreaterOrEqual(usize, Reference, Reference),
}


impl Instr {
    fn from_str(chars: &mut Peekable<Chars>) -> Result<Self, InstrError> {
        let mut instr = vec![];

        const INSTR_LEN: usize = 4;
        for _ in 0..INSTR_LEN {
            if let Some(char) = chars.next() {
                instr.push(char)
            } else {
                return Err(InstrError::new("instr string not lone enough"))
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
            "ANDX" => Ok(Instr::Add(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "ORXX" => Ok(Instr::Or(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "XORX" => Ok(Instr::Xor(Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "COND" => Ok(Instr::Conditional(Reference::from_str(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "PUSH" => Ok(Instr::Push(Reference::from_str(chars)?)),
            "POPX" => Ok(Instr::Pop),
            "DUPX" => Ok(Instr::Duplicate),
            "CRTM" => Ok(Instr::CreateMatrix(parse_usize(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "RMMT" => Ok(Instr::RemoveMatrix(parse_usize(chars)?)),
            "GETM" => Ok(Instr::GetMatrix(parse_usize(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "SETM" => Ok(Instr::SetMatrix(parse_usize(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "GTFM" => Ok(Instr::GetFlatMatrix(parse_usize(chars)?, Reference::from_str(chars)?)),
            "STFM" => Ok(Instr::SetFlatMatrix(parse_usize(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "LNMT" => Ok(Instr::LenMatrix(parse_usize(chars)?)),
            "WDMT" => Ok(Instr::WidthMatrix(parse_usize(chars)?)),
            "HTMT" => Ok(Instr::HeightMatrix(parse_usize(chars)?)),
            "PSHF" => Ok(Instr::PushFrame(parse_usize(chars)?)),
            "POPF" => Ok(Instr::PopFrame(Reference::from_str(chars)?)),
            "GETA" => Ok(Instr::GetArg(parse_usize(chars)?)),
            "SETA" => Ok(Instr::SetArg(parse_usize(chars)?, Reference::from_str(chars)?)),
            "GETG" => Ok(Instr::GetGlobal(parse_usize(chars)?)),
            "SETG" => Ok(Instr::SetGlobal(parse_usize(chars)?, Reference::from_str(chars)?)),
            "CRUP" => Ok(Instr::CreateTuple(parse_usize(chars)?, Reference::from_str(chars)?)),
            "RTUP" => Ok(Instr::RemoveTuple(parse_usize(chars)?)),
            "GETT" => Ok(Instr::GetTuple(parse_usize(chars)?, Reference::from_str(chars)?)),
            "SETT" => Ok(Instr::SetTuple(parse_usize(chars)?, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "LNUP" => Ok(Instr::LenTuple(parse_usize(chars)?)),
            "JMPX" => Ok(Instr::Jump(parse_number(chars)?.floor() as usize)),
            "JPEQ" => Ok(Instr::JumpEqual(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPLS" => Ok(Instr::JumpLesser(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPGR" => Ok(Instr::JumpGreater(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPLE" => Ok(Instr::JumpLesserOrEqual(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
            "JPGE" => Ok(Instr::JumpGreaterOrEqual(parse_number(chars)?.floor() as usize, Reference::from_str(chars)?, Reference::from_str(chars)?)),
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
            Instr::Pop => "POPX".to_string(),
            Instr::Duplicate => "DUPX".to_string(),
            Instr::CreateMatrix(a,  b, c) => format!("CRTM {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::RemoveMatrix(a) => format!("RMMT {}", a.to_string()),
            Instr::GetMatrix(a,  b, c) => format!("GETM {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::SetMatrix(a,  b, c, d) => format!("SETM {} {} {} {}", a.to_string(), b.to_string(), c.to_string(), d.to_string()),
            Instr::GetFlatMatrix(a,  b) => format!("GTFM {} {}", a.to_string(), b.to_string()),
            Instr::SetFlatMatrix(a,  b, c) => format!("STFM {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::LenMatrix(a) => format!("LNMT {}", a.to_string()),
            Instr::WidthMatrix(a) => format!("WDMT {}", a.to_string()),
            Instr::HeightMatrix(a) => format!("HTMT {}", a.to_string()),
            Instr::PushFrame(a) => format!("PSHF {}", a.to_string()),
            Instr::PopFrame(a) => format!("POPF {}", a.to_string()),
            Instr::GetArg(a) => format!("GETA {}", a.to_string()),
            Instr::SetArg(a,  b) => format!("SETA {} {}", a.to_string(), b.to_string()),
            Instr::GetGlobal(a) => format!("GETG {}", a.to_string()),
            Instr::SetGlobal(a,  b) => format!("SETG {} {}", a.to_string(), b.to_string()),
            Instr::CreateTuple(a,  b) => format!("CRUP {} {}", a.to_string(), b.to_string()),
            Instr::RemoveTuple(a) => format!("RTUP {}", a.to_string()),
            Instr::GetTuple(a,  b) => format!("GETT {} {}", a.to_string(), b.to_string()),
            Instr::SetTuple(a,  b, c) => format!("SETT {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::LenTuple(a) => format!("LNUP {}", a.to_string()),
            Instr::Jump(a) => format!("JMPX {}", a.to_string()),
            Instr::JumpEqual(a,  b, c) => format!("JPEQ {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpLesser(a,  b, c) => format!("JPLS {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpGreater(a,  b, c) => format!("JPGR {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpLesserOrEqual(a,  b, c) => format!("JPLE {} {} {}", a.to_string(), b.to_string(), c.to_string()),
            Instr::JumpGreaterOrEqual(a,  b, c) => format!("JPGE {} {} {}", a.to_string(), b.to_string(), c.to_string()),
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


#[allow(dead_code)]
mod instr_ops {
    use crate::vm::{Data, Matrix};

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

    pub fn not(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if a != b { 1.0 } else { 0.0 }))
    }

    pub fn and(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if (a != 0.0) && (b != 0.0) { 1.0 } else { 0.0 }))
    }

    pub fn or(a: f64, b: f64, _vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if (a != 1.0) || (b != 1.0) { 1.0 } else { 0.0 }))
    }

    pub fn xor(a: f64, b: f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        let or = or(a,b,vm)?.ok_or(InstrError::new("failed to or values"))?;
        let and = and(a,b,vm)?.ok_or(InstrError::new("failed to or values"))?;
        if or != 0.0 && and == 0.0 { Ok(Some(1.0)) } else { Ok(Some(0.0)) }
    }

    pub fn conditional(a: f64, b: f64, c: f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        Ok(Some(if a != 0.0 { b } else { c }))
    }

    pub fn create_matrix(a: usize, b: f64, c: f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(_) = vm.matrices.insert(a, Matrix::new(b.floor() as usize, c.floor() as usize)) {
            Ok(None)
        } else {
            Err(InstrError::new("could not create matrix"))
        }
    }
    pub fn remove_matrix(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(_) = vm.matrices.remove(&a) {
            Ok(None)
        } else {
            Err(InstrError::new("could not remove"))
        }
    }   
    pub fn get_matrix(a:usize, b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(matrix) = vm.matrices.get(&a) {
            if let Some(number) = matrix.get(b.floor() as usize, c.floor() as usize) {
                Ok(Some(number))
            } else{
                Err(InstrError::new("could not get value in matrix"))
            }
        } else {
            Err(InstrError::new("could not get matrix"))
        }

    }
    
    pub fn set_matrix(a: usize, b:  f64, c:  f64, d:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(matrix) = vm.matrices.get_mut(&a) {
            if let Some(number) = matrix.set(b.floor() as usize, c.floor() as usize, d) {
                Ok(Some(number))
            } else{
                Err(InstrError::new("could not get value in matrix"))
            }
        } else {
            Err(InstrError::new("could not get matrix"))
        }
    }   
    pub fn get_flat_matrix(a:usize, b:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(matrix) = vm.matrices.get(&a) {
            if let Some(number) = matrix.get_flat(b.floor() as usize) {
                Ok(Some(number))
            } else{
                Err(InstrError::new("could not get value in matrix"))
            }
        } else {
            Err(InstrError::new("could not get matrix"))
        }

    }
    
    pub fn set_flat_matrix(a: usize, b:  f64, c:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(matrix) = vm.matrices.get_mut(&a) {
            if let Some(number) = matrix.set_flat(b.floor() as usize, c) {
                Ok(Some(number))
            } else{
                Err(InstrError::new("could not get value in matrix"))
            }
        } else {
            Err(InstrError::new("could not get matrix"))
        }
    }   

    pub fn len_matrix(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(matrix) = vm.matrices.get(&a) {
            Ok(Some(matrix.len() as f64))
        } else {
            Err(InstrError::new("could not get matrix"))
        }
    }

    pub fn width_matrix(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(matrix) = vm.matrices.get(&a) {
            Ok(Some(matrix.width() as f64))
        } else {
            Err(InstrError::new("could not get matrix"))
        }
    }

    pub fn height_matrix(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(matrix) = vm.matrices.get(&a) {
            Ok(Some(matrix.height() as f64))
        } else {
            Err(InstrError::new("could not get matrix"))
        }
    }
    pub fn push_frame(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        vm.stack.push(Data::Frame(a, 0, vm.instr_pointer));
        Ok(None)
    }

    pub fn create_tuple(a: usize, b:  f64, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(_) = vm.tuples.insert(a, vec![Reference::None; b.floor() as usize]) {
            Ok(None)
        } else {
            Err(InstrError::new("couldn't create tuple"))
        }
    }
    pub fn remove_tuple(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(_) = vm.tuples.remove(&a) {
            Ok(None)
        } else {
            Err(InstrError::new("couldn't remove tuple"))
        }
    }
    pub fn len_tuple(a: usize, vm: &mut VM) -> Result<Option<f64>, InstrError>{
        if let Some(tuples) = vm.tuples.get(&a) {
            Ok(Some(tuples.len() as f64))
        } else {
            Err(InstrError::new("could not get tuple"))
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
}
pub struct VM {
    globals: Vec<Reference>,
    matrices: HashMap<usize, Matrix>,
    tuples: HashMap<usize, Vec<Reference>>,
    stack: Stack,
    instrs: Vec<Instr>,
    instr_pointer: usize,
}

impl VM {
    pub fn new(globals_capacity: usize, stack_capacity: usize, instrs: Vec<Instr>, instr_pointer: usize)-> VM {
        VM { 
            globals: vec![Reference::None; globals_capacity], 
            matrices: HashMap::new(),
            tuples: HashMap::new(), 
            stack: Stack::new(stack_capacity), 
            instrs: instrs, 
            instr_pointer,
        }
    }

    pub fn run(&mut self) {
        while self.instr_pointer < self.instrs.len() {
            if let Err(err) = self.eval_instr() {
                panic!("{}", err.msg);
            }
        }
    }

    pub fn eval_instr(&mut self) -> Result<(), InstrError> {
        if let Some(instr) = self.instrs.get(self.instr_pointer) {
            self.instr_pointer+=1;
            let res = match *instr.clone() {
                Instr::Add(a, b) => self.eval_binary_op(a, b, &instr_ops::add),
                Instr::Subtract (a,b) => self.eval_binary_op(a, b, &instr_ops::sub),
                Instr::Multiply (a,b) => self.eval_binary_op(a, b, &instr_ops::multiply),
                Instr::Divide (a,b) => self.eval_binary_op(a, b, &instr_ops::divide),
                Instr::Equal (a,b) => self.eval_binary_op(a, b, &instr_ops::equal),
                Instr::Lesser (a,b) => self.eval_binary_op(a, b, &instr_ops::lesser),
                Instr::Greater (a,b) => self.eval_binary_op(a, b, &instr_ops::greater),
                Instr::LesserEqual (a,b) => self.eval_binary_op(a, b, &instr_ops::lesser_equal),
                Instr::GreaterEqual (a,b) => self.eval_binary_op(a, b, &instr_ops::greater_equal),
                Instr::Not(a) => {
                    let a = a.to_number(0, 0, self);
                    if let Some(a) = a {
                        Ok(Some(if a != 0.0 { 0.0 } else { 1.0 }))
                    } else {
                        Err(InstrError::new("could not resolve references"))
                    }
                },
                Instr::And (a,b) => self.eval_binary_op(a, b, &instr_ops::and),
                Instr::Or (a,b) => self.eval_binary_op(a, b, &instr_ops::or),
                Instr::Xor (a,b) => self.eval_binary_op(a, b, &instr_ops::xor),
                Instr::Conditional(a, b, c) => self.eval_trinary_op(a, b, c, &instr_ops::conditional),
                Instr::Push(a) => {
                    self.stack.push(Data::Reference(a));
                    Ok(None)
                },
                Instr::Pop => {self.stack.pop(); Ok(None)},
                Instr::Duplicate => {
                    if let Some(data) = self.stack.peek() {
                        self.stack.push(data);
                        Ok(None)
                    } else {
                        Err(InstrError::new("stack is empty"))
                    }
                },
                Instr::CreateMatrix(a, b, c) => self.eval_trinary_op_fixed1(a, b, c, &instr_ops::create_matrix),
                Instr::RemoveMatrix(a) => self.eval_unary_op_fixed(a, &instr_ops::remove_matrix),
                Instr::GetMatrix(a, b, c) => self.eval_trinary_op_fixed1(a, b, c, &instr_ops::get_matrix),
                Instr::SetMatrix(a, b, c, d) => self.eval_quadnary_op_fixed(a, b, c, d, &instr_ops::set_matrix),
                Instr::GetFlatMatrix(a, b) => self.eval_binary_op_fixed1(a, b, &instr_ops::get_flat_matrix),
                Instr::SetFlatMatrix(a, b, c) => self.eval_trinary_op_fixed1(a, b, c, &instr_ops::set_flat_matrix),
                Instr::LenMatrix(a) => self.eval_unary_op_fixed(a, &instr_ops::len_matrix),
                Instr::WidthMatrix(a) => self.eval_unary_op_fixed(a, &instr_ops::width_matrix),
                Instr::HeightMatrix(a) => self.eval_unary_op_fixed(a, &instr_ops::height_matrix),
                Instr::PushFrame(a) => self.eval_unary_op_fixed(a, &instr_ops::push_frame),
                Instr::PopFrame(a) => {
                    if let Some(instr_pointer) = self.stack.pop_frame() {
                        self.instr_pointer = instr_pointer;
                        self.stack.push(Data::Reference(a));
                        Ok(None)
                    } else {
                        Err(InstrError::new("couldn't pop frame from stack"))
                    }
                },
                Instr::GetArg(a) => {
                    if let Some(reference) = self.stack.get_arg(a) {
                        self.stack.push(Data::Reference(reference));
                        Ok(None)
                    } else {
                        Err(InstrError::new("args index out of bounds"))
                    }
                },
                Instr::SetArg(a, b) => {
                    if let Some(reference) = self.stack.set_arg(a, b) {
                        Ok(None)
                    } else {
                        Err(InstrError::new("args index out of bounds"))
                    }
                },
                Instr::GetGlobal(a) => {
                    if let Some(reference) = self.globals.get(a) {
                        self.stack.push(Data::Reference(*reference));
                        Ok(None)
                    } else {
                        Err(InstrError::new("args index out of bounds"))
                    }
                },
                Instr::SetGlobal(a, b) => {
                    if let Some(reference) = self.globals.get_mut(a) {
                        *reference = b;
                        Ok(None)
                    } else {
                        Err(InstrError::new("args index out of bounds"))
                    }
                },
                Instr::CreateTuple(a, b) => self.eval_binary_op_fixed1(a, b, &instr_ops::create_tuple),
                Instr::RemoveTuple(a) => self.eval_unary_op_fixed(a, &instr_ops::remove_tuple),
                Instr::GetTuple(a, b) => {
                    if let Some(b) = b.to_number(0, 0, self) {
                        if let Some(references) = self.tuples.get(&a) {
                            if let Some(reference) = references.get(b.floor() as usize) {
                                self.stack.push(Data::Reference(*reference));
                                Ok(None)
                            } else {
                                Err(InstrError::new("tuple index out of bounds"))
                            }
                        } else {
                            Err(InstrError::new("args index out of bounds"))
                        }
                    } else {
                        Err(InstrError::new("reference does not resolve to a number"))
                    }
                },
                Instr::SetTuple(a, b, c) => {
                    if let Some(b) = b.to_number(0, 0, self) {
                        if let Some(references) = self.tuples.get_mut(&a) {
                            if let Some(reference) = references.get_mut(b.floor() as usize) {
                                *reference = c;
                                Ok(None)
                            } else {
                                Err(InstrError::new("tuple index out of bounds"))
                            }
                        } else {
                            Err(InstrError::new("args index out of bounds"))
                        }
                    } else {
                        Err(InstrError::new("reference does not resolve to a number"))
                    }
                },
                Instr::LenTuple(a) => self.eval_unary_op_fixed(a, &instr_ops::len_tuple),
                Instr::Jump(a) => self.eval_unary_op_fixed(a, &instr_ops::jump),
                Instr::JumpEqual(a, b, c) => self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_equal),
                Instr::JumpLesser(a, b, c) => self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_lesser),
                Instr::JumpGreater(a, b, c) =>  self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_greater),
                Instr::JumpLesserOrEqual(a, b, c) =>  self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_lesser_equal),
                Instr::JumpGreaterOrEqual(a, b, c) =>  self.eval_trinary_op_fixed1(a, b, c, &instr_ops::jump_greater_equal),
            };

            if let Ok(value) = res {
                if let Some(value) = value {
                    self.stack.push(Data::Reference(Reference::Literal(value)));
                }
                Ok(())
            } else {
                Err(InstrError::new("could not evaluate instr"))
            }
        } else {
            Err(InstrError::new("instr pointer over ran instrs"))
        }
    }

    fn execute(&mut self, argc: usize, instr_pointer: usize) -> Reference {
        let prev_instr_pointer = self.instr_pointer;
        self.stack.push(Data::Frame(argc, 0, self.instr_pointer));
        self.instr_pointer = instr_pointer;
        while self.instr_pointer != prev_instr_pointer && self.instr_pointer < self.instrs.len() {
            if let Err(err) = self.eval_instr() {
                panic!("{}", err.msg);
            }
        }
        Reference::Stack
    }

    #[allow(dead_code)]
    fn eval_unary_op(&mut self, a: Reference,op: &dyn Fn(f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let a = a.to_number(0, 0, self);

        if a == None {
            Err(InstrError::new("could not resolve references"))
        } else {
            let a = a.unwrap();
            
            op(a, self)
        }
    }

    fn eval_unary_op_fixed(&mut self, a: usize,op: &dyn Fn(usize, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        op(a, self)
    }

    fn eval_binary_op(&mut self, a: Reference, b: Reference,op: &dyn Fn(f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let a = a.to_number(0, 0, self);
        let b = b.to_number(1, 0, self);

        if a == None || b == None {
            Err(InstrError::new("could not resolve references"))
        } else {
            let a = a.unwrap();
            let b = b.unwrap();
            
            op(a,b, self)
        }
    }

    fn eval_binary_op_fixed1(&mut self, a: usize, b: Reference,op: &dyn Fn(usize, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let b = b.to_number(1, 0, self);

        if b == None {
            Err(InstrError::new("could not resolve references"))
        } else {
            let b = b.unwrap();
            
            op(a,b, self)
        }
    }

    fn eval_binary_op_fixed2(&mut self, a: usize, b: usize,op: &dyn Fn(usize, usize, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        op(a,b, self)
    }

    fn eval_trinary_op(&mut self, a: Reference, b: Reference, c: Reference,op: &dyn Fn(f64, f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let a = a.to_number(0, 0, self);
        let b = b.to_number(1, 0, self);
        let c = c.to_number(2, 0, self);
        
        if a == None || b == None || c == None {
            Err(InstrError::new("could not resolve references"))
        } else {
            let a = a.unwrap();
            let b = b.unwrap();
            let c = c.unwrap();

            op(a,b,c, self)
        }
    }

    fn eval_trinary_op_fixed1(&mut self, a: usize, b: Reference, c: Reference,op: &dyn Fn(usize, f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let b = b.to_number(1, 0, self);
        let c = c.to_number(2, 0, self);
        
        if b == None || c == None {
            Err(InstrError::new("could not resolve references"))
        } else {
            let b = b.unwrap();
            let c = c.unwrap();

            op(a, b,c, self)
        }
    }

    fn eval_trinary_op_fixed2(&mut self, a: usize, b: usize, c: Reference,op: &dyn Fn(usize, usize, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let c = c.to_number(2, 0, self);
        
        if c == None {
            Err(InstrError::new("could not resolve references"))
        } else {
            let c = c.unwrap();

            op(a, b, c, self)
        }
    }

    fn eval_quadnary_op(&mut self, a: Reference, b: Reference, c: Reference, d: Reference,op: &dyn Fn(f64, f64, f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let a = a.to_number(0, 0, self);
        let b = b.to_number(1, 0, self);
        let c = c.to_number(2, 0, self);
        let d = d.to_number(2, 0, self);
        
        if a == None || b == None || c == None || d == None {
            Err(InstrError::new("could not resolve references"))
        } else {
            let a = a.unwrap();
            let b = b.unwrap();
            let c = c.unwrap();
            let d = d.unwrap();

            op(a,b,c, d, self)
        }
    }

    fn eval_quadnary_op_fixed(&mut self, a: usize, b: Reference, c: Reference, d: Reference,op: &dyn Fn(usize, f64, f64, f64, &mut VM) -> Result<Option<f64>, InstrError>) -> Result<Option<f64>, InstrError> {
        let b = b.to_number(1, 0, self);
        let c = c.to_number(2, 0, self);
        let d = d.to_number(2, 0, self);
        
        if b == None || c == None || d == None {
            Err(InstrError::new("could not resolve references"))
        } else {
            let b = b.unwrap();
            let c = c.unwrap();
            let d = d.unwrap();

            op(a,b,c, d, self)
        }
    }
}