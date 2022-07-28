use std::collections::HashMap;

use super::{Reference, InstrError, VM};


pub trait Call {
    fn name(&self) -> String;
    fn argc(&self) -> usize;
    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>;
}

pub struct SinCall{}
impl Call for SinCall {
    fn name(&self) -> String {
        "sin".to_string()
    }

    fn argc(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::sin(num)))
        } else {
            Err(InstrError::new("couldn't pop call arg from stack"))
        }
    }
}
pub struct CosCall{}
impl Call for CosCall {
    fn name(&self) -> String {
        "cos".to_string()
    }

    fn argc(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::cos(num)))
        } else {
            Err(InstrError::new("couldn't pop call arg from stack"))
        }
    }
}

pub struct TanCall{}
impl Call for TanCall {
    fn name(&self) -> String {
        "tan".to_string()
    }

    fn argc(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::tan(num)))
        } else {
            Err(InstrError::new("couldn't pop call arg from stack"))
        }
    }
}

pub struct ABSCall{}
impl Call for ABSCall {
    fn name(&self) -> String {
        "abs".to_string()
    }

    fn argc(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::abs(num)))
        } else {
            Err(InstrError::new("couldn't pop call arg from stack"))
        }
    }
}

pub struct FloorCall{}
impl Call for FloorCall {
    fn name(&self) -> String {
        "floor".to_string()
    }

    fn argc(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::floor(num)))
        } else {
            Err(InstrError::new("couldn't pop call arg from stack"))
        }
    }
}

pub struct CeilCall{}
impl Call for CeilCall {
    fn name(&self) -> String {
        "ceil".to_string()
    }

    fn argc(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::ceil(num)))
        } else {
            Err(InstrError::new("couldn't pop call arg from stack"))
        }
    }
}


pub struct PowCall{}
impl Call for PowCall {
    fn name(&self) -> String {
        "pow".to_string()
    }

    fn argc(&self) -> usize {
        2
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let a = reference.to_number(0, 0, vm)?;
            if let Some(reference) = vm.expr_stack.pop() {
                let b = reference.to_number(0, 0, vm)?;
                Ok(Reference::Literal(f64::powf(b, a)))
            } else {
                Err(InstrError::new("couldn't pop call arg from stack"))
            }
        } else {
            Err(InstrError::new("couldn't pop call arg from stack"))
        }
    }
}
pub struct SqrtCall{}
impl Call for SqrtCall {
    fn name(&self) -> String {
        "sqrt".to_string()
    }

    fn argc(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let a = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::sqrt(a)))
        } else {
            Err(InstrError::new("couldn't pop call arg from stack"))
        }
    }
}

pub struct DebugCall{}
impl Call for DebugCall {
    fn name(&self) -> String {
        "debug".to_string()
    }

    fn argc(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            vm.extern_println.println_num(num);
            Ok(Reference::Literal(num))
        } else {
            Err(InstrError::new("couldn't pop call arg from stack"))
        }
    }
}

pub const CALLS: [&dyn Call; 8] = [&SinCall{}, &CosCall{}, &TanCall{}, &FloorCall{}, &CeilCall{}, &PowCall{}, &SqrtCall{}, &DebugCall{}];

pub fn create_call_map() -> HashMap<String, (usize, &'static dyn Call)> {
    let mut map = HashMap::new();
    for i in 0..CALLS.len() {
        map.insert(CALLS[i].name(), (i, CALLS[i]));
    }

    map
}