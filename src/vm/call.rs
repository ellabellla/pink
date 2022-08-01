use std::{collections::HashMap, f64::consts::PI};

use crate::{create_instr_error, create_unwrapped_instr_error};

use super::{Reference, InstrError, VM};


pub trait Call {
    fn name(&self) -> String;
    fn argc(&self) -> usize;
    fn any_scope(&self) -> bool;
    fn takes_str(&self) -> bool;
    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>;
    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError>;
}

pub struct SinCall{}
impl Call for SinCall {
    fn name(&self) -> String {
        "sin".to_string()
    }

    fn argc(&self) -> usize {
        1
    }

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::sin(num)))
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
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

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::cos(num)))
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
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

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::tan(num)))
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
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

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::abs(num)))
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
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

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::floor(num)))
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
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

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::ceil(num)))
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
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

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let a = reference.to_number(0, 0, vm)?;
            if let Some(reference) = vm.expr_stack.pop() {
                let b = reference.to_number(0, 0, vm)?;
                Ok(Reference::Literal(f64::powf(b, a)))
            } else {
                create_instr_error!(vm, "couldn't pop call arg from stack")
            }
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
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

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let a = reference.to_number(0, 0, vm)?;
            Ok(Reference::Literal(f64::sqrt(a)))
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
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

    fn any_scope(&self) -> bool {
        false
    }

    fn takes_str(&self) -> bool {
        true
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            vm.extern_println.println_num(num);
            Ok(Reference::Literal(num))
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }

    fn call_str(&self, string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        if let Some(reference) = vm.expr_stack.pop() {
            let num = reference.to_number(0, 0, vm)?;
            vm.extern_println.println_str(&format!("{} ({})", string, num));
            Ok(Reference::Literal(num))
        } else {
            create_instr_error!(vm, "couldn't pop call arg from stack")
        }
    }
}

pub struct CircleCall{}
impl Call for CircleCall {
    fn name(&self) -> String {
        "circle".to_string()
    }

    fn argc(&self) -> usize {
        3
    }

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        let radius = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?; 
        let y = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?; 
        let x = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?; 
        Ok(Reference::Literal(f64::sqrt(x.powf(2.0) + y.powf(2.0)) - radius))

    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
    }
}

pub struct TranslateCall{}
impl Call for TranslateCall {
    fn name(&self) -> String {
        "translate".to_string()
    }

    fn argc(&self) -> usize {
        2
    }

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        let offset_x = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let x = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?; 
        Ok(Reference::Literal(x-offset_x))

    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
    }
}

pub struct ScaleCall{}
impl Call for ScaleCall {
    fn name(&self) -> String {
        "scale".to_string()
    }

    fn argc(&self) -> usize {
        2
    }

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        let scale = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let x = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?; 
        Ok(Reference::Literal(x/scale))

    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
    }
}

pub struct RotateXCall{}
impl Call for RotateXCall {
    fn name(&self) -> String {
        "rotateX".to_string()
    }

    fn argc(&self) -> usize {
        3
    }

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        let rotation = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let y= vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let x = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?; 
        let rotation = rotation * PI * 2.0 * -1.0;
        let (sin, cos) = rotation.sin_cos();
        Ok(Reference::Literal(cos * x + sin * y))

    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
    }
}

pub struct RotateYCall{}
impl Call for RotateYCall {
    fn name(&self) -> String {
        "rotateY".to_string()
    }

    fn argc(&self) -> usize {
        3
    }

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        let rotation = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let y= vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let x = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?; 
        let rotation = rotation * PI * 2.0 * -1.0;
        let (sin, cos) = rotation.sin_cos();
        Ok(Reference::Literal(cos * y - sin * x))

    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
    }
}

pub struct RectangleCall{}
impl Call for RectangleCall {
    fn name(&self) -> String {
        "rect".to_string()
    }

    fn argc(&self) -> usize {
        4
    }

    fn any_scope(&self) -> bool {
        true
    }

    fn takes_str(&self) -> bool {
        false
    }

    fn call(&self, vm: &mut VM) -> Result<Reference, InstrError>{
        let half_y = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let half_x= vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let y = vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let x= vm.expr_stack.pop()
            .ok_or_else(|| create_unwrapped_instr_error!(vm, "couldn't pop call arg from stack"))?
            .to_number(0, 0, vm)?;
        let edge_dist_x    = x.abs() - half_x;
        let edge_dist_y    = y.abs() - half_y;
        let outside_distance = f64::sqrt(edge_dist_x.max(0.0).powf(2.0) + edge_dist_y.max(0.0).powf(2.0));
        let inside_distance = edge_dist_x.max(edge_dist_y).min(0.0);
        Ok(Reference::Literal(outside_distance + inside_distance))

    }

    fn call_str(&self, _string:&str, vm: &mut VM) -> Result<Reference, InstrError> {
        create_instr_error!(vm, "doesn't take string")
    }
}

pub const CALLS: [&dyn Call; 14] = [
    &SinCall{}, 
    &CosCall{}, 
    &TanCall{}, 
    &FloorCall{}, 
    &CeilCall{}, 
    &PowCall{}, 
    &SqrtCall{}, 
    &DebugCall{},
    &CircleCall{},
    &TranslateCall{},
    &ScaleCall{},
    &RotateXCall{},
    &RotateYCall{},
    &RectangleCall{}
];

pub fn create_call_map() -> HashMap<String, (usize, &'static dyn Call)> {
    let mut map = HashMap::new();
    for i in 0..CALLS.len() {
        map.insert(CALLS[i].name(), (i, CALLS[i]));
    }

    map
}