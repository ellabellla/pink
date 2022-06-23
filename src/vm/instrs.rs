use std::ops::Deref;

use super::{data::*, VM, StackData};

#[derive(Clone)]
pub enum Instr {
    ADD, // add
    SUB, // subtract
    MUL, // multiply
    DIV, // divide

    LES, // less
    GRE, // greater
    LEQ, // less or equal
    GEQ, // greater or equal

    NOT,
    AND,
    ORX, // or
    XOR, // xor

    ISX, // is
    CON, // concat
    IFX, // if
    ATX, // at

    PSH(StackData), //push
    DUP,  // duplicate top of stack

    LST(usize), // create list
    PSD, // push to list
    POD, // pop from list

    TUP(usize), // create tuple

    MTX(usize), // create matrix
    STX, // Set matrix
    GTX, // Get matrix

    EXC, //EXEC
    RDC, //REDUCE
    EXO, //EXEC external
    ITO, //ITO
}

fn add(a: &f64, b: &f64) -> f64 {
    a + b
}

fn sub(a: &f64, b: &f64) -> f64 {
    a - b
}

fn mul(a: &f64, b: &f64) -> f64 {
    a * b
}

fn div(a: &f64, b: &f64) -> f64 {
    a / b
}

fn les(a: &f64, b: &f64) -> f64 {
    if a < b { 1.0 } else { 0.0 }
}

fn gre(a: &f64, b: &f64) -> f64 {    
    if a > b { 1.0 } else { 0.0 }
}

fn leq(a: &f64, b: &f64) -> f64 {
    if a <= b { 1.0 } else { 0.0 }
}

fn geq(a: &f64, b: &f64) -> f64 {
    if a >= b { 1.0 } else { 0.0 }
}

fn not(a: &f64, b: &f64) -> f64 {
    if a != b { 1.0 } else { 0.0 }
}

fn and(a: &f64, b: &f64) -> f64 {
    if (*a != 0.0) && (*b != 0.0) { 1.0 } else { 0.0 }
}

fn orx(a: &f64, b: &f64) -> f64 {
    if (*a != 1.0) || (*b != 1.0) { 1.0 } else { 0.0 }
}

fn xor(a: &f64, b: &f64) -> f64 {
    if orx(a,b) != 0.0 && and(a,b) == 0.0  { 1.0 } else { 0.0 }
}


pub fn simple_operation(vm: &mut VM, f: &dyn Fn(&f64, &f64) -> f64) -> StackData {
    let a: StackData = vm.pop();
    let b: StackData = vm.pop();
    let mut matrix_a: Option<usize> = None;
    let mut tuple_a: Option<Tuple> = None;
    let mut data_a: Option<Data> = None;
    match a {
        StackData::FRAME(_) => panic!("Cannot operate on stack frames"),
        StackData::DATA(data) => match data {
            Data::STRING(_) => data_a = Some(Data::NOTHING),
            Data::NUMBER(_) => data_a = Some(data),
            Data::NOTHING => {},
        },
        StackData::MATRIX(matrix) => matrix_a = Some(matrix),
        StackData::TUPLE(tuple) => tuple_a = Some(tuple),
        StackData::LIST(list) => {},
    }

    let mut matrix_b: Option<usize> = None;
    let mut tuple_b: Option<Tuple> = None;
    let mut data_b: Option<Data> = None;
    match b {
        StackData::FRAME(_) => panic!("Cannot operate on stack frames"),
        StackData::DATA(data) => match data {
            Data::STRING(_) => {},
            Data::NUMBER(_) => data_b = Some(data),
            Data::NOTHING => {},
        },
        StackData::MATRIX(matrix) => matrix_b = Some(matrix),
        StackData::TUPLE(tuple) => tuple_b = Some(tuple),
        StackData::LIST(list) => {},
    }

    if let (Some(Data::NUMBER(num_a)), Some(Data::NUMBER(num_b))) = (&data_a, &data_b){
        StackData::DATA(Data::NUMBER(f(num_a,num_b)))
    } else if let (Some(mut tuple), Some(Data::NUMBER(num))) = (tuple_a, &data_b) {
        for i in 0..tuple.deref().len() {
            match tuple[i] {
                Data::STRING(_) => tuple[i] = Data::NOTHING,
                Data::NUMBER(num_a) => tuple[i] = Data::NUMBER(f(&num_a, num)),
                Data::NOTHING => tuple[i] = Data::NOTHING,
            } 
        } 

        StackData::TUPLE(tuple)
    } else if let (Some(mut tuple), Some(Data::NUMBER(num))) = (tuple_b, &data_a) {
        for i in 0..tuple.deref().len() {
            match tuple[i] {
                Data::STRING(_) => tuple[i] = Data::NOTHING,
                Data::NUMBER(num_a) => tuple[i] = Data::NUMBER(f(&num_a, num)),
                Data::NOTHING => tuple[i] = Data::NOTHING,
            } 
        } 

        StackData::TUPLE(tuple)
    } else if let (Some(index), Some(Data::NUMBER(num))) = (matrix_a, &data_b) {
        let matrix = vm.get_matrix(index);
        if let Some(matrix) = matrix {
            for i in 0..matrix.memory.len() {
                matrix.memory[i] = f(&matrix.memory[i], num);
            }
        }
        StackData::MATRIX(index)
    } else if let (Some(index), Some(Data::NUMBER(num))) = (matrix_b, &data_a) {
        let matrix = vm.get_matrix(index);
        if let Some(matrix) = matrix {
            for i in 0..matrix.memory.len() {
                matrix.memory[i] = f(&matrix.memory[i], num);
            }
        }
        StackData::MATRIX(index)
    } else {
        StackData::DATA(Data::NOTHING)
    }
}

pub fn match_instr(instr: Instr, vm: &mut VM) {
    match instr {
        Instr::ADD => {
            let data = simple_operation(vm, &add);
            vm.push(data);
        },
        Instr::SUB =>  {
            let data = simple_operation(vm, &sub);
            vm.push(data);
        },
        Instr::MUL =>  {
            let data = simple_operation(vm, &mul);
            vm.push(data);
        },
        Instr::DIV =>  {
            let data = simple_operation(vm, &div);
            vm.push(data);
        },
        Instr::LES =>  {
            let data = simple_operation(vm, &les);
            vm.push(data);
        },
        Instr::GRE =>  {
            let data = simple_operation(vm, &gre);
            vm.push(data);
        },
        Instr::LEQ =>  {
            let data = simple_operation(vm, &leq);
            vm.push(data);
        },
        Instr::GEQ =>  {
            let data = simple_operation(vm, &geq);
            vm.push(data);
        },
        Instr::NOT =>  {
            let data = simple_operation(vm, &not);
            vm.push(data);
        },
        Instr::AND =>  {
            let data = simple_operation(vm, &and);
            vm.push(data);
        },
        Instr::ORX =>  {
            let data = simple_operation(vm, &orx);
            vm.push(data);
        },
        Instr::XOR =>  {
            let data = simple_operation(vm, &xor);
            vm.push(data);
        },
        Instr::ISX => todo!(),
        Instr::CON => todo!(),
        Instr::IFX => todo!(),
        Instr::ATX => todo!(),
        Instr::PSH(data) => vm.push(data),
        Instr::DUP => { 
            match vm.pop() {
                StackData::FRAME(_) => panic!("Cannot DUP stack frame"),
                StackData::DATA(data) => match data {
                    Data::STRING(string) => {
                        vm.push(StackData::DATA(Data::STRING(string)));
                        vm.push(StackData::DATA(Data::STRING(string)));
                    },
                    Data::NUMBER(num) => {
                        vm.push(StackData::DATA(Data::NUMBER(num)));
                        vm.push(StackData::DATA(Data::NUMBER(num)));
                    },
                    Data::NOTHING => {
                        vm.push(StackData::DATA(Data::NOTHING));
                        vm.push(StackData::DATA(Data::NOTHING));
                    },
                },
                StackData::MATRIX(index) => {
                    vm.push(StackData::MATRIX(index));
                    vm.push(StackData::MATRIX(index));
                },
                StackData::TUPLE(tuple) => {
                    vm.push(StackData::TUPLE(tuple.deref().clone().to_vec().into_boxed_slice()));
                    vm.push(StackData::TUPLE(tuple));
                },
                StackData::LIST(list) => {
                    vm.push(StackData::LIST(list.deref().clone().to_vec()));
                    vm.push(StackData::LIST(list));
                },
            }
        },
        Instr::LST(_) => todo!(),
        Instr::PSD => todo!(),
        Instr::POD => todo!(),
        Instr::TUP(size) => { 
            let data = StackData::TUPLE(Tuple::new_tuple(size, vm));
            vm.push(data);
        },
        Instr::MTX(_) => todo!(),
        Instr::STX => todo!(),
        Instr::GTX => todo!(),
        Instr::EXC => todo!(),
        Instr::RDC => todo!(),
        Instr::EXO => todo!(),
        Instr::ITO => todo!(),
    }
}