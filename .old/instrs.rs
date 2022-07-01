use std::ops::Deref;

use rand::seq::index;

use super::{data::*, VM, StackData, stack, Stack};

#[derive(Clone, Copy)]
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

    POD, // pop from list
    
    MTX(usize, usize), // create matrix
    STX, // Set matrix

    EXC, //EXEC
    RDC, //REDUCE
    EXO, //EXEC external
    ITO, //Into
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
    let mut tuple_a: Option<usize> = None;
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
    let mut tuple_b: Option<usize> = None;
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
    } else if let (Some(tuple), Some(Data::NUMBER(num))) = (tuple_a, &data_b) {
        if let Some(tuple) = vm.get_tuple_mut(tuple) {
            for i in 0..tuple.deref().len() {
                match tuple[i] {
                    Data::STRING(_) => tuple[i] = Data::NOTHING,
                    Data::NUMBER(num_a) => tuple[i] = Data::NUMBER(f(&num_a, num)),
                    Data::NOTHING => tuple[i] = Data::NOTHING,
                } 
            } 
        }
        StackData::TUPLE(tuple)
    } else if let (Some(tuple), Some(Data::NUMBER(num))) = (tuple_b, &data_a) {
        if let Some(tuple) = vm.get_tuple_mut(tuple) {
            for i in 0..tuple.deref().len() {
                match tuple[i] {
                    Data::STRING(_) => tuple[i] = Data::NOTHING,
                    Data::NUMBER(num_a) => tuple[i] = Data::NUMBER(f(&num_a, num)),
                    Data::NOTHING => tuple[i] = Data::NOTHING,
                } 
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
        Instr::ISX => {
            if let StackData::DATA(Data::NOTHING) = vm.pop() {
                vm.push(StackData::DATA(Data::NUMBER(0.0)));
            } else{
                vm.push(StackData::DATA(Data::NUMBER(1.0)));
            }
        },
        Instr::CON => {
                match vm.pop() {
                StackData::DATA(data_a) => {
                    match vm.pop() {
                        StackData::DATA(data_b) => {
                            let index = vm.get_list_index();
                            vm.add_list(index, vec![
                                vec![
                                    Instr::PSH(StackData::DATA(data_a)), 
                                    Instr::PSH(StackData::DATA(data_b))
                                ]
                            ]);
                            vm.push(StackData::LIST(index));
                        },
                        StackData::LIST(mut list_b) => {
                            let index = vm.get_list_index();
                            if let Some(list_b) = vm.get_list_mut(list_b) {
                                let mut list_a = vec![vec![
                                    Instr::PSH(StackData::DATA(data_a))
                                ]];
                                list_a.append(list_b);
                                vm.add_list(index, list_a);
                                vm.push(StackData::LIST(index))
                            } else {
                                vm.push(StackData::DATA(Data::NOTHING));
                            }
                        },
                        _ => {
                            vm.push(StackData::DATA(Data::NOTHING));
                        }
                    }
                },
                StackData::LIST(mut list_a) => {
                    match vm.pop() {
                        StackData::DATA(data_b) => {
                            if let Some(list_a_data) = vm.get_list_mut(list_a) {
                                let mut list_b = vec![vec![
                                    Instr::PSH(StackData::DATA(data_b))
                                ]];
                                list_a_data.append(&mut list_b);
                                vm.push(StackData::LIST(list_a))
                            } else {
                                vm.push(StackData::DATA(Data::NOTHING));
                            }
                        },
                        StackData::LIST(mut list_b) => {
                            if let Some(list_b_data) = vm.remove_list(list_b) {
                                if let Some(list_a_data) = vm.get_list_mut(list_a) {
                                    list_a_data.extend(list_b_data);
                                    vm.push(StackData::LIST(list_a))
                                }
                            }
                        },
                        _ => {
                            vm.push(StackData::DATA(Data::NOTHING));
                        }
                    }
                },
                _ => {
                    vm.push(StackData::DATA(Data::NOTHING));
                }
            }
        },
        Instr::IFX => {
            let mut condition = false;
            match vm.pop() {
                StackData::DATA(data) => {
                    match data {
                        Data::NUMBER(num) => condition = num != 0.0,
                        _ => ()
                    }
                },
                StackData::TUPLE(tuple) => {
                    if let Some(tuple) = vm.get_tuple(tuple) {
                        if tuple.len() > 0 {
                            if let Data::NUMBER(num) = tuple[0] {
                                if num != 0.0 {
                                    condition = true;
                                }
                            }
                        }
                    }
                }
                _ => ()
            }

            match vm.pop() {
                StackData::MATRIX(index) => {
                    if let Some(matrix) = vm.get_matrix(index) {
                        if condition {
                            if let Some(num) = matrix.get(0, 0) {
                                vm.push(StackData::DATA(Data::NUMBER(num)));
                                return;
                            } 
                        } else if let Some(num) = matrix.get(1, 0) {
                            vm.push(StackData::DATA(Data::NUMBER(num)));
                            return;
                        }
                    }
                    vm.push(StackData::DATA(Data::NOTHING));
                },
                StackData::TUPLE(tuple) => {
                    if let Some(tuple) = vm.get_tuple(tuple) {
                        if condition {
                            if tuple.len() > 0 {
                                vm.push(StackData::DATA(tuple[0]));
                                return;
                            } 
                        } else {
                            if tuple.len() > 1 {
                                vm.push(StackData::DATA(tuple[1]));
                                return;
                            }
                        }
                    }
                    vm.push(StackData::DATA(Data::NOTHING));
                },
                StackData::DATA(data) => match data {
                    Data::NUMBER(num) => {
                        if condition {
                            vm.push(StackData::DATA(Data::NUMBER(num)));
                        } else {
                            vm.push(StackData::DATA(Data::NOTHING));
                        }
                    },
                    _ => {
                        vm.push(StackData::DATA(Data::NOTHING));
                    }
                },
                _ => {
                    vm.push(StackData::DATA(Data::NOTHING));
                }
            }
        },
        Instr::ATX => {
            match vm.pop() {
                StackData::MATRIX(index) => {
                    let mut x = None;
                    let mut y = None;

                    let error_result = StackData::DATA(Data::NOTHING);

                    match vm.pop() {
                        StackData::DATA(data) => match data {
                                Data::NUMBER(num) => {
                                    x = Some(num.floor() as usize);
                                },
                                _ => (),
                        } ,
                        StackData::TUPLE(tuple) => {
                            if let Some(tuple) = vm.get_tuple(tuple) {
                                if tuple.len() > 0 {
                                    x = match tuple[0] {
                                        Data::NUMBER(num) => Some(num.floor() as usize),
                                        _ => None,
                                    }
                                }

                                if tuple.len() > 1  {
                                    y = match tuple[1] {
                                        Data::NUMBER(num) => Some(num.floor() as usize),
                                        _ => None,
                                    }
                                }
                            }
                        },
                        _ => (),
                    }

                    if x == None {
                        vm.push(error_result);
                        return;
                    }

                    if y == None {
                        match vm.pop() {
                            StackData::DATA(data) => match data {
                                    Data::NUMBER(num) => {
                                        y = Some(num.floor() as usize);
                                    },
                                    _ => (),
                            } ,
                            StackData::TUPLE(tuple) => {
                                if let Some(tuple) = vm.get_tuple(tuple) {
                                    if tuple.len() > 0 {
                                        y = match tuple[0] {
                                            Data::NUMBER(num) => Some(num.floor() as usize),
                                            _ => None,
                                        }
                                    } 
                                }
                            },
                            _ => (),
                        }
                    }

                    if y == None {
                        vm.push(error_result);
                        return;
                    }


                    if let Some(x) = x {
                        if let Some(y) = y {
                            if let Some(matrix) = vm.get_matrix(index) {
                                if let Some(number) = matrix.get(x, y) {
                                    vm.push(StackData::DATA(Data::NUMBER(number)));
                                    return;
                                }
                            }
                        }
                    }
                    


                    vm.push(error_result);
                },
                StackData::TUPLE(tuple) => {
                    if let Some(tuple) = vm.get_tuple(tuple) {
                        let mut x = None;
                        match vm.pop() {
                            StackData::DATA(data) => match data {
                                    Data::NUMBER(num) => {
                                        x = Some(num.floor() as usize);
                                    },
                                    _ => (),
                            } ,
                            StackData::TUPLE(tuple) => {
                                if let Some(tuple) = vm.get_tuple(tuple) {
                                    if tuple.len() > 0 {
                                        x = match tuple[0] {
                                            Data::NUMBER(num) => Some(num.floor() as usize),
                                            _ => None,
                                        }
                                    }
                                }
                            },
                            _ => (),
                        }

                        if let Some(x) = x {
                            vm.push(StackData::TUPLE())
                        } else {

                        }
                    }
                    vm.push(StackData::DATA(Data::NOTHING));
                }
            }
        },
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
                    vm.push(StackData::TUPLE(tuple));
                    vm.push(StackData::TUPLE(tuple));
                },
                StackData::LIST(list) => {
                    vm.push(StackData::LIST(list));
                    vm.push(StackData::LIST(list));
                },
            }
        },
        Instr::POD => {
            match vm.pop() {
                StackData::LIST(mut list) => {
                    if let Some(list) = vm.get_list_mut(list) {
                        list.pop();
                    }
                },
                _ => ()
            }
        },
        Instr::MTX(index, size) => {
            let matrix = vm.add_matrix(index, Matrix::new(size));
        },
        Instr::STX => {
            let mut index = None;
            let mut x = None;
            let mut y = None;
            let mut number = None;

            match vm.pop() {
                StackData::DATA(data) => match data {
                        Data::NUMBER(num) => {
                            index = Some(num.floor() as usize)
                        },
                        _ => return,
                },
                _ => return
            }

            match vm.pop() {
                StackData::DATA(data) => match data {
                        Data::NUMBER(num) => {
                            x = Some(num.floor() as usize);
                        },
                        _ => return,
                } ,
                StackData::TUPLE(tuple) => {
                    if let Some(tuple) = vm.get_tuple(tuple) {
                        if tuple.len() > 0 {
                            x = match tuple[0] {
                                Data::NUMBER(num) => Some(num.floor() as usize),
                                _ => return,
                            }
                        } else {
                            return;
                        }
    
                        if tuple.len() > 1  {
                            y = match tuple[1] {
                                Data::NUMBER(num) => Some(num.floor() as usize),
                                _ => return,
                            }
                        }
    
                        if tuple.len() > 2  {
                            number = match tuple[2] {
                                Data::NUMBER(num) => Some(num),
                                _ => return,
                            }
                        }
                    } else {
                        return;
                    }
                },
                _ => return,
            }

            if y == None {
                match vm.pop() {
                    StackData::DATA(data) => match data {
                        Data::NUMBER(num) => {
                            y = Some(num.floor() as usize);
                        },
                        _ => return,
                } ,
                StackData::TUPLE(tuple) => {
                    if let Some(tuple) = vm.get_tuple(tuple) {
                        if tuple.len() > 0 {
                            y = match tuple[0] {
                                Data::NUMBER(num) => Some(num.floor() as usize),
                                _ => return,
                            }
                        } else {
                            return;
                        }

                        if tuple.len() > 1  {
                            number = match tuple[1] {
                                Data::NUMBER(num) => Some(num),
                                _ => return,
                            }
                        }
                    } else {
                        return;
                    }
                },
                _ => return,
                }
            }

            if number == None {
                match vm.pop() {
                    StackData::DATA(data) => match data {
                        Data::NUMBER(num) => {
                            number = Some(num);
                        },
                        _ => return,
                } ,
                StackData::TUPLE(tuple) => {
                    if let Some(tuple) = vm.get_tuple(tuple) {
                        if tuple.len() > 0  {
                            number = match tuple[0] {
                                Data::NUMBER(num) => Some(num),
                                _ => return,
                            }
                        } else {
                            return
                        }
                    } else {
                        return
                    }
                },
                _ => return,
                }
            }

            if let Some(index) = index {
                if let Some(x) = x {
                    if let Some(y) = y {
                        if let Some(number) = number {
                            if let Some(matrix) = vm.get_matrix(index) {
                                matrix.set(x, y, number);
                            }
                        }
                    }
                }
            }
        },
        Instr::EXC => todo!(),
        Instr::RDC => todo!(),
        Instr::EXO => todo!(),
        Instr::ITO => todo!(),
    }
}