use std::{fmt::{self, DebugTuple}, collections::HashMap, vec};

use crate::{lexer::{Token}, parser::{AbstractSyntaxTree, ASTNode, Annotation, ASTNodeType, StatementType}, vm::{Instr, Reference}};


#[macro_use] 
mod macros {
    #[macro_export]
    macro_rules! push_string {
        ($output:expr, $str:tt) => {
            for c in stringify!($str).chars() {
                $output.push(c);
            }
        };
        ($output:expr, $format:tt, $str:tt) => {
            for c in format!(stringify!($format), stringify!($str)).chars() {
                $output.push(c);
            }
        };
    }

    #[macro_export]
    macro_rules! get_annotation {
        ($node:expr, $err:tt, $enum:ident::$pattern:ident($vars:ident $(,$vars2:ident)* )) => {
            {
                let mut i = 0;
                loop {
                    if i < $node.annotations.len() {
                        if let $enum::$pattern($vars $(,$vars2)* ) = &$node.annotations[i]  {
                            break Ok($vars $(,$vars2)* )
                        }
                    } else {
                        break Err(GenerationError::new(stringify!($err)))
                    }
                    i+=1;
                }
            }
        };
        ($node:expr, $err:tt, $enum:ident::$pattern:ident) => {
            {
                let mut i = 0;
                loop {
                    if i < $node.annotations.len() {
                        if let $enum::$pattern = &$node.annotations[i]  {
                            break Ok(())
                        }
                    } else {
                        break Err(GenerationError::new(stringify!($err)))
                    }
                    i+=1;
                }
            }
        };
    }
}
#[derive(Debug, PartialEq)]
pub struct GenerationError {
    msg: String
}

#[allow(dead_code)]
impl GenerationError {
    pub fn new(msg: &str) -> GenerationError {
        GenerationError { msg: msg.to_string() }
    }

    pub fn to_string(&self) -> String {
        self.msg.clone()
    }
}

impl fmt::Display for GenerationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

pub enum JumpType {
    Label(usize),
    Function(usize),
}
pub enum Line {
    Comment(String),
    Instr(Instr),
    Call(usize),
    SetFunc(usize, usize),
    GetFunc(usize),
    SetFuncGlobal(usize, usize),
    GetFuncGlobal(usize),
    Jump(Instr, JumpType),
    Label(usize),
}

struct Naming {
    func_id_front: usize,
    tuple_id_front: usize,
    matrix_id_front: usize,
    label_id_front: usize,
}

impl Naming {
    pub fn new() -> Naming {
        Naming { func_id_front: 0, tuple_id_front: 0, matrix_id_front: 0, label_id_front: 0 }
    }

    pub fn new_func_id(&mut self) -> usize {
        self.func_id_front += 1;
        self.func_id_front - 1
    }

    pub fn new_tuple_id(&mut self) -> usize {
        self.tuple_id_front += 1;
        self.tuple_id_front - 1
    }

    pub fn new_matrix_id(&mut self) -> usize {
        self.matrix_id_front += 1;
        self.matrix_id_front - 1
    }

    pub fn new_label_id(&mut self) -> usize {
        self.label_id_front += 1;
        self.label_id_front - 1
    }
}

struct Function {
    id: usize,
    args: usize,
    code: Vec<Line>,
    funcs: HashMap<usize, Function>,
    pull_through: HashMap<usize, usize>,
    tuples: Vec<usize>,
    matrices: Vec<usize>,
}

impl Function {
    pub fn new(args:usize, id: usize) -> Function {
        Function { id, args, code: vec![], funcs: HashMap::new(), tuples: vec![], matrices: vec![], pull_through: HashMap::new() }
    }
}

struct Code {
    global: Function,
}

impl Code {
    pub fn new(ast: &mut AbstractSyntaxTree) -> Result<Code, GenerationError> {
        let mut naming = Naming::new();
        let scope = get_annotation!(ast.root, "root does not have a naming", Annotation::Scope(_naming))?;
        let mut code = Code{
            global: Function::new(*scope, naming.new_func_id())
        };
        for i in 0..ast.root.children.len() {
            generate_statement(&mut code.global, &mut naming, &ast.root.children[i])?;
        }
        
        Ok(code)
    }
}

fn generate_value(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    match node.node_type {
        ASTNodeType::Exec => generate_eval_function(func, naming, node),
        ASTNodeType::Reduce => generate_eval_function(func, naming, node),
        ASTNodeType::ForEach => generate_eval_function(func, naming, node),
        ASTNodeType::Into => generate_eval_function(func, naming, node),
        ASTNodeType::ExpressionList(_) => generate_expression_list(func, naming, node),
        ASTNodeType::Indexed => generate_indexed(func, naming, node),
        ASTNodeType::Reference(_) => generate_reference(func, naming, node),
        ASTNodeType::Number(num) => {
            func.code.push(Line::Instr(Instr::Push(Reference::Literal(num))));
            Ok(Reference::Stack)
        },
        _ => Err(GenerationError::new("invalid value in expression"))
    }
}

fn generate_eval_function(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    let func_id = generate_function(func, naming, node)?;
    func.code.push(Line::Call(func_id));
    Ok(Reference::Stack)
}

fn generate_function(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    match node.node_type {
        ASTNodeType::Exec => generate_exec(func, naming, node),
        ASTNodeType::Reduce => generate_extended_exec(func, naming, node),
        ASTNodeType::ForEach => generate_extended_exec(func, naming, node),
        ASTNodeType::Into => generate_extended_exec(func, naming, node),
        ASTNodeType::ExpressionList(_) => {
            let mut inner_func = Function::new(0,naming.new_func_id());
            let reference = generate_expression_list(&mut inner_func, naming, node)?;
            inner_func.code.push(Line::Instr(Instr::PopFrame(reference)));
            let id =  inner_func.id;
            func.funcs.insert(inner_func.id, inner_func);
            Ok(id)
        },
        _ => return Err(GenerationError::new("expected function"))
    }

}

fn generate_exec(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    let func_id = generate_func_tuple(func, naming, &node.children[0])?;
    let mut inner_func = func.funcs.get_mut(&func_id).expect("function");
    if let Ok(id) = get_annotation!(node.children[1], "", Annotation::GlobalId(_id)) {
        inner_func.code.push(Line::GetFuncGlobal(*id));
        inner_func.code.push(Line::Instr(Instr::PopFrame(Reference::Stack)));
    } else if let Ok(id) = get_annotation!(node.children[1], "", Annotation::Id(_id)) {
        inner_func.code.push(Line::GetFunc(*id));
        inner_func.code.push(Line::Instr(Instr::PopFrame(Reference::Stack)));
    } else if matches!(node.children[1].node_type, ASTNodeType::ExpressionList(_)){
        let reference = generate_expression_list(&mut inner_func, naming, &node.children[1])?;
        inner_func.code.push(Line::Instr(Instr::PopFrame(reference)));
    }
    Ok(inner_func.id)
}

fn generate_extended_exec(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    let mut inner_func = Function::new(2, naming.new_func_id());
    let exec_id = generate_exec(func, naming, &node.children[1])
    .or_else(|_| {
        let mut inner_func = Function::new(0,naming.new_func_id());
        let reference = generate_expression_list(&mut inner_func, naming, node)?;
        inner_func.code.push(Line::Instr(Instr::PopFrame(reference)));
        let id = inner_func.id;
        func.funcs.insert(inner_func.id, inner_func);
        Ok(id)
    })?;
    let mut lhs = None;
    match &node.children[0].node_type {
        ASTNodeType::Meta => {
            let reference = generate_expression(&mut inner_func, naming, &node.children[0].children[0])?;
            let tuple_id = naming.new_tuple_id();
            inner_func.tuples.push(tuple_id);
            inner_func.code.push(Line::Instr(Instr::CreateTuple(tuple_id, reference)));
            lhs = Some(Reference::Tuple(tuple_id));
        },
        ASTNodeType::Meta2D => {
            let height = generate_expression(&mut inner_func, naming, &node.children[0].children[1])?;
            let width = generate_expression(&mut inner_func, naming, &node.children[0].children[0])?;
            let matrix_id = naming.new_matrix_id();
            inner_func.matrices.push(matrix_id);
            inner_func.code.push(Line::Instr(Instr::CreateMatrix(matrix_id, width, height)));
            lhs = Some(Reference::Matrix(matrix_id));
        },
        ASTNodeType::Tuple(_) => {
            if let Reference::Tuple(id) = generate_tuple(func, naming, &node.children[0])? {
                lhs = Some(Reference::Tuple(id));
            } else {
                return Err(GenerationError::new("expected tuple as lhs"))
            }
        },
        ASTNodeType::Reference(_) => {
            match generate_reference(func, naming, node)? {
                Reference::Tuple(id) => lhs = Some(Reference::Tuple(id)),
                Reference::Matrix(id) => lhs = Some(Reference::Matrix(id)),
                _=> return Err(GenerationError::new("expected reference to be a tuple or matrix")),
            }
        }
        _ => (),
    }

    let exec_func = func.funcs.get(&exec_id).expect("function");
    if let Some(lhs) = lhs {
        // init i and accumulator 
        inner_func.code.push(Line::Instr(Instr::SetArg(0, Reference::Literal(0.0))));
        inner_func.code.push(Line::Instr(Instr::SetArg(1, Reference::Literal(0.0))));
        match node.node_type {
            ASTNodeType::Reduce => {
                let start = naming.new_label_id();
                let end = naming.new_label_id();

                // start of loop
                inner_func.code.push(Line::Label(start));

                // conditional of loop
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenTuple(id)));
                        inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Stack, Reference::Stack), JumpType::Label(end)))
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenMatrix(id)));
                        inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Stack, Reference::Stack), JumpType::Label(end)))
                    }
                    _ => panic!("unreachable"),
                }

                // create argument to pass accumulator
                inner_func.code.push(Line::Instr(Instr::GetArg(1)));

                //create arguments and frame for exec func
                for _ in 0..exec_func.args {
                    inner_func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
                inner_func.code.push(Line::Instr(Instr::PushFrame(exec_func.args+1)));

                // push accumulator onto stack
                inner_func.code.push(Line::Instr(Instr::GetArg(exec_func.args)));

                // call exec
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Function(exec_id)));

                // frame is popped by exec, result is on stack

                // add result to accumulator
                inner_func.code.push(Line::Instr(Instr::GetArg(1)));
                inner_func.code.push(Line::Instr(Instr::Add(Reference::Stack,  Reference::Stack)));
                inner_func.code.push(Line::Instr(Instr::SetArg(1, Reference::Stack)));

                // increment i
                inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                inner_func.code.push(Line::Instr(Instr::Add(Reference::Literal(1.0), Reference::Stack)));
                inner_func.code.push(Line::Instr(Instr::SetArg(0, Reference::Stack)));

                // loop
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Label(start)));

                // end of loop
                inner_func.code.push(Line::Label(end));
                
                inner_func.code.push(Line::Instr(Instr::GetArg(1)));
                inner_func.code.push(Line::Instr(Instr::PopFrame(Reference::Stack)));
            },
            ASTNodeType::ForEach => {
                let start = naming.new_label_id();
                let end = naming.new_label_id();

                // start of loop
                inner_func.code.push(Line::Label(start));

                // conditional of loop
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenTuple(id)));
                        inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Stack, Reference::Stack), JumpType::Label(end)))
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenMatrix(id)));
                        inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Stack, Reference::Stack), JumpType::Label(end)))
                    }
                    _ => panic!("unreachable"),
                }

                // create argument to pass element
                inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::GetTuple(id, Reference::Stack)));
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::GetFlatMatrix(id, Reference::Stack)));

                    }
                    _ => panic!("unreachable"),
                }

                //create arguments and frame for exec func
                for _ in 0..exec_func.args {
                    inner_func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
                inner_func.code.push(Line::Instr(Instr::PushFrame(exec_func.args+1)));

                // push element onto stack
                inner_func.code.push(Line::Instr(Instr::GetArg(exec_func.args)));

                // call exec
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Function(exec_id)));

                // frame is popped by exec, result is on stack

                // increment i
                inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                inner_func.code.push(Line::Instr(Instr::Add(Reference::Literal(1.0), Reference::Stack)));
                inner_func.code.push(Line::Instr(Instr::SetArg(0, Reference::Stack)));

                // loop
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Label(start)));

                // end of loop
                inner_func.code.push(Line::Label(end));
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::PopFrame(Reference::Tuple(id))));
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::PopFrame(Reference::Matrix(id))));

                    }
                    _ => panic!("unreachable"),
                }
            },
            ASTNodeType::Into => {
                let start = naming.new_label_id();
                let end = naming.new_label_id();

                // start of loop
                inner_func.code.push(Line::Label(start));

                // conditional of loop
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenTuple(id)));
                        inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Stack, Reference::Stack), JumpType::Label(end)))
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenMatrix(id)));
                        inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Stack, Reference::Stack), JumpType::Label(end)))
                    }
                    _ => panic!("unreachable"),
                }

                // create argument to pass element
                inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::GetTuple(id, Reference::Stack)));
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::GetFlatMatrix(id, Reference::Stack)));

                    }
                    _ => panic!("unreachable"),
                }

                //create arguments and frame for exec func
                for _ in 0..exec_func.args {
                    inner_func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
                inner_func.code.push(Line::Instr(Instr::PushFrame(exec_func.args+1)));

                // push element onto stack
                inner_func.code.push(Line::Instr(Instr::GetArg(exec_func.args)));

                // call exec
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Function(exec_id)));

                // frame is popped by exec, result is on stack

                // set element to returned
                inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::SetTuple(id, Reference::Stack, Reference::Stack)));
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::SetFlatMatrix(id, Reference::Stack, Reference::Stack)));

                    }
                    _ => panic!("unreachable"),
                }

                // increment i
                inner_func.code.push(Line::Instr(Instr::GetArg(0)));
                inner_func.code.push(Line::Instr(Instr::Add(Reference::Literal(1.0), Reference::Stack)));
                inner_func.code.push(Line::Instr(Instr::SetArg(0, Reference::Stack)));

                // loop
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Label(start)));

                // end of loop
                inner_func.code.push(Line::Label(end));

                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::PopFrame(Reference::Tuple(id))));
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::PopFrame(Reference::Matrix(id))));

                    }
                    _ => panic!("unreachable"),
                }
            },
            _ => return Err(GenerationError::new("expected an extended function type node"))
        }
    } else {
        return Err(GenerationError::new("expected a tuple or matrix as the lhs"))
    }

    let id = inner_func.id;
    func.funcs.insert(inner_func.id, inner_func);
    Ok(id)
}

fn generate_func_tuple(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    let mut inner_func = Function::new(node.children.len(), naming.new_func_id());
    for i in 0..node.children.len() {
        if matches!(node.children[i].node_type, ASTNodeType::Throw) {
            continue;
        }
        if generate_definition(&mut inner_func, naming, &node.children[i]).is_ok() {
            inner_func.args += 1;
        } else {
            let reference = generate_expression(&mut inner_func, naming, &node.children[i])?;

            inner_func.code.push(Line::Instr(Instr::Push(reference)));
        }
    }
    let id = inner_func.id;
    func.funcs.insert(inner_func.id, inner_func);
    Ok(id)
}

fn generate_tuple(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    let tuple_id = naming.new_tuple_id();
    func.tuples.push(tuple_id);
    func.code.push(Line::Instr(Instr::CreateTuple(tuple_id, Reference::Literal(node.children.len() as f64))));
    for i in 0..node.children.len() {
        if matches!(node.children[i].node_type, ASTNodeType::Throw) {
            continue;
        }
        let reference = generate_expression(func, naming, &node.children[i])?;
        func.code.push(Line::Instr(Instr::SetTuple(tuple_id, Reference::Literal(i as f64), reference)));
    }
    Ok(Reference::Tuple(tuple_id))
}

fn generate_expression_list(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    if let ASTNodeType::ExpressionList(size) = node.node_type {
        let mut prev_reference = Reference::None;
        for i in 0..node.children.len() {
            if i % 2 != 0 {
                match &node.children[i].node_type {
                    ASTNodeType::Throw => {
                        if let Reference::Stack = prev_reference {
                            func.code.push(Line::Instr(Instr::Pop))
                        }
                        prev_reference = Reference::None;
                    },
                    ASTNodeType::Push => {
                        if !matches!(prev_reference, Reference::Stack) && !matches!(prev_reference, Reference::None) {
                            func.code.push(Line::Instr(Instr::Push(prev_reference)));
                        }
                        prev_reference = Reference::None;
                    },
                    _ => return Err(GenerationError::new("expected terminator to expression"))
                }
            } else {
                prev_reference = generate_definition(func, naming, &node.children[i])
                .or_else(|_| generate_expression(func, naming, &node.children[i]) )?;
            }
        }
        if matches!(prev_reference, Reference::None) {
            return Ok(Reference::None)
        } else if !matches!(prev_reference, Reference::Stack) {
            func.code.push(Line::Instr(Instr::Push(prev_reference)));
        } 
        Ok(Reference::Stack)
    } else {
        Err(GenerationError::new("expected an expression list"))
    }
}

fn generate_indexed(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    let reference = generate_reference(func, naming, &node.children[0])?;

    if let ASTNodeType::Meta = &node.children[1].node_type {
        let x = generate_expression(func, naming, &node.children[1])?;
        let index = if let Reference::Tuple(index) = reference {
            index
        } else {
            return Err(GenerationError::new("indexed reference must be a tuple"))
        };
        func.code.push(Line::Instr(Instr::GetTuple(index, x)))
    } else if let ASTNodeType::Meta2D = &node.children[1].node_type {
        let x = generate_expression(func, naming, &node.children[1])?;
        let y = generate_expression(func, naming, &node.children[2])?;
        let index = if let Reference::Matrix(index) = reference {
            index
        } else {
            return Err(GenerationError::new("indexed reference must be a matrix"))
        };
        func.code.push(Line::Instr(Instr::GetMatrix(index, x, y)))
    } else {
        return Err(GenerationError::new("Expected meta in indexed value"))
    }

    Ok(Reference::Stack)
}

fn generate_reference(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    let reference = if let ASTNodeType::Reference(reference) = &node.node_type {
        reference
    } else {
        return Err(GenerationError::new("couldn't generate reference"))
    };

    match &reference {
        Token::Peek => {
            func.code.push(Line::Instr(Instr::Duplicate));
        },
        Token::Pop => (),
        Token::Identifier(_) => {
            let is_pullthrough = get_annotation!(node, "", Annotation::PullThrough(_id));
            let is_exec = get_annotation!(node, "", Annotation::Executable).is_ok();
            let is_expression_list = get_annotation!(node, "", Annotation::ExpressionList).is_ok();
            let id = get_annotation!(node, "", Annotation::GlobalId(id))
                .or_else(|_| get_annotation!(node, "definition requires id", Annotation::Id(id)))?;
            
            if let Ok(pull_through_id) = is_pullthrough {
                if !func.pull_through.contains_key(id) {
                    func.pull_through.insert(*id, *pull_through_id);
                }
            }
            
            if is_exec || is_expression_list{
                if get_annotation!(node, "", Annotation::GlobalId(id)).is_ok() {
                    func.code.push(Line::GetFuncGlobal(*id));
                } else {
                    func.code.push(Line::GetFunc(*id));
                }
            } else {
                if get_annotation!(node, "", Annotation::GlobalId(id)).is_ok() {
                    func.code.push(Line::Instr(Instr::GetGlobal(*id)));
                } else {
                    func.code.push(Line::Instr(Instr::GetArg(*id)));
                } 
            }
        },
        _ => return Err(GenerationError::new("invalid reference")),
    }
    
    Ok(Reference::Stack)
}

fn generate_expression(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    match &node.node_type {
        ASTNodeType::Operator(token) => {
            let ref2 = if matches!(token, Token::If) {
                generate_tuple(func, naming, &node.children[1])?
            } else {
                generate_expression(func, naming, &node.children[1])?
            };
            let ref1 = generate_expression(func, naming, &node.children[0])?;
            match token {
                Token::Add => {
                    func.code.push(Line::Instr(Instr::Add(ref1, ref2)));
                },
                Token::Subtract => {
                    func.code.push(Line::Instr(Instr::Subtract(ref1, ref2)));
                },
                Token::Multiply => {
                    func.code.push(Line::Instr(Instr::Multiply(ref1, ref2)));
                },
                Token::Divide => {
                    func.code.push(Line::Instr(Instr::Divide(ref1, ref2)));
                },
                Token::Equals => {
                    func.code.push(Line::Instr(Instr::Equal(ref1, ref2)));
                },
                Token::LessThan => {
                    func.code.push(Line::Instr(Instr::Lesser(ref1, ref2)));
                },
                Token::GreaterThan => {
                    func.code.push(Line::Instr(Instr::Greater(ref1, ref2)));
                },
                Token::LessThanOrEqual => {
                    func.code.push(Line::Instr(Instr::LesserEqual(ref1, ref2)));
                },
                Token::GreaterThanOrEqual => {
                    func.code.push(Line::Instr(Instr::GreaterEqual(ref1, ref2)));
                },
                Token::And => {
                    func.code.push(Line::Instr(Instr::And(ref1, ref2)));
                },
                Token::Or => {
                    func.code.push(Line::Instr(Instr::Or(ref1, ref2)));
                },
                Token::Xor => {
                    func.code.push(Line::Instr(Instr::Xor(ref1, ref2)));
                },
                Token::If => {
                    func.code.push(Line::Instr(Instr::Conditional(ref1, ref2, ref2)));
                },
                _ => return Err(GenerationError::new("could not resolve operator"))
            }

            Ok(Reference::Stack)
        },
        _ => generate_value(func, naming, node)
    }
}


fn generate_definition(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    if !matches!(node.node_type, ASTNodeType::Set(_)) {
        return Err(GenerationError::new("node is not a definition"));
    }
    let is_exec = get_annotation!(node.children[0], "", Annotation::Executable).is_ok();
    let is_expression_list = get_annotation!(node.children[0], "", Annotation::ExpressionList).is_ok();
    let id = get_annotation!(node.children[0], "", Annotation::GlobalId(id))
        .or_else(|_| get_annotation!(node.children[0], "definition requires id", Annotation::Id(id)))?;

    if is_exec {
        let func_id = generate_function(func, naming, &node.children[1])?;
        let function = func.funcs.get(&func_id).expect("function");
        let func_id = function.id;
        if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
            func.code.push(Line::SetFuncGlobal(*id, func_id));
        } else {
            func.code.push(Line::SetFunc(*id, func_id));
        }
    } else if is_expression_list {
        let mut inner_func = Function::new(0, naming.new_func_id());
        let func_id = inner_func.id;
        let expression_list = generate_expression_list(&mut inner_func, naming, node)?;
        inner_func.code.push(Line::Instr(Instr::PopFrame(expression_list)));
        func.funcs.insert(inner_func.id, inner_func);
        if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
            func.code.push(Line::SetFuncGlobal(*id, func_id));
        } else {
            func.code.push(Line::SetFunc(*id, func_id));
        }
    } else {
        let mut reference =match &node.children[1].node_type {
            ASTNodeType::Meta => {
                let reference = generate_expression(func, naming, &node.children[1].children[0])?;
                let tuple_id = naming.new_tuple_id();
                func.tuples.push(tuple_id);
                func.code.push(Line::Instr(Instr::CreateTuple(tuple_id, reference)));
                Reference::Tuple(tuple_id)
            },
            ASTNodeType::Meta2D => {
                let height = generate_expression(func, naming, &node.children[1].children[1])?;
                let width = generate_expression(func, naming, &node.children[1].children[0])?;
                let matrix_id = naming.new_matrix_id();
                func.matrices.push(matrix_id);
                func.code.push(Line::Instr(Instr::CreateMatrix(matrix_id, width, height)));
                Reference::Matrix(matrix_id)
            },
            ASTNodeType::Tuple(_) => {
                if let Reference::Tuple(id) = generate_tuple(func, naming, &node.children[1])? {
                    Reference::Tuple(id)
                } else {
                    return Err(GenerationError::new("expected tuple as lhs"))
                }
            },
            _ => generate_expression(func, naming, &node.children[1])?,
        };

        if matches!(node.node_type, ASTNodeType::Set(Token::AddAndSet)) || 
            matches!(node.node_type, ASTNodeType::Set(Token::SubtractAndSet)) || 
            matches!(node.node_type, ASTNodeType::Set(Token::MultiplyAndSet)) || 
            matches!(node.node_type, ASTNodeType::Set(Token::DivideAndSet)) {
            if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
                func.code.push(Line::Instr(Instr::GetGlobal(*id)));
            } else {
                func.code.push(Line::Instr(Instr::GetArg(*id)));
            }
        }
        
        match node.node_type {
            ASTNodeType::Set(Token::AddAndSet) => {
                func.code.push(Line::Instr(Instr::Add(Reference::Stack, reference)));
                reference = Reference::Stack;
            },
            ASTNodeType::Set(Token::SubtractAndSet) => {
                func.code.push(Line::Instr(Instr::Subtract(Reference::Stack, reference)));
                reference = Reference::Stack;
            },
            ASTNodeType::Set(Token::MultiplyAndSet) => {
                func.code.push(Line::Instr(Instr::Multiply(Reference::Stack, reference)));
                reference = Reference::Stack;
            },
            ASTNodeType::Set(Token::DivideAndSet) => {
                func.code.push(Line::Instr(Instr::Divide(Reference::Stack, reference)));
                reference = Reference::Stack;
            },
            _ => ()
        }
        
        if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
            func.code.push(Line::Instr(Instr::SetGlobal(*id, reference)));
        } else {
            func.code.push(Line::Instr(Instr::SetArg(*id, reference)));
        }

        if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
            func.code.push(Line::Instr(Instr::GetGlobal(*id)));
        } else {
            func.code.push(Line::Instr(Instr::GetArg(*id)));
        }
    }
    Ok(Reference::None)
}

fn generate_statement(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<(), GenerationError> {
    if generate_definition(func, naming, &node.children[0]).is_ok() {
        return Ok(())
    }
    
    generate_expression(func, naming, &node.children[0])?;

    if let ASTNodeType::Statement(statement_type) = &node.node_type {
        match statement_type {
            StatementType::Push => (),
            StatementType::Throw => func.code.push(Line::Instr(Instr::Pop)),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{parser::AbstractSyntaxTree, lexer::Tokenizer, semantic::validate};

    use super::Code;


    #[test] 
    fn test() {
        let mut tree = &mut AbstractSyntaxTree::new(&mut Tokenizer::new(r"
            var0: (1; 0);
            var1: 10;
            var1;
            var2: 2*2;
            var2: 4;
            var3: (1; x:2; y:1) -> [(y) -> [1]];
            var4: {1;2};
            2* 2 ? (2; 2);
        "));

        if let Err(err) = validate(&mut tree) {
            panic!("{}", err.to_string());
        }

        let code = Code::new(&mut tree);

        match code {
            Ok(code) => {
                
            },
            Err(err) => panic!("{}", err.to_string())
        }
    }
}