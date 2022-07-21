use std::{fmt::{self}, collections::HashMap, vec};

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
    CallInline(usize, usize),
    ExecRef(Instr, usize),
    SetFunc(usize, usize),
    SetFuncGlobal(usize, usize),
    Jump(Instr, JumpType),
    Label(usize),
}

struct Naming {
    func_id_front: usize,
    tuple_id_front: usize,
    matrix_id_front: usize,
    label_id_front: usize,
    heap_id_front: usize,
}

impl Naming {
    pub fn new() -> Naming {
        Naming { func_id_front: 0, tuple_id_front: 0, matrix_id_front: 0, label_id_front: 0, heap_id_front: 0 }
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

    pub fn new_heap_id(&mut self) -> usize {
        self.heap_id_front += 1;
        self.heap_id_front - 1
    }
}

struct Function {
    id: usize,
    args: usize,
    code: Vec<Line>,
    funcs: HashMap<usize, Function>,
    pull_through: HashMap<usize, (usize, usize)>,
    tuples: Vec<usize>,
    matrices: Vec<usize>,
    ret: Reference,
}

impl Function {
    pub fn new(args:usize, id: usize) -> Function {
        Function { id, args, code: vec![], funcs: HashMap::new(), tuples: vec![], matrices: vec![], pull_through: HashMap::new(), ret: Reference::None }
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
            global: Function::new(0, naming.new_func_id())
        };
        for i in 0..ast.root.children.len() {
            generate_statement(&mut code.global, &mut naming, &ast.root.children[i])?;
        }
        
        Ok(code)
    }

    pub fn to_instrs(&self) -> (usize, Vec<Instr>) {
        let mut out = Vec::with_capacity(self.global.code.len() * 2);
        let mut func_indices: HashMap<usize, (usize, usize)> = HashMap::new();
        let mut label_indices: HashMap<usize, usize> = HashMap::new();

        (Code::create_function(&self.global, &mut out, &mut func_indices, &mut label_indices), out)
    } 

    fn create_function(func: &Function, out: &mut Vec<Instr>, func_indices: &mut HashMap<usize, (usize, usize)>, label_indices: &mut HashMap<usize, usize>) -> usize {
        for key in func.funcs.keys()  {
            Code::create_function(func.funcs.get(key).unwrap(), out, func_indices, label_indices);
        }
        let mut index = out.len();
        for line in &func.code {
            match line {
                Line::Comment(_) => continue,
                Line::Label(label_id) => {
                    label_indices.insert(*label_id, index);
                },
                _ => index += 1,
            }
        }
        let code_start = out.len();
        func_indices.insert(func.id, (func.args, code_start));
        for line in &func.code {
            match line {
                Line::Comment(_) => continue,
                Line::Label(_) => continue,
                Line::Instr(instr) => out.push(instr.clone()),
                Line::Call(func_id) => {
                    let (argc, index) = func_indices.get(func_id).expect("should already be defined");
                    out.push(Instr::PushFrame(*argc, *index));
                },
                Line::CallInline(argc, label_id) => {
                    let index = label_indices.get(label_id).expect("should already be defined");
                    out.push(Instr::PushInlineFrame(*argc, *index));
                },
                Line::ExecRef(instr, func_id) => {
                    let (argc, index) = func_indices.get(func_id).expect("should already be defined");
                    out.push(match instr {
                        Instr::Add(_, a) => Instr::Add(Reference::Executable(*argc, *index), *a),
                        Instr::Subtract(_, a) => Instr::Subtract(Reference::Executable(*argc, *index), *a),
                        Instr::Multiply(_, a) => Instr::Multiply(Reference::Executable(*argc, *index), *a),
                        Instr::Divide(_, a) => Instr::Divide(Reference::Executable(*argc, *index), *a),
                        Instr::Equal(_, a) => Instr::Equal(Reference::Executable(*argc, *index), *a),
                        Instr::Lesser(_, a) => Instr::Lesser(Reference::Executable(*argc, *index), *a),
                        Instr::Greater(_, a) => Instr::Greater(Reference::Executable(*argc, *index), *a),
                        Instr::LesserEqual(_, a) => Instr::LesserEqual(Reference::Executable(*argc, *index), *a),
                        Instr::GreaterEqual(_, a) => Instr::GreaterEqual(Reference::Executable(*argc, *index), *a),
                        Instr::Not(_) => Instr::Not(Reference::Executable(*argc, *index)),
                        Instr::And(_, a) => Instr::And(Reference::Executable(*argc, *index), *a),
                        Instr::Or(_, a) => Instr::Or(Reference::Executable(*argc, *index), *a),
                        Instr::Xor(_, a) => Instr::Xor(Reference::Executable(*argc, *index), *a),
                        Instr::Conditional(_, a, b) => todo!(),
                        Instr::Push(_) => Instr::Push(Reference::Executable(*argc, *index)),
                        Instr::SetArg(a, _) => Instr::SetArg(*a, Reference::Executable(*argc, *index)),
                        Instr::SetGlobal(a, _) => Instr::SetGlobal(*a, Reference::Executable(*argc, *index)),
                        Instr::ExecRef(_) => Instr::ExecRef(Reference::Executable(*argc, *index)),
                        Instr::Alloc(a, _) => Instr::Alloc(*a, Reference::Executable(*argc, *index)),
                        Instr::SetPoint(a, _) => Instr::SetPoint(*a, Reference::Executable(*argc, *index)),
                        _ => todo!(),
                    })
                },
                Line::SetFunc(id, func_id) => {
                    let (argc, index) = func_indices.get(func_id).expect("should already be defined");
                    out.push(Instr::SetArg(*id, Reference::Executable(*argc, *index)))
                },
                Line::SetFuncGlobal(id, func_id) => {
                    let (argc, index) = func_indices.get(func_id).expect("should already be defined");
                    out.push(Instr::SetGlobal(*id, Reference::Executable(*argc, *index)))
                },
                Line::Jump(instr, jump_type) => {
                    let index = match jump_type {
                        JumpType::Label(label_id) => {
                            *label_indices.get(label_id).expect("should already be defined")
                        },
                        JumpType::Function(func_id) => {
                            let (_, index) = func_indices.get(func_id).expect("should already be defined");
                            *index
                        },
                    };
                    match instr {
                        Instr::Jump(_) => out.push(Instr::Jump(index)),
                        Instr::JumpEqual(_, a, b) =>  out.push(Instr::JumpEqual(index, *a, *b)),
                        Instr::JumpNotEqual(_, a, b) => out.push(Instr::JumpNotEqual(index, *a, *b)),
                        Instr::JumpLesser(_, a, b) => out.push(Instr::JumpLesser(index, *a, *b)),
                        Instr::JumpGreater(_, a, b) => out.push(Instr::JumpGreater(index, *a, *b)),
                        Instr::JumpLesserOrEqual(_, a, b) => out.push(Instr::JumpLesserOrEqual(index, *a, *b)),
                        Instr::JumpGreaterOrEqual(_, a, b) => out.push(Instr::JumpGreaterOrEqual(index, *a, *b)),
                        _ => panic!("unreachable"),
                    }
                },
            }
        }

        for key in func.funcs.keys()  {
            let func = func.funcs.get(key).expect("should already be defined");
            for tuple in &func.tuples {
                out.push(Instr::RemoveTuple(*tuple));
            }
            for matrix in &func.matrices {
                out.push(Instr::RemoveMatrix(*matrix));
            }
            for (_,(_, heap_id)) in &func.pull_through {
                out.push(Instr::Free(*heap_id));
            }
        }

        out.push(Instr::PopFrame(func.ret));

        return code_start;
    }
}

impl ToString for Code {
    fn to_string(&self) -> String {
        let (start, instrs) = self.to_instrs();
        let mut out: Vec<char> = format!("start index: {}\n", start).chars().collect();
        let indent_size = instrs.len().to_string().len() + 1;
        for (i, instr) in instrs.iter().enumerate() {
            let line_num = format!("{}", i);
            out.append(&mut line_num.chars().collect());
            for _ in 0..indent_size-line_num.len() {
                out.push(' ');
            }
            out.append(&mut instr.to_string().chars().collect());
            out.push('\n');
        }

        out.iter().collect()
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
    generate_function(true, func, naming, node)?;
    Ok(Reference::Stack)
}

fn generate_function(is_eval: bool, func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    match node.node_type {
        ASTNodeType::Exec => generate_exec(is_eval, func, naming, node),
        ASTNodeType::Reduce => generate_extended_exec(is_eval, func, naming, node),
        ASTNodeType::ForEach => generate_extended_exec(is_eval, func, naming, node),
        ASTNodeType::Into => generate_extended_exec(is_eval, func, naming, node),
        ASTNodeType::ExpressionList(_) => {
            let mut inner_func = Function::new(0,naming.new_func_id());
            let reference = generate_expression_list(&mut inner_func, naming, node)?;
            inner_func.ret = reference;
            let id =  inner_func.id;
            func.funcs.insert(inner_func.id, inner_func);
            Ok(id)
        },
        _ => return Err(GenerationError::new("expected function"))
    }
}

fn generate_exec(is_eval: bool, func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    let func_tuple_ret = generate_func_tuple(is_eval, func, naming, &node.children[0])?;
    if !is_eval {
        let func_id = func_tuple_ret;
        let mut inner_func = func.funcs.get_mut(&func_id).expect("function");
        if let Ok(id) = get_annotation!(node.children[1], "", Annotation::GlobalId(_id)) {
            let argc = *get_annotation!(node.children[1], "expected executable to take args", Annotation::Argc(_args))?;
            if inner_func.args < argc {
                for _ in 0..argc-inner_func.args {
                    inner_func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
            }
            inner_func.code.push(Line::Instr(Instr::ExecRef(Reference::Global(*id))));
            inner_func.ret = Reference::Stack;
        } else if let Ok(id) = get_annotation!(node.children[1], "", Annotation::Id(_id)) {
            let argc = *get_annotation!(node.children[1], "expected executable to take args", Annotation::Argc(_args))?;
            if inner_func.args < argc {
                for _ in 0..argc-inner_func.args {
                    inner_func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
            }
            inner_func.code.push(Line::Instr(Instr::ExecRef(Reference::Argument(*id))));
            inner_func.ret = Reference::Stack;
        } else if matches!(node.children[1].node_type, ASTNodeType::ExpressionList(_)){
            let reference = generate_expression_list(&mut inner_func, naming, &node.children[1])?;
            inner_func.ret = reference;
        }
        Ok(inner_func.id)
    } else {
        let outer_argc = func_tuple_ret;
        if let Ok(id) = get_annotation!(node.children[1], "", Annotation::GlobalId(_id)) {
            let argc = *get_annotation!(node.children[1], "expected executable to take args", Annotation::Argc(_args))?;
            if outer_argc < argc {
                for _ in 0..argc-outer_argc {
                    func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
            }
            func.code.push(Line::Instr(Instr::ExecRef(Reference::Global(*id))));
        } else if let Ok(id) = get_annotation!(node.children[1], "", Annotation::Id(_id)) {
            let argc = *get_annotation!(node.children[1], "expected executable to take args", Annotation::Argc(_args))?;
            if outer_argc < argc {
                for _ in 0..argc-outer_argc {
                    func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
            }
            func.code.push(Line::Instr(Instr::ExecRef(Reference::Argument(*id))));
        } else if matches!(node.children[1].node_type, ASTNodeType::ExpressionList(_)){
            let mut inner_func = Function::new(outer_argc, naming.new_func_id());
            let reference = generate_expression_list(&mut inner_func, naming, &node.children[1])?;
            inner_func.ret = reference;
            func.code.push(Line::Call(inner_func.id));
            func.funcs.insert(inner_func.id, inner_func);
        }
        Ok(func.id)
    }
}

fn generate_extended_exec(is_eval: bool, func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    let mut inner_func = Function::new(2, naming.new_func_id());
    let exec_id = generate_exec(is_eval, func, naming, &node.children[1])
    .or_else(|_| {
        let mut inner_func = Function::new(0,naming.new_func_id());
        let reference = generate_expression_list(&mut inner_func, naming, node)?;
        inner_func.ret = reference;
        let id = inner_func.id;
        func.funcs.insert(inner_func.id, inner_func);
        Ok(id)
    })?;
    let mut lhs = None;
    match &node.children[0].node_type {
        ASTNodeType::Meta => {
            let reference = generate_expression(&mut inner_func, naming, &node.children[0].children[0])?;
            let tuple_id = naming.new_tuple_id();
            func.tuples.push(tuple_id);
            inner_func.code.push(Line::Instr(Instr::CreateTuple(tuple_id, reference)));
            lhs = Some(Reference::Tuple(tuple_id));
        },
        ASTNodeType::Meta2D => {
            let height = generate_expression(&mut inner_func, naming, &node.children[0].children[1])?;
            let width = generate_expression(&mut inner_func, naming, &node.children[0].children[0])?;
            let matrix_id = naming.new_matrix_id();
            func.matrices.push(matrix_id);
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
                let end_of_func = naming.new_label_id();

                // start of loop
                inner_func.code.push(Line::Label(start));

                // conditional of loop
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenTuple(id)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Argument(0), Reference::Stack), JumpType::Label(end)))
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenMatrix(id)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Argument(0), Reference::Stack), JumpType::Label(end)))
                    }
                    _ => panic!("unreachable"),
                }

                // create argument to pass accumulator
                inner_func.code.push(Line::Instr(Instr::GetArg(1)));

                //create arguments and frame for exec func
                for _ in 0..exec_func.args {
                    inner_func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
                inner_func.code.push(Line::CallInline(exec_func.args+1, end_of_func));

                // push accumulator onto stack
                inner_func.code.push(Line::Instr(Instr::GetArg(exec_func.args)));

                // call exec
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Function(exec_id)));

                // frame is popped by exec, result is on stack
                inner_func.code.push(Line::Label(end_of_func));

                // add result to accumulator
                inner_func.code.push(Line::Instr(Instr::Add(Reference::Argument(1),  Reference::Stack)));
                inner_func.code.push(Line::Instr(Instr::SetArg(1, Reference::Stack)));

                // increment i
                inner_func.code.push(Line::Instr(Instr::Add(Reference::Literal(1.0), Reference::Argument(0))));
                inner_func.code.push(Line::Instr(Instr::SetArg(0, Reference::Stack)));

                // loop
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Label(start)));

                // end of loop
                inner_func.code.push(Line::Label(end));
                
                inner_func.code.push(Line::Instr(Instr::GetArg(1)));
                inner_func.ret = Reference::Stack;
            },
            ASTNodeType::ForEach => {
                let start = naming.new_label_id();
                let end = naming.new_label_id();
                let end_of_func = naming.new_label_id();

                // start of loop
                inner_func.code.push(Line::Label(start));

                // conditional of loop
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenTuple(id)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Argument(0), Reference::Stack), JumpType::Label(end)))
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenMatrix(id)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Argument(0), Reference::Stack), JumpType::Label(end)))
                    }
                    _ => panic!("unreachable"),
                }

                // create argument to pass element
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::GetTuple(id, Reference::Argument(0))));
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::GetFlatMatrix(id, Reference::Argument(0))));

                    }
                    _ => panic!("unreachable"),
                }

                //create arguments and frame for exec func
                for _ in 0..exec_func.args {
                    inner_func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
                inner_func.code.push(Line::CallInline(exec_func.args+1, end_of_func));

                // push element onto stack
                inner_func.code.push(Line::Instr(Instr::GetArg(exec_func.args)));

                // call exec
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Function(exec_id)));

                // frame is popped by exec, result is on stack
                inner_func.code.push(Line::Label(end_of_func));

                // increment i
                inner_func.code.push(Line::Instr(Instr::Add(Reference::Literal(1.0), Reference::Argument(0))));
                inner_func.code.push(Line::Instr(Instr::SetArg(0, Reference::Stack)));

                // loop
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Label(start)));

                // end of loop
                inner_func.code.push(Line::Label(end));
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.ret = Reference::Tuple(id);
                    }
                    Reference::Matrix(id) => {
                        inner_func.ret = Reference::Matrix(id);

                    }
                    _ => panic!("unreachable"),
                }
            },
            ASTNodeType::Into => {
                let start = naming.new_label_id();
                let end = naming.new_label_id();
                let end_of_func = naming.new_label_id();

                // start of loop
                inner_func.code.push(Line::Label(start));

                // conditional of loop
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenTuple(id)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Argument(0), Reference::Stack), JumpType::Label(end)))
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::LenMatrix(id)));
                        inner_func.code.push(Line::Jump(Instr::JumpGreaterOrEqual(0, Reference::Argument(0), Reference::Stack), JumpType::Label(end)))
                    }
                    _ => panic!("unreachable"),
                }

                // create argument to pass element
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::GetTuple(id, Reference::Argument(1))));
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::GetFlatMatrix(id, Reference::Argument(1))));

                    }
                    _ => panic!("unreachable"),
                }

                //create arguments and frame for exec func
                for _ in 0..exec_func.args {
                    inner_func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
                inner_func.code.push(Line::CallInline(exec_func.args+1, end_of_func));

                // push element onto stack
                inner_func.code.push(Line::Instr(Instr::GetArg(exec_func.args)));

                // call exec
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Function(exec_id)));

                // frame is popped by exec, result is on stack
                inner_func.code.push(Line::Label(end_of_func));

                // set element to returned
                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.code.push(Line::Instr(Instr::SetTuple(id, Reference::Argument(0), Reference::Stack)));
                    }
                    Reference::Matrix(id) => {
                        inner_func.code.push(Line::Instr(Instr::SetFlatMatrix(id, Reference::Argument(0), Reference::Stack)));

                    }
                    _ => panic!("unreachable"),
                }

                // increment i
                inner_func.code.push(Line::Instr(Instr::Add(Reference::Literal(1.0), Reference::Argument(0))));
                inner_func.code.push(Line::Instr(Instr::SetArg(0, Reference::Stack)));

                // loop
                inner_func.code.push(Line::Jump(Instr::Jump(0), JumpType::Label(start)));

                // end of loop
                inner_func.code.push(Line::Label(end));

                match lhs {
                    Reference::Tuple(id) => {
                        inner_func.ret = Reference::Tuple(id);
                    }
                    Reference::Matrix(id) => {
                        inner_func.ret = Reference::Matrix(id);

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

fn generate_func_tuple(is_eval: bool, func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    let mut arg_index = 0;
    if is_eval {
        for i in 0..node.children.len() {
            if matches!(node.children[i].node_type, ASTNodeType::Throw) {
                continue;
            }
            if generate_func_definition(arg_index, is_eval, func, naming, &node.children[i]).is_ok() {
                arg_index += 1;
            } else {
                let reference = generate_expression(func, naming, &node.children[i])?;
                
                if !matches!(reference, Reference::None) && !matches!(reference, Reference::Stack) {
                    func.code.push(Line::Instr(Instr::Push(reference)));
                }
            } 
        }
        Ok(arg_index)
    } else {
        let mut inner_func = Function::new(0, naming.new_func_id());
        for i in 0..node.children.len() {
            if matches!(node.children[i].node_type, ASTNodeType::Throw) {
                continue;
            }
            if generate_func_definition(arg_index, is_eval, &mut inner_func, naming, &node.children[i]).is_ok() {
                arg_index += 1;
            } else {
                let reference = generate_expression(&mut inner_func, naming, &node.children[i])?;
    
                if !matches!(reference, Reference::None) && !matches!(reference, Reference::Stack) {
                    inner_func.code.push(Line::Instr(Instr::Push(reference)));
                }
            } 
        }
        
        inner_func.args = arg_index;
    
        for (_, (pull_through_id, heap_id)) in &inner_func.pull_through {
            func.code.push(Line::Instr(Instr::Alloc(*heap_id, Reference::Argument(*pull_through_id))))
        }
    
        let id = inner_func.id;
        func.funcs.insert(inner_func.id, inner_func);
        Ok(id)
    }
}

fn generate_tuple(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    let tuple_id = naming.new_tuple_id();
    if let ASTNodeType::Tuple(size) = node.node_type {
        func.tuples.push(tuple_id);
        func.code.push(Line::Instr(Instr::CreateTuple(tuple_id, Reference::Literal(size as f64))));
        let mut tuple_index = 0;
        for i in 0..node.children.len() {
            if matches!(node.children[i].node_type, ASTNodeType::Throw) {
                continue;
            }
            let reference = generate_expression(func, naming, &node.children[i])?;
            func.code.push(Line::Instr(Instr::SetTuple(tuple_id, Reference::Literal(tuple_index as f64), reference)));
            tuple_index += 1
        }
        Ok(Reference::Tuple(tuple_id))
    } else {
        Err(GenerationError::new("expected tuple"))
    }
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
            Ok(Reference::Stack)
        },
        Token::Pop => Ok(Reference::Stack),
        Token::Identifier(_) => {
            let is_pullthrough = get_annotation!(node, "", Annotation::PullThrough(_id));
            let mut global = true;
            let id = get_annotation!(node, "", Annotation::GlobalId(id))
                .or_else(|_| {global = false; get_annotation!(node, "definition requires id", Annotation::Id(id))})?;
            
            if let Ok(pull_through_id) = is_pullthrough {
                let id = if let Some((_, heap_id)) = func.pull_through.get(id) {
                    *heap_id
                } else {
                    let heap_id = naming.new_heap_id();
                    func.pull_through.insert(*id, (*pull_through_id, heap_id));
                    heap_id
                };
                Ok(Reference::Heap(id))
            } else if get_annotation!(node, "", Annotation::GlobalId(id)).is_ok() {
                Ok(Reference::Global(*id))
            } else {
                Ok(Reference::Argument(*id))
            }
        },
        _ => return Err(GenerationError::new("invalid reference")),
    }
    
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
        let func_id = generate_function(false, func, naming, &node.children[1])?;
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
        inner_func.ret = expression_list;
        func.funcs.insert(inner_func.id, inner_func);
        if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
            func.code.push(Line::SetFuncGlobal(*id, func_id));
        } else {
            func.code.push(Line::SetFunc(*id, func_id));
        }
    } else {
        let mut rhs =match &node.children[1].node_type {
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

        let lhs = if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
                Reference::Global(*id)
            } else {
                Reference::Argument(*id)
            };
        
        match node.node_type {
            ASTNodeType::Set(Token::AddAndSet) => {
                func.code.push(Line::Instr(Instr::Add(lhs, rhs)));
                rhs = Reference::Stack;
            },
            ASTNodeType::Set(Token::SubtractAndSet) => {
                func.code.push(Line::Instr(Instr::Subtract(lhs, rhs)));
                rhs = Reference::Stack;
            },
            ASTNodeType::Set(Token::MultiplyAndSet) => {
                func.code.push(Line::Instr(Instr::Multiply(lhs, rhs)));
                rhs = Reference::Stack;
            },
            ASTNodeType::Set(Token::DivideAndSet) => {
                func.code.push(Line::Instr(Instr::Divide(lhs, rhs)));
                rhs = Reference::Stack;
            },
            _ => ()
        }
        
        if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
            func.code.push(Line::Instr(Instr::SetGlobal(*id, rhs)));
        } else {
            func.code.push(Line::Instr(Instr::SetArg(*id, rhs)));
        }
    }
    Ok(Reference::None)
}

fn generate_func_definition(arg_index: usize, is_eval: bool, func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<(), GenerationError> {
    if !matches!(node.node_type, ASTNodeType::Set(_)) {
        return Err(GenerationError::new("node is not a definition"));
    }
    let is_exec = get_annotation!(node.children[0], "", Annotation::Executable).is_ok();
    let is_expression_list = get_annotation!(node.children[0], "", Annotation::ExpressionList).is_ok();
    let id = get_annotation!(node.children[0], "", Annotation::GlobalId(id))
        .or_else(|_| get_annotation!(node.children[0], "definition requires id", Annotation::Id(id)))?;

    let mut end = 0;
    if !is_eval {
        end = naming.new_label_id();
        func.code.push(Line::Instr(Instr::GetArg(arg_index)));
        func.code.push(Line::Instr(Instr::Duplicate));
        func.code.push(Line::Jump(Instr::JumpNotEqual(0, Reference::None, Reference::Stack), JumpType::Label(end)));
        func.code.push(Line::Instr(Instr::Pop));
    }

    let reference = if is_exec {
        let func_id = generate_function(is_eval, func, naming, &node.children[1])?;
        func.code.push(Line::ExecRef(Instr::Push(Reference::None), func_id));
        Reference::Stack
    } else if is_expression_list {
        let mut inner_func = Function::new(0, naming.new_func_id());
        let expression_list = generate_expression_list(&mut inner_func, naming, node)?;
        inner_func.ret = expression_list;

        let func_id = inner_func.id;
        func.funcs.insert(inner_func.id, inner_func);

        func.code.push(Line::ExecRef(Instr::Push(Reference::None), func_id));
        Reference::Stack
    } else {
        match &node.children[1].node_type {
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
        }
    };

    if !is_eval {
        if !matches!(reference,Reference::Stack) {
            func.code.push(Line::Instr(Instr::Push(reference)));
        }

        func.code.push(Line::Label(end));
    }

    Ok(())
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
            func1: (x:2) -> [],
            () -> func1,
        "));


        if let Err(err) = validate(&mut tree) {
            panic!("{}", err.to_string());
        }
        println!("{}", tree.to_pretty_string(true));


        let code = Code::new(&mut tree);

        match code {
            Ok(code) => {
                println!("{}", code.to_string())
            },
            Err(err) => panic!("{}", err.to_string())
        }
    }
}