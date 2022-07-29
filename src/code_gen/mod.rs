use std::{fmt::{self}, collections::HashMap, vec};

use crate::{lexer::{Token}, parser::{AbstractSyntaxTree, ASTNode, Annotation, ASTNodeType, StatementType}, vm::{Instr, Reference, InstrError, Call, create_call_map}};


#[macro_use] 
mod macros {
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
                        break create_generation_error!($node, $err)
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
                        break create_generation_error!($node, $err)
                    }
                    i+=1;
                }
            }
        };
    }

    
    macro_rules! create_generation_error {
        ($node:expr, $err:tt) => {
            {
                let mut err = stringify!($err).to_string();
                for i in 0..$node.annotations.len() {
                    if let Annotation::DebugInfo(line, line_index) = $node.annotations[i] {
                        err = format!("{} at line: {} and index: {}", err, line, line_index);
                        break;
                    }
                }
                Err(GenerationError::new(&err))
            }
        };
    }
}

#[derive(Debug, PartialEq)]
pub struct GenerationError {
    msg: String
}


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

#[derive(Debug)]
pub enum JumpType {
    Label(usize),
    #[allow(dead_code)]
    Function(usize),
}

#[derive(Debug)]
pub enum Line {
    #[allow(dead_code)]
    Comment(String),
    Instr(Instr),
    Call(usize),
    Return(Reference),
    #[allow(dead_code)]
    CallInline(usize, usize),
    ExecRef(Instr, usize),
    Jump(Instr, JumpType),
    Label(usize),
}

struct Naming {
    func_id_front: usize,
    tuple_id_front: usize,
    label_id_front: usize,
    heap_id_front: usize,
    call_map: HashMap<String, (usize, &'static dyn Call)>,
}

impl Naming {
    pub fn new() -> Naming {
        Naming { func_id_front: 0, tuple_id_front: 0, label_id_front: 0, heap_id_front: 0, call_map: create_call_map() }
    }

    pub fn new_func_id(&mut self) -> usize {
        self.func_id_front += 1;
        self.func_id_front - 1
    }

    pub fn new_tuple_id(&mut self) -> usize {
        self.tuple_id_front += 1;
        self.tuple_id_front - 1
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
    argc: usize,
    code: Vec<Line>,
    funcs: HashMap<usize, Function>,
    heap: Vec<usize>,
    tuples: Vec<usize>,
    ret: Reference,
}

impl Function {
    pub fn new(args:usize, id: usize) -> Function {
        Function { id, argc: args, code: vec![], funcs: HashMap::new(), heap: vec![], tuples: vec![], ret: Reference::None }
    }
}

pub struct Code {
    global: Function,
}

impl Code {
    pub fn new(ast: &mut AbstractSyntaxTree) -> Result<Code, GenerationError> {
        let mut naming = Naming::new();
        let mut code = Code{
            global: Function::new(0, naming.new_func_id())
        };
        for i in 0..ast.root.children.len() {
            generate_statement(&mut code.global, &mut naming, &ast.root.children[i])?;
        }
        
        code.global.argc = *get_annotation!(ast.root, "expected scope annotation on root", Annotation::Scope(scope))?;

        Ok(code)
    }

    pub fn from_string_to_instr(code: &str) -> Result<Vec<Instr>, InstrError> {
        let mut instrs = vec![];
        for line in code.split("\n") {
            if line.len() == 0 {
                continue;
            }
            let instr = Instr::from_str(&mut line.chars().peekable())?;
            instrs.push(instr);
        }
        Ok(instrs)
    }

    pub fn to_instrs(&self) -> (usize, usize, Vec<Instr>) {
        let mut out = Vec::with_capacity(self.global.code.len() * 2);
        let mut func_indices: HashMap<usize, (usize, usize)> = HashMap::new();
        let mut label_indices: HashMap<usize, usize> = HashMap::new();

        (self.global.argc, Code::create_function(true,&self.global, &mut out, &mut func_indices, &mut label_indices), out)
    } 

    fn create_function(is_global: bool, func: &Function, out: &mut Vec<Instr>, func_indices: &mut HashMap<usize, (usize, usize)>, label_indices: &mut HashMap<usize, usize>) -> usize {
        for key in func.funcs.keys()  {
            Code::create_function(false, func.funcs.get(key).unwrap(), out, func_indices, label_indices);
        }

        let code_start = out.len();
        func_indices.insert(func.id, (func.argc, code_start));

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
        for line in &func.code {
            match line {
                Line::Comment(_) => continue,
                Line::Label(_) => continue,
                Line::Instr(instr) => out.push(instr.clone()),
                Line::Call(func_id) => {
                    let (argc, index) = func_indices.get(func_id).expect("should already be defined");
                    out.push(Instr::PushFrame(*argc, *index));
                },
                Line::Return(reference) => {
                    out.push(Instr::PopFrame(*reference));
                }
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
                        Instr::Conditional(_, _a, _b) => todo!(),
                        Instr::Push(_) => Instr::Push(Reference::Executable(*argc, *index)),
                        Instr::PushExpr(_) => Instr::PushExpr(Reference::Executable(*argc, *index)),
                        Instr::SetArg(a, _) => Instr::SetArg(*a, Reference::Executable(*argc, *index)),
                        Instr::SetGlobal(a, _) => Instr::SetGlobal(*a, Reference::Executable(*argc, *index)),
                        Instr::ExecRef(_) => Instr::ExecRef(Reference::Executable(*argc, *index)),
                        Instr::Alloc(a, _) => Instr::Alloc(*a, Reference::Executable(*argc, *index)),
                        Instr::SetPoint(a, _) => Instr::SetPoint(*a, Reference::Executable(*argc, *index)),
                        Instr::Reduce(_, a) => Instr::Reduce(Reference::Executable(*argc, *index), *a),
                        Instr::ReduceRange(_, a, b, c) => Instr::ReduceRange(Reference::Executable(*argc, *index), *a, *b, *c),
                        Instr::Into(_, a) => Instr::Into(Reference::Executable(*argc, *index), *a),
                        Instr::ForEach(_, a) => Instr::ForEach(Reference::Executable(*argc, *index), *a),
                        Instr::ForEachRange(_, a, b, c) => Instr::ForEachRange(Reference::Executable(*argc, *index), *a, *b, *c),
                        _ => todo!(),
                    })
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
                        Instr::JumpNotNone(_, a) => out.push(Instr::JumpNotNone(index, *a)),
                        _ => panic!("unreachable"),
                    }
                },
            }
        }

        for tuple in &func.tuples {
            out.push(Instr::RemoveTuple(*tuple));
        }
        for heap in &func.heap {
            out.push(Instr::Free(*heap));
        }

        if !is_global{
            out.push(Instr::PopFrame(func.ret));
        }

        return code_start;
    }


    pub fn from_instr_to_string(instrs: &Vec<Instr>, pretty: bool) -> String {
        let mut out = vec![];
        let indent_size = instrs.len().to_string().len() + 1;
        for (i, instr) in instrs.iter().enumerate() {
            if pretty {
                let line_num = format!("{}", i);
                out.append(&mut line_num.chars().collect());
                for _ in 0..indent_size-line_num.len() {
                    out.push(' ');
                }
            }
            out.append(&mut instr.to_string().chars().collect());
            out.push('\n');
        }

        out.iter().collect()
    }
}

impl ToString for Code {
    fn to_string(&self) -> String {
        let (_, _, instrs) = self.to_instrs();
        Code::from_instr_to_string(&instrs, false)
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
        ASTNodeType::Call(_) => generate_call(func, naming, node),
        ASTNodeType::CallStr(_,_) => generate_call(func, naming, node),
        ASTNodeType::Number(num) => Ok(Reference::Literal(num)),
        _ => create_generation_error!(node, "invalid value in expression")
    }
}

fn generate_eval_function(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    generate_function(true, func, naming, node)?;
    Ok(Reference::StackExpr)
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
        _ => return create_generation_error!(node, "expected function")
    }
}

fn generate_exec(is_eval: bool, func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    if matches!(node.node_type, ASTNodeType::ExpressionList(_)) {
        return create_generation_error!(node, "expected an exec");
    }
    let is_ref = get_annotation!(node.children[1], "", Annotation::GlobalId(_id))
        .or_else(|_| get_annotation!(node.children[1], "", Annotation::Id(_id))).is_ok();

    let func_tuple_ret = generate_func_tuple(is_eval, is_ref, func, naming, &node.children[0])?;
    let func_id = func_tuple_ret;
    let mut inner_func = func.funcs.get_mut(&func_id).expect("function");
    if !is_eval {
        if let Ok(id) = get_annotation!(node.children[1], "", Annotation::GlobalId(_id)) {
            let argc = *get_annotation!(node.children[1], "expected executable to take args", Annotation::Argc(_args))?;
            if inner_func.argc < argc {
                for _ in 0..argc-inner_func.argc {
                    inner_func.code.push(Line::Instr(Instr::PushExpr(Reference::None)));
                }
            }
            inner_func.code.push(Line::Instr(Instr::ExecRef(Reference::Global(*id))));
            inner_func.ret = Reference::StackExpr;
        } else if let Ok(id) = get_annotation!(node.children[1], "", Annotation::Id(_id)) {
            let argc = *get_annotation!(node.children[1], "expected executable to take args", Annotation::Argc(_args))?;
            if inner_func.argc < argc {
                for _ in 0..argc-inner_func.argc {
                    inner_func.code.push(Line::Instr(Instr::PushExpr(Reference::None)));
                }
            }
            inner_func.code.push(Line::Instr(Instr::ExecRef(Reference::Argument(*id))));
            inner_func.ret = Reference::StackExpr;
        } else if matches!(node.children[1].node_type, ASTNodeType::ExpressionList(_)){
            let reference = generate_expression_list(&mut inner_func, naming, &node.children[1])?;
            inner_func.ret = reference;
        }
        Ok(inner_func.id)
    } else {
        if let Ok(id) = get_annotation!(node.children[1], "", Annotation::GlobalId(_id)) {
            let argc = *get_annotation!(node.children[1], "expected executable to take args", Annotation::Argc(_args))?;
            func.code.append(&mut inner_func.code);
            if inner_func.argc < argc {
                for _ in 0..argc-inner_func.argc {
                    func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
            }
            func.code.push(Line::Instr(Instr::ExecRef(Reference::Global(*id))));
            func.funcs.remove(&func_id);
            Ok(func.id)
        } else if let Ok(id) = get_annotation!(node.children[1], "", Annotation::Id(_id)) {
            let argc = *get_annotation!(node.children[1], "expected executable to take args", Annotation::Argc(_args))?;
            func.code.append(&mut inner_func.code);
            if inner_func.argc < argc {
                for _ in 0..argc-inner_func.argc {
                    func.code.push(Line::Instr(Instr::Push(Reference::None)));
                }
            }
            func.code.push(Line::Instr(Instr::ExecRef(Reference::Argument(*id))));
            func.funcs.remove(&func_id);
            Ok(func.id)
        } else if matches!(node.children[1].node_type, ASTNodeType::ExpressionList(_)){
            let reference = generate_expression_list(&mut inner_func, naming, &node.children[1])?;
            func.code.push(Line::Call(func_id));
            inner_func.ret = reference;
            Ok(inner_func.id)
        } else{
            unreachable!("failed to validate exec");
        }
    }
}

fn generate_extended_exec(is_eval: bool, func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    let lhs = match &node.children[0].node_type {
        ASTNodeType::Range =>  Reference::None,
        ASTNodeType::RangeComplex => Reference::None,
        ASTNodeType::TupleConstructor => {
            let reference = generate_expression(func, naming, &node.children[0].children[0])?;
            let tuple_id = naming.new_tuple_id();
            func.code.push(Line::Instr(Instr::CreateTuple(tuple_id, reference)));
            func.tuples.push(tuple_id);
            Reference::Tuple(tuple_id)
        }
        ASTNodeType::Matrix => Reference::Matrix,
        ASTNodeType::Tuple(_) => {
            if let Reference::Tuple(id) = generate_tuple(func, naming, &node.children[0])? {
                Reference::Tuple(id)
            } else {
                return create_generation_error!(node.children[0], "expected tuple as lhs")
            }
        },
        ASTNodeType::Reference(_) => generate_reference(func, naming, &node.children[0])?,
        _ => return create_generation_error!(node.children[0], "invalid lhs for extended exec"),
    };

    if is_eval {
        let exec_id = generate_exec(false, func, naming, &node.children[1])
        .or_else(|_| {
            let mut inner_inner_func = Function::new(0,naming.new_func_id());
            let reference = generate_expression_list(&mut inner_inner_func, naming, &node.children[1])?;
            inner_inner_func.ret = reference;
            let id = inner_inner_func.id;
            func.funcs.insert(inner_inner_func.id, inner_inner_func);
            Ok(id)
        })?;
    
        if lhs != Reference::None {
            match node.node_type {
                ASTNodeType::Reduce => func.code.push(Line::ExecRef(Instr::Reduce(Reference::None, lhs), exec_id)),
                ASTNodeType::ForEach => func.code.push(Line::ExecRef(Instr::ForEach(Reference::None, lhs), exec_id)),
                ASTNodeType::Into => func.code.push(Line::ExecRef(Instr::Into(Reference::None, lhs), exec_id)),
                _ => return create_generation_error!(node, "expected extended func")
            }
        } else {
            let (start, end, step) = match &node.children[0].node_type {
                ASTNodeType::Range => {
                    let end = generate_expression(func, naming, &node.children[0].children[1])?;
                    let start = generate_expression(func, naming, &node.children[0].children[0])?;
                    (start, end, Reference::Literal(1.0))
                },
                ASTNodeType::RangeComplex => {
                    let step = generate_expression(func, naming, &node.children[0].children[2])?;
                    let end = generate_expression(func, naming, &node.children[0].children[1])?;
                    let start = generate_expression(func, naming, &node.children[0].children[0])?;
                    (start, end, step)
                },
                _ => return create_generation_error!(node.children[0], "invalid lhs for extended exec"),
            };
    
            match node.node_type {
                ASTNodeType::Reduce => func.code.push(Line::ExecRef(Instr::ReduceRange(Reference::None, start, end, step), exec_id)),
                ASTNodeType::ForEach => func.code.push(Line::ExecRef(Instr::ForEachRange(Reference::None, start, end, step), exec_id)),
                _ => return create_generation_error!(node, "expected extended func")
            }
        }
        Ok(func.id)
    } else {
        let mut inner_func = Function::new(0, naming.new_func_id());

        let exec_id = generate_exec(false, &mut inner_func, naming, &node.children[1])
        .or_else(|_| {
            let mut inner_inner_func = Function::new(0,naming.new_func_id());
            let reference = generate_expression_list(&mut inner_inner_func, naming, &node.children[1])?;
            inner_inner_func.ret = reference;
            let id = inner_inner_func.id;
            inner_func.funcs.insert(inner_inner_func.id, inner_inner_func);
            Ok(id)
        })?;
    
        if lhs != Reference::None {
            match node.node_type {
                ASTNodeType::Reduce => inner_func.code.push(Line::ExecRef(Instr::Reduce(Reference::None, lhs), exec_id)),
                ASTNodeType::ForEach => inner_func.code.push(Line::ExecRef(Instr::ForEach(Reference::None, lhs), exec_id)),
                ASTNodeType::Into => inner_func.code.push(Line::ExecRef(Instr::Into(Reference::None, lhs), exec_id)),
                _ => return create_generation_error!(node, "expected extended func")
            }
        } else {
            let (start, end, step) = match &node.children[0].node_type {
                ASTNodeType::Range => {
                    let start = generate_expression(func, naming, &node.children[0].children[0])?;
                    let end = generate_expression(func, naming, &node.children[0].children[1])?;
                    (start, end, Reference::Literal(1.0))
                },
                ASTNodeType::RangeComplex => {
                    let start = generate_expression(func, naming, &node.children[0].children[0])?;
                    let end = generate_expression(func, naming, &node.children[0].children[1])?;
                    let step = generate_expression(func, naming, &node.children[0].children[2])?;
                    (start, end, step)
                },
                _ => return create_generation_error!(node.children[0], "invalid lhs for extended exec"),
            };

            let step_id = naming.new_heap_id();
            func.code.push(Line::Instr(Instr::Alloc(step_id, step)));
            func.heap.push(step_id);
            let end_id = naming.new_heap_id();
            func.code.push(Line::Instr(Instr::Alloc(end_id, end)));
            func.heap.push(end_id);
            let start_id = naming.new_heap_id();
            func.code.push(Line::Instr(Instr::Alloc(start_id, start)));
            func.heap.push(start_id);
    
            match node.node_type {
                ASTNodeType::Reduce => inner_func.code.push(Line::ExecRef(
                    Instr::ReduceRange(Reference::None, Reference::Heap(start_id), Reference::Heap(end_id), Reference::Heap(step_id)),
                exec_id)),
                ASTNodeType::ForEach => inner_func.code.push(Line::ExecRef(
                    Instr::ForEachRange(Reference::None, Reference::Heap(start_id), Reference::Heap(end_id), Reference::Heap(step_id)),
                exec_id)),
                _ => return create_generation_error!(node, "expected extended func")
            }
        }
        let id = inner_func.id;
        inner_func.ret = Reference::StackExpr;
        func.funcs.insert(inner_func.id, inner_func);
        Ok(id)
    }
}

fn generate_func_tuple(is_eval: bool, is_ref: bool, func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<usize, GenerationError> {
    let mut arg_index = 0;
    if is_eval {
        let mut inner_func = Function::new(0, naming.new_func_id());
        for i in 0..node.children.len() {
            if matches!(node.children[i].node_type, ASTNodeType::Throw) {
                continue;
            }
            if is_ref {
                let reference = generate_expression(&mut inner_func, naming, &node.children[i])?;
                    
                if !matches!(reference, Reference::StackPeek) && !matches!(reference, Reference::Stack) {
                    inner_func.code.push(Line::Instr(Instr::Push(reference)));
                }

                arg_index += 1;
            } else {
                if generate_func_definition(is_eval, func, naming, &node.children[i]).is_ok() {
                    arg_index += 1;
                } else {
                    let reference = generate_expression(&mut inner_func, naming, &node.children[i])?;
                    
                    if !matches!(reference, Reference::StackPeek) && !matches!(reference, Reference::Stack) {
                        inner_func.code.push(Line::Instr(Instr::Push(reference)));
                    }
                } 
            }
        }
        let id = inner_func.id;
        inner_func.argc = arg_index;
        func.funcs.insert(inner_func.id, inner_func);
        Ok(id)
    } else {
        let mut inner_func = Function::new(0, naming.new_func_id());
        for i in 0..node.children.len() {
            if matches!(node.children[i].node_type, ASTNodeType::Throw) {
                continue;
            }
            if generate_func_definition(is_eval, &mut inner_func, naming, &node.children[i]).is_ok() {
                if !is_ref {
                    arg_index += 1;
                }
            } else {
                let reference = generate_expression(&mut inner_func, naming, &node.children[i])?;
    
                if !matches!(reference, Reference::StackPeek) && !matches!(reference, Reference::Stack) {
                    inner_func.code.push(Line::Instr(Instr::Push(reference)));
                }
                if is_ref {
                    arg_index += 1;
                }
            } 
        }
        
        inner_func.argc = arg_index;
    
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
        create_generation_error!(node, "expected tuple")
    }
}

fn generate_expression_list(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    if let ASTNodeType::ExpressionList(_) = node.node_type {
        for i in (0..node.children.len()).step_by(2) {
            func.code.push(Line::Instr(Instr::StartExpr)); 
            let reference = generate_definition(func, naming, &node.children[i])
            .or_else(|_| generate_expression(func, naming, &node.children[i]) )?;

            if i+1 >= node.children.len() {
                if !matches!(reference, Reference::Stack) && !matches!(reference, Reference::StackPeek) {
                    func.code.push(Line::Instr(Instr::Push(reference)));
                }
                if matches!(func.code.last().unwrap(), Line::Instr(Instr::StartExpr)) {
                    func.code.pop();
                } else {
                    func.code.push(Line::Instr(Instr::EndExpr));   
                }
                break;
            }
            
            match &node.children[i+1].node_type {
                ASTNodeType::Throw => {
                    if let Ok(count) = get_annotation!(node.children[i], "", Annotation::StackPop(_count)) {
                        func.code.push(Line::Instr(Instr::Pop(*count)));
                    }
                    if matches!(func.code.last().unwrap(), Line::Instr(Instr::StartExpr)) {
                        func.code.pop();
                    } else {
                        func.code.push(Line::Instr(Instr::EndExpr));   
                    }     
                },
                ASTNodeType::Push => {   
                    if !matches!(reference, Reference::Stack) && !matches!(reference, Reference::StackPeek) {
                        func.code.push(Line::Instr(Instr::Push(reference)));
                    }
                    if matches!(func.code.last().unwrap(), Line::Instr(Instr::StartExpr)) {
                        func.code.pop();
                    } else {
                        func.code.push(Line::Instr(Instr::EndExpr));   
                    } 
                },
                _ => return create_generation_error!(node.children[i+1], "expected terminator to expression")
            }
        }
        Ok(Reference::Stack)
    } else {
        create_generation_error!(node, "expected an expression list")
    }
}

fn generate_indexed(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    match node.children[0].node_type {
        ASTNodeType::Matrix => {
            if let ASTNodeType::Index2D = &node.children[1].node_type {
                let x = generate_expression(func, naming, &node.children[1].children[0])?;
                let y = generate_expression(func, naming, &node.children[1].children[1])?;
                func.code.push(Line::Instr(Instr::GetMatrix(x, y)));
            } else {
                return create_generation_error!(node.children[1], "Expected meta in indexed value")
            }
        },
        ASTNodeType::Reference(_) => {
            let reference = generate_reference(func, naming, &node.children[0])?;

            if let ASTNodeType::Index = &node.children[1].node_type {
                let x = generate_expression(func, naming, &node.children[1].children[0])?;
                func.code.push(Line::Instr(Instr::GetTupleReference(reference, x)));
            } else {
                return create_generation_error!(node.children[1], "Expected index in indexed value")
            }
        },
        _=> return create_generation_error!(node.children[0], "Expected index in indexed value")
    }

    Ok(Reference::StackExpr)
}

fn generate_call(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    let (string, ident) = if let ASTNodeType::Call(ident) = &node.node_type {
        (None, ident)
    } else if let  ASTNodeType::CallStr(ident, string) = &node.node_type {
        (Some(string), ident)
    } else {
        return create_generation_error!(node, "couldn't generate call")
    };

    let id = if let Some((id, _)) = naming.call_map.get(ident) {
        *id
    } else {
        return create_generation_error!(node, "call not found")
    };

    for i in 0..node.children.len() {
        let reference = generate_expression(func, naming, &node.children[i])?;
        if !matches!(reference, Reference::StackExpr) && !matches!(reference, Reference::StackPeekExpr) {
            func.code.push(Line::Instr(Instr::PushExpr(reference)));
        }
    }

    if let Some(string) = string {
        func.code.push(Line::Instr(Instr::CallStr(id, string.clone())));

    } else {
        func.code.push(Line::Instr(Instr::Call(id)));
    }

    Ok(Reference::StackExpr)
}

fn generate_reference(func: &mut Function, _naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    println!("{:?}", node.node_type);
    let reference = if let ASTNodeType::Reference(reference) = &node.node_type {
        reference
    } else {
        return create_generation_error!(node, "couldn't generate reference")
    };

    match &reference {
        Token::Peek => {
            if let Ok(index) = get_annotation!(node, "", Annotation::StackIndex(_index)) {
                Ok(Reference::StackIndex(*index))
            } else {
                func.code.push(Line::Instr(Instr::Duplicate));
                Ok(Reference::Stack)
            }
        },
        Token::Pop => {
            if let Ok(index) = get_annotation!(node, "", Annotation::StackIndex(_index)) {
                Ok(Reference::StackIndex(*index))
            } else {
                Ok(Reference::Stack)
            }
        },
        Token::Matrix => Ok(Reference::Matrix),
        Token::Identifier(_) => {
            if let Ok(pull_through_id) = get_annotation!(node, "", Annotation::PullThrough(_id)) {
                Ok(Reference::Argument(*pull_through_id))
            } else if let Ok(id) = get_annotation!(node, "", Annotation::GlobalId(id)) {
                Ok(Reference::Global(*id))
            } else {
                let id = get_annotation!(node, "definition requires id", Annotation::Id(id))?;
                Ok(Reference::Argument(*id))
            }
        },
        _ => return create_generation_error!(node, "invalid reference"),
    }
    
}

fn generate_expression(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    let res = generate_expression_helper(func, naming, node)?;
    if let Ok(count) = get_annotation!(node, "", Annotation::StackPop(_count)) {
        if *count != 0 {
            if let Reference::StackIndex(index) = res {
                if index < *count   {
                    let index = count - index - 1;
                    if index > 0 {
                        func.code.push(Line::Instr(Instr::Pop(*count)));
                    }
                }
            } else {
                func.code.push(Line::Instr(Instr::Pop(*count)));
            }
        }
    }
    Ok(res)
}

fn generate_expression_helper(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    let res = match &node.node_type {
        ASTNodeType::Operator(token) => {
            if matches!(token, Token::Not) {
                let reference = generate_expression_helper(func, naming, &node.children[0])?;
                func.code.push(Line::Instr(Instr::Not(reference)));
            } else if matches!(token, Token::If) {
                let cond = generate_expression_helper(func, naming, &node.children[0])?;
                let false_outcome = naming.new_label_id();
                let end = naming.new_label_id();

                func.code.push(Line::Jump(Instr::JumpEqual(0, cond, Reference::Literal(0.0)), JumpType::Label(false_outcome)));
                let true_reference = generate_expression_helper(func, naming, &node.children[1].children[0])?;
                if !matches!(true_reference, Reference::StackExpr) && !matches!(true_reference, Reference::None) {
                    func.code.push(Line::Instr(Instr::PushExpr(true_reference)));
                }
                func.code.push(Line::Jump(Instr::Jump(0), JumpType::Label(end)));
                func.code.push(Line::Label(false_outcome));
                let false_reference = generate_expression_helper(func, naming, &node.children[1].children[2])?;
                if !matches!(false_reference, Reference::StackExpr) && !matches!(false_reference, Reference::None) {
                    func.code.push(Line::Instr(Instr::PushExpr(false_reference)));
                }
                func.code.push(Line::Label(end));
            } else {
                let ref2 = generate_expression_helper(func, naming, &node.children[1])?;
                let ref1 = generate_expression_helper(func, naming, &node.children[0])?;
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
                    _ => return create_generation_error!(node, "could not resolve operator")
                }
            }
            Ok(Reference::StackExpr)
        },
        _ => generate_value(func, naming, node)
    }?;
    if get_annotation!(node, "", Annotation::Return).is_ok() {
        func.code.push(Line::Return(res));
    } else if get_annotation!(node, "", Annotation::Exit).is_ok() {
        func.code.push(Line::Instr(Instr::Exit));
    }
    Ok(res)
}

fn generate_definition(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<Reference, GenerationError> {
    if !matches!(node.node_type, ASTNodeType::Set(_)) {
        return create_generation_error!(node, "node is not a definition");
    }
    let is_exec = get_annotation!(node.children[0], "", Annotation::Executable).is_ok();
    let is_expression_list = get_annotation!(node.children[0], "", Annotation::ExpressionList).is_ok();
    let id = get_annotation!(node.children[0], "", Annotation::GlobalId(id))
        .or_else(|_| get_annotation!(node.children[0], "definition requires id", Annotation::Id(id)))?;

    let reference = {
        let mut rhs = if is_exec {
            let func_id = generate_function(false, func, naming, &node.children[1])?;
            let function = func.funcs.get(&func_id).expect("function");
            let func_id = function.id;
            func.code.push(Line::ExecRef(Instr::PushExpr(Reference::None), func_id));
            Reference::StackExpr
        } else if is_expression_list {
            let mut inner_func = Function::new(0, naming.new_func_id());
            let func_id = inner_func.id;
            let expression_list = generate_expression_list(&mut inner_func, naming, &node.children[1])?;
            inner_func.ret = expression_list;
            func.funcs.insert(inner_func.id, inner_func);
            func.code.push(Line::ExecRef(Instr::PushExpr(Reference::None), func_id));
            Reference::StackExpr
        } else {
            match &node.children[1].node_type {
                ASTNodeType::Tuple(_) => {
                    if let Reference::Tuple(id) = generate_tuple(func, naming, &node.children[1])? {
                        Reference::Tuple(id)
                    } else {
                        return create_generation_error!(node.children[1], "expected tuple as lhs")
                    }
                },
                _ => generate_expression(func, naming, &node.children[1])?,
            }
        };

        let lhs = if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
                Reference::Global(*id)
            } else {
                Reference::Argument(*id)
            };
        
        match node.node_type {
            ASTNodeType::Set(Token::AddAndSet) => {
                func.code.push(Line::Instr(Instr::Add(lhs, rhs)));
                rhs = Reference::StackExpr;
            },
            ASTNodeType::Set(Token::SubtractAndSet) => {
                func.code.push(Line::Instr(Instr::Subtract(lhs, rhs)));
                rhs = Reference::StackExpr;
            },
            ASTNodeType::Set(Token::MultiplyAndSet) => {
                func.code.push(Line::Instr(Instr::Multiply(lhs, rhs)));
                rhs = Reference::StackExpr;
            },
            ASTNodeType::Set(Token::DivideAndSet) => {
                func.code.push(Line::Instr(Instr::Divide(lhs, rhs)));
                rhs = Reference::StackExpr;
            },
            _ => ()
        }
        
        if get_annotation!(node.children[0], "", Annotation::GlobalId(id)).is_ok() {
            func.code.push(Line::Instr(Instr::SetGlobal(*id, rhs)));
            Reference::Global(*id)
        } else {
            func.code.push(Line::Instr(Instr::SetArg(*id, rhs)));
            Reference::Argument(*id)
        }
    };

    Ok(reference)
}

fn generate_func_definition(is_eval: bool, func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<(), GenerationError> {
    if !matches!(node.node_type, ASTNodeType::Set(_)) {
        return create_generation_error!(node, "node is not a definition");
    }
    let is_exec = get_annotation!(node.children[0], "", Annotation::Executable).is_ok();
    let is_expression_list = get_annotation!(node.children[0], "", Annotation::ExpressionList).is_ok();
    let id = get_annotation!(node.children[0], "definition requires id", Annotation::Id(id))?;

    let mut end = 0;
    if !is_eval {
        end = naming.new_label_id();
        func.code.push(Line::Jump(Instr::JumpNotNone(0, Reference::Argument(*id)), JumpType::Label(end)));
    }

    if is_exec {
        let func_id = generate_function(is_eval, func, naming, &node.children[1])?;
        if is_eval {
            func.code.push(Line::ExecRef(Instr::Push(Reference::None), func_id));
        } else {
            func.code.push(Line::ExecRef(Instr::SetArg(*id, Reference::None), func_id));
        }
    } else if is_expression_list {
        let mut inner_func = Function::new(0, naming.new_func_id());
        let expression_list = generate_expression_list(&mut inner_func, naming, node)?;
        inner_func.ret = expression_list;

        let func_id = inner_func.id;
        func.funcs.insert(inner_func.id, inner_func);

        if is_eval {
            func.code.push(Line::ExecRef(Instr::Push(Reference::None), func_id));
        } else {
            func.code.push(Line::ExecRef(Instr::SetArg(*id, Reference::None), func_id));
        }
    } else {
        let reference = match &node.children[1].node_type {
            ASTNodeType::Tuple(_) => {
                if let Reference::Tuple(id) = generate_tuple(func, naming, &node.children[1])? {
                    Reference::Tuple(id)
                } else {
                    return create_generation_error!(node.children[1], "expected tuple as lhs")
                }
            },
            _ => generate_expression(func, naming, &node.children[1])?,
        };
        
        if is_eval {
            func.code.push(Line::Instr(Instr::Push(reference)));
        } else {
            func.code.push(Line::Instr(Instr::SetArg(*id, reference)));
        }
    }

    if !is_eval {
        func.code.push(Line::Label(end));
    }

    Ok(())
}

fn generate_statement(func: &mut Function, naming: &mut Naming, node: &Box<ASTNode>) -> Result<(), GenerationError> {
    func.code.push(Line::Instr(Instr::StartExpr));
    let reference = generate_definition(func, naming, &node.children[0])
        .or_else(|_| generate_expression(func, naming, &node.children[0]))?;
    
    if let ASTNodeType::Statement(statement_type) = &node.node_type {
        match statement_type {
            StatementType::Throw => {
                if let Ok(count) = get_annotation!(node.children[0], "", Annotation::StackPop(_count)) {
                    func.code.push(Line::Instr(Instr::Pop(count.clone())));
                }   
            },
            StatementType::Push => {   
                if !matches!(reference, Reference::Stack) && !matches!(reference, Reference::StackPeek) {
                    func.code.push(Line::Instr(Instr::Push(reference)));
                }
            }
        }
    }

    if matches!(func.code.last().unwrap(), Line::Instr(Instr::StartExpr)) {
        func.code.pop();
    } else {
        func.code.push(Line::Instr(Instr::EndExpr));   
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{parser::AbstractSyntaxTree, lexer::Tokenizer, semantic::validate};

    use super::Code;
    /*
        fac:(x:0) -> [x=0 ? (1;x*(x-1) -> fac)];
            (10) -> fac,

            centre: 250/2;
        x:100;
        y:10;
        sqrt|pow|125-centre;2| + pow|125-centre;2||,
        debug|@|;
        {0;125} <* [@@, sqrt|pow|debug|@|-centre;2| + pow|debug|@|-centre;2||, debug|@|];
        centre: 250/2;
{0;125} <* [@@, sqrt|pow|debug|@|-centre;2| + pow|debug|@|-centre;2||, debug|@|];
centre: 250/2;
{0;125} <* [@@, sqrt|pow|@-centre;2| + pow|@-centre;2||, debug|@ < 100 ? (1;0)|];
    */

    #[test] 
    fn test() {
        let mut tree = &mut AbstractSyntaxTree::new(&mut Tokenizer::new(r"
        ({x}) <- [@; @];
        ")).unwrap();

        println!("{}", tree.to_pretty_string(true));

        if let Err(err) = validate(&mut tree) {
            panic!("{}", err.to_string());
        }
        println!("{}", tree.to_pretty_string(true));


        let code = Code::new(&mut tree);

        match code {
            Ok(code) => {
                let (globals, start, instrs) = code.to_instrs();
                println!("Start index: {}\nGlobals: {}\n{}", start, globals, Code::from_instr_to_string(&instrs, true));
            },
            Err(err) => panic!("{}", err.to_string())
        }
    }
}