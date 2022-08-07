use std::{collections::HashMap, fmt};

use crate::parser::{AbstractSyntaxTree, ASTNode, ASTNodeType, Annotation};
use crate::lexer::Token;
use crate::vm::{Call, create_call_map};

#[macro_use] 
mod macros {
    macro_rules! is {
        ($a:expr, $err:tt, $enum:ident::$pattern:ident($vars:ident $(,$vars2:ident)* ) ) => {
            {
                if let $enum::$pattern($vars $(,$vars2)* ) = $a {
                    Ok(($vars $(,$vars2)*))
                } else {
                    Err(SemanticError::new(stringify!($err)))
                }
            }
        };
        ($a:expr, $err:tt, $pattern:ident($vars:ident $(,$vars2:ident)* ) ) => {
            {
                if let $pattern($vars $(,$vars2)* ) = $a {
                    Ok(($vars $(,$vars2)*))
                } else {
                    Err(SemanticError::new(stringify!($err)))
                }
            }
        };
        ($a:expr, $err:tt, $enum:ident::$pattern:ident) => {
            {
                if let $enum::$pattern = $a {
                    Ok(())
                } else {
                    Err(SemanticError::new(stringify!($err)))
                }
            }
        };
    }

    
    macro_rules! create_semantic_error {
        ($node:expr, $err:tt) => {
            {
                let mut err = stringify!($err).to_string();
                for i in 0..$node.annotations.len() {
                    if let Annotation::DebugInfo(line, line_index) = $node.annotations[i] {
                        err = format!("{} at line: {} and index: {}", err, line, line_index);
                        break;
                    }
                }
                Err(SemanticError::new(&err))
            }
        };
    }

    macro_rules! create_unwrapped_semantic_error {
        ($node:expr, $err:tt) => {
            {
                let mut err = stringify!($err).to_string();
                for i in 0..$node.annotations.len() {
                    if let Annotation::DebugInfo(line, line_index) = $node.annotations[i] {
                        err = format!("{} at line: {} and index: {}", err, line, line_index);
                        break;
                    }
                }
                SemanticError::new(&err)
            }
        };
    }

    
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
                        break create_semantic_error!($node, $err)
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
                        break create_semantic_error!($node, $err)
                    }
                    i+=1;
                }
            }
        };
    }

    #[cfg(test)]
    macro_rules! assert_semantic_eq {
        ($input:tt, $output:tt) => {
            {
                let mut tree = AbstractSyntaxTree::new(&mut Tokenizer::new($input)).unwrap();
                validate(&mut tree).unwrap();
                assert_eq!(tree.to_string(true), $output);
            }
        };
    }
}

#[derive(Debug, PartialEq)]
pub struct SemanticError {
    msg: String
}


impl SemanticError {
    pub fn new(msg: &str) -> SemanticError {
        SemanticError { msg: msg.to_string() }
    }

    pub fn to_string(&self) -> String {
        self.msg.clone()
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}


#[derive(Debug, Clone, Copy)]
pub enum VariableType {
    Number,
    Executable,
    ExpressionList,
    Tuple,
    Matrix,
}


impl VariableType {
    pub fn node_to_type(data: &SemanticData, node: &Box<ASTNode>, pull_through: bool) -> Result<VariableType, SemanticError> {
        match &node.node_type {
            ASTNodeType::Operator(_) => Ok(VariableType::Number),
            ASTNodeType::Exec => Ok(VariableType::Executable),
            ASTNodeType::Reduce => Ok(VariableType::Executable),
            ASTNodeType::ForEach => Ok(VariableType::Executable),
            ASTNodeType::Into => Ok(VariableType::Executable),
            ASTNodeType::ExpressionList(_) => Ok(VariableType::ExpressionList),
            ASTNodeType::Tuple(_) => Ok(VariableType::Tuple),
            ASTNodeType::Matrix => Ok(VariableType::Matrix),
            ASTNodeType::Indexed => Ok(VariableType::Number),
            ASTNodeType::Range => Ok(VariableType::Tuple),
            ASTNodeType::RangeComplex => Ok(VariableType::Tuple),
            ASTNodeType::Call(_) => Ok(VariableType::Number),
            ASTNodeType::Reference(ref_type) => match ref_type {
                Token::Identifier(ident) => {
                    if let Some(variable) = data.globals.variables.get(ident) {
                        return Ok(variable.var_type)
                    } else if let Some(scope) = data.stack.last() {
                        if let  Some(variable) = scope.variables.get(ident) {
                            return Ok(variable.var_type)
                        } else if pull_through {
                            if let Some(scope) = data.stack.get(data.stack.len()-2){
                                if let  Some(variable) = scope.variables.get(ident) {
                                    return Ok(variable.var_type)
                                }
                            }
                        }
                    }
                    create_semantic_error!(node, "variable value type could not be determined at compilation")
                },
                Token::Pop => Ok(VariableType::Number),
                Token::Peek => Ok(VariableType::Number),
                _ => create_semantic_error!(node, "variable value type could not be determined at compilation"),
            },
            ASTNodeType::Number(_) => Ok(VariableType::Number),
            _=> create_semantic_error!(node, "variable value type could not be determined at compilation")
        }
    }
}


#[derive(Debug, Clone)]
pub struct Variable {
    pub id: usize,
    pub var_type: VariableType,
    pub argc: Option<usize>,
}

impl Variable {
    pub fn new(id: usize, var_type: VariableType, argc: Option<usize>) -> Variable {
        Variable { id, var_type, argc }
    }
}

#[derive(Debug)]

pub struct Scope {
    pub variables: HashMap<String, Variable>,
    id_front: usize,
    in_expr: bool,
    pop_count: usize,
    peeked: bool,
}


impl Scope {
    pub fn new() -> Scope {
        Scope { variables: HashMap::new(), id_front: 0, in_expr: false, pop_count: 0, peeked: false }
    }

    pub fn new_id(&mut self) -> usize {
        self.id_front += 1;
        self.id_front - 1
    }
}


pub struct SemanticData{
    stack: Vec<Scope>,
    globals: Scope,
    call_map: HashMap<String, (usize, &'static dyn Call)>,
}


impl SemanticData {
    pub fn new() -> SemanticData {
        SemanticData { stack: vec![], globals: Scope::new(), call_map: create_call_map() }
    }

    pub fn enter_expr(&mut self) {
        if let Some(scope) = self.stack.last_mut() {
            scope.peeked = false;
            scope.in_expr = true;
            scope.pop_count = 0;
        } else {
            self.globals.peeked = false;
            self.globals.in_expr = true;
            self.globals.pop_count = 0;
        }
    }

    pub fn exit_expr(&mut self) {
        if let Some(scope) = self.stack.last_mut() {
            scope.peeked = false;
            scope.in_expr = false;
            scope.pop_count = 0;
        } else {
            self.globals.peeked = false;
            self.globals.in_expr = false;
            self.globals.pop_count = 0;
        }
    }


    pub fn in_expr(&mut self) -> bool {
        if let Some(scope) = self.stack.last_mut() {
            scope.in_expr
        } else {
            self.globals.in_expr 
        }
    }

    pub fn pop(&mut self) -> usize {
        if let Some(scope) = self.stack.last_mut() {
            scope.peeked = false;
            let res = scope.pop_count;
            scope.pop_count += 1;
            res
        } else {
            self.globals.peeked = false;
            let res = self.globals.pop_count;
            self.globals.pop_count += 1;
            res
        }
    }


    pub fn peek(&mut self) -> usize {
        if let Some(scope) = self.stack.last_mut() {
            scope.peeked = true;
            scope.pop_count
        } else {
            self.globals.peeked = true;
            self.globals.pop_count
        }
    }

    pub fn get_pop_count(&mut self) -> usize {
        if let Some(scope) = self.stack.last_mut() {
            scope.pop_count
        } else {
            self.globals.pop_count
        }
    }
    
}


pub fn validate(ast: &mut AbstractSyntaxTree) -> Result<(), SemanticError> {
    let mut data = SemanticData::new();
    if let ASTNodeType::Root = ast.root.node_type {
        for i in 0..ast.root.children.len() {
            validate_statement(&mut data, &mut ast.root.children[i])?;
        }
        ast.root.annotations.push(Annotation::Scope(data.globals.variables.len()));
        Ok(())
    } else {
        let root = &mut ast.root;
        create_semantic_error!(root, "expected statements")
    }
}

fn validate_statement(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    for i in 0..node.children.len() {
        if let ASTNodeType::Set(_) = node.children[i].node_type {
            validate_definition(data, &mut node.children[i], false, true)?;
        } else {
            validate_expression(data, &mut node.children[i], false)?;
        }
    }
    Ok(())
}

fn validate_definition(data: &mut SemanticData, node: &mut Box<ASTNode>, pull_through: bool, allow_creation: bool) -> Result<(), SemanticError> {
    let set_type = is!(&node.node_type, "expected definition", ASTNodeType::Set(set_type))?;
    let var_type = VariableType::node_to_type(data, &node.children[1], pull_through)?;

    let mut init = false;
    {
        let argc =  if matches!(var_type, VariableType::Executable) || matches!(var_type, VariableType::ExpressionList) {
            if let Ok(argc) = validate_argc(false, data, &mut node.children[1], pull_through) {
                Some(argc) 
            } else {
                None
            }
        } else{
            None
        };
        let reference = is!(&node.children[0].node_type, "expected identifier in definition", ASTNodeType::Reference(reference))?;
        let ident = is!(reference, "expected identifier in definition", Token::Identifier(ident))?;
        if data.globals.variables.get(ident).is_none() && (data.stack.last().is_none() || data.stack.last().unwrap().variables.get(ident).is_none()) {
            if !allow_creation {
                return create_semantic_error!(node, "variable creation is not allowed in this scope") 
            }

            if !matches!(set_type, Token::Set) {
                return create_semantic_error!(node, "variable must be defined before it is modified") 
            }

            init = true;
            
            if let Some(scope) = data.stack.last_mut() {
                let id = scope.new_id();
                scope.variables.insert(ident.clone(), Variable::new(id, var_type, argc));
            } else {
                let id = data.globals.new_id();
                data.globals.variables.insert(ident.clone(), Variable::new(id, var_type, argc));
            }
        }
    }

    let value = match node.children[1].node_type {
        ASTNodeType::ExpressionList(_) => validate_expression_list(data, &mut node.children[1]),
        ASTNodeType::Exec => validate_exec( false, data, &mut node.children[1]),
        ASTNodeType::Reduce => validate_extended_exec(false, data, &mut node.children[1]),
        ASTNodeType::ForEach => validate_extended_exec(false, data, &mut node.children[1]),
        ASTNodeType::Into => validate_extended_exec(false, data, &mut node.children[1]),
        _ => validate_expression(data, &mut node.children[1], pull_through),
    };

    if !matches!(set_type, Token::Set) {
        if !matches!(var_type, VariableType::Number) {
            create_semantic_error!(node, "only numbers may be set with an operator")?
        }
    }

    let reference = is!(&node.children[0].node_type, "expected identifier in definition", ASTNodeType::Reference(reference))?;
    let ident = is!(reference, "expected identifier in definition", Token::Identifier(ident))?;

    if let Err(err) = value {
        if init {
            if data.globals.variables.get(ident).is_some() {
                data.globals.variables.remove(ident);
            } else if let Some(scope) = data.stack.last_mut() {
                if scope.variables.get(ident).is_some() {
                    scope.variables.remove(ident);
                }
            }
        }

        return Err(err);
    }
    
    if let Some(variable) = data.globals.variables.get(ident) {
        if !init {
            if match var_type {
                VariableType::Number => !matches!(variable.var_type, VariableType::Number),
                VariableType::Executable => !matches!(variable.var_type, VariableType::Executable),
                VariableType::ExpressionList => !matches!(variable.var_type, VariableType::ExpressionList),
                VariableType::Tuple => !matches!(variable.var_type, VariableType::Tuple),
                VariableType::Matrix => !matches!(variable.var_type, VariableType::Matrix),
            } {
                return create_semantic_error!(node, "definition cannot change type");
            }

            if data.stack.len() != 0 {
                return create_semantic_error!(node, "global variables can only be changed in the global scope");
            }
        }
        node.children[0].annotations.push(Annotation::GlobalId(variable.id));
        if matches!(variable.var_type, VariableType::Executable) {
            if let Some(argc) = variable.argc {
                node.children[0].annotations.push(Annotation::Argc(argc));
            }
            node.children[0].annotations.push(Annotation::Executable);
        } else if matches!(variable.var_type, VariableType::ExpressionList)  {
            node.children[0].annotations.push(Annotation::Argc(0));
            node.children[0].annotations.push(Annotation::ExpressionList);
        }
    } else if let Some(scope) = data.stack.last_mut() {
        if let Some(variable) = scope.variables.get(ident) {
            if !init {
                if match var_type {
                    VariableType::Number => !matches!(variable.var_type, VariableType::Number),
                    VariableType::Executable => !matches!(variable.var_type, VariableType::Executable),
                    VariableType::ExpressionList => !matches!(variable.var_type, VariableType::ExpressionList),
                    VariableType::Tuple => !matches!(variable.var_type, VariableType::Tuple),
                    VariableType::Matrix => !matches!(variable.var_type, VariableType::Matrix),
                } {
                    return create_semantic_error!(node, "definition cannot change type");
                }
            }
            node.children[0].annotations.push(Annotation::Id(variable.id));
            if matches!(variable.var_type, VariableType::Executable) {
                if let Some(argc) = variable.argc {
                    node.children[0].annotations.push(Annotation::Argc(argc));
                }
                node.children[0].annotations.push(Annotation::Executable);
            } else if matches!(variable.var_type, VariableType::ExpressionList)  {
                node.children[0].annotations.push(Annotation::Argc(0));
                node.children[0].annotations.push(Annotation::ExpressionList);
            }
        }
    }
    Ok(())
}

fn validate_expression(data: &mut SemanticData, node: &mut Box<ASTNode>, pull_through: bool) -> Result<(), SemanticError> {
    data.enter_expr();
    match validate_expression_helper(data, node, pull_through) {
        Ok(_) => {
            if data.get_pop_count() != 0 {
                node.annotations.push(Annotation::StackPop(data.get_pop_count()));
            }
            data.exit_expr();

            if data.stack.last().is_none() {
                let mut index = None;
                for i in 0..node.annotations.len() {
                    if matches!(node.annotations[i], Annotation::Return) {
                        index = Some(i);
                        break;
                    }
                }
                if let Some(index) = index {
                    node.annotations.remove(index);
                    node.annotations.push(Annotation::Exit)
                }
            }
            Ok(())
        },
        Err(err) => {
            data.exit_expr();
            Err(err)
        },
    }
}

fn validate_expression_helper(data: &mut SemanticData, node: &mut Box<ASTNode>, pull_through: bool) -> Result<(), SemanticError> {
    match node.node_type {
        ASTNodeType::Operator(Token::Not) => Ok(()),
        ASTNodeType::Operator(Token::If) => {
            validate_expression_helper(data, &mut node.children[0], pull_through)?;
            let len = is!(node.children[1].node_type, "expected a tuple", ASTNodeType::Tuple(len))?;
            if len < 2 {
                return create_semantic_error!(node, "if requires a tuple with a length of at least 2")
            }
            validate_tuple(data, &mut node.children[1])
        },
        ASTNodeType::Operator(_) => {
            validate_expression_helper(data, &mut node.children[0], pull_through)?;
            validate_expression_helper(data, &mut node.children[1], pull_through)
        },
        _ => validate_value(data, node, pull_through),
    }
}

fn validate_value(data: &mut SemanticData, node: &mut Box<ASTNode>, pull_through: bool) -> Result<(), SemanticError> {
    match node.node_type {
        ASTNodeType::TailExec =>  validate_tail_exec(data, node),
        ASTNodeType::Exec => validate_exec( true, data, node),
        ASTNodeType::Reduce => validate_extended_exec(true, data, node),
        ASTNodeType::ForEach => validate_extended_exec(true, data, node),
        ASTNodeType::Into => validate_extended_exec(true, data, node),
        ASTNodeType::ExpressionList(_) => validate_expression_list(data, node),
        ASTNodeType::Indexed => validate_indexed(data, node),
        ASTNodeType::Call(_) => validate_call(data, node),
        ASTNodeType::CallStr(_,_) => validate_call(data, node),
        ASTNodeType::Reference(_) => {
            let var_type = VariableType::node_to_type(data, node, pull_through);
            if matches!(var_type, Ok(VariableType::Matrix)) {
                    return create_semantic_error!(node, "reference cannot be to or matrix in an expression")
            }
            validate_reference(data, node, pull_through)
        },
        ASTNodeType::Number(_) => Ok(()),
        _ => create_semantic_error!(node, "invalid value")
    }
}

fn validate_tail_exec(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    if !matches!(node.node_type, ASTNodeType::TailExec) {
        return create_semantic_error!(node, "expected tail exec")
    }

    if data.stack.is_empty() {
        node.annotations.push(Annotation::Exit);
    }

    validate_exec(true, data, &mut node.children[0])
}

fn validate_exec(is_eval: bool, data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    if matches!(node.node_type, ASTNodeType::ExpressionList(_)) {
        return create_semantic_error!(node, "expected exec")
    }
    data.stack.push(Scope::new());
    let res = {
        let is_ref = is!(&node.children[1].node_type, "expected identifier", ASTNodeType::Reference(reference)).is_ok();
        validate_exec_tuple(is_eval, is_ref, data, &mut node.children[0])
        .and_then(|argc| {
            node.annotations.push(Annotation::Argc(argc));
            let ident = is!(&node.children[1].node_type, "expected identifier", ASTNodeType::Reference(reference))
            .and_then(|reference| is!(reference, "expected identifier", Token::Identifier(ident)));

            if let Ok(ident) = ident {
                if let Some(variable) = data.globals.variables.get(ident) {
                    if let Some(var_argc) = variable.argc {
                        if var_argc < argc {
                            return create_semantic_error!(node, "tried to pass to many args");
                        }
                        node.children[1].annotations.push(Annotation::Argc(var_argc));
                    }
                    if matches!(variable.var_type, VariableType::ExpressionList) {
                        node.children[1].annotations.push(Annotation::GlobalId(variable.id));
                        node.children[1].annotations.push(Annotation::Executable);
                        return Ok(())
                    } else if matches!(variable.var_type, VariableType::Executable) {
                        node.children[1].annotations.push(Annotation::GlobalId(variable.id));
                        return Ok(())
                    } else {
                        return create_semantic_error!(node, "identifier must be an expression list");
                    }
                }

                if let Some(scope) = data.stack.get(data.stack.len() - 2) {
                    if let Some(variable) = scope.variables.get(ident) {
                        if let Some(var_argc) = variable.argc {
                            if var_argc < argc {
                                return create_semantic_error!(node, "tried to pass to many args");
                            }
                            node.children[1].annotations.push(Annotation::Argc(var_argc));
                        }
                        if matches!(variable.var_type, VariableType::ExpressionList) {
                            node.children[1].annotations.push(Annotation::Id(variable.id));
                            node.children[1].annotations.push(Annotation::ExpressionList);
                            return Ok(())
                        } else if matches!(variable.var_type, VariableType::Executable) {
                            node.children[1].annotations.push(Annotation::Id(variable.id));
                            return Ok(())
                        } 
                    } else {
                        return create_semantic_error!(node, "identifier must be an expression list");
                    }
                }
                create_semantic_error!(node, "identifier must already be defined")
            } else {
                validate_expression_list(data, &mut node.children[1])
            }
        })
    };
    let scope = data.stack.pop().unwrap();
    if res.is_ok() {
        node.annotations.push(Annotation::Scope(scope.variables.len()));
    }
    res
}

fn validate_extended_exec(is_eval: bool, data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    validate_exec(is_eval, data, &mut node.children[1]).or_else(|_| {
        data.stack.push(Scope::new());
        let res = validate_expression_list(data, &mut node.children[1]);
        let scope = data.stack.pop().unwrap();
        node.children[1].annotations.push(Annotation::Scope(scope.variables.len()));
        res
    })?;
    match &node.children[0].node_type {
        ASTNodeType::Range => Ok(()),
        ASTNodeType::RangeComplex => Ok(()),
        ASTNodeType::Matrix => Ok(()),
        ASTNodeType::Reference(Token::Identifier(_)) => {
            validate_reference(data, &mut node.children[0], false)?;
            if !matches!(get_annotation!(node.children[0], "", Annotation::Matrix), Ok(_)) &&
                !matches!(get_annotation!(node.children[0], "", Annotation::Tuple), Ok(_)) {
                create_semantic_error!(node.children[0], "expected reference to be a tuple or matrix")
            } else {
                Ok(())
            }
        },
        _ => create_semantic_error!(node, "invalid lhs of into")
    }
}

fn validate_expression_list(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    for i in 0..node.children.len() {
        if matches!(node.children[i].node_type, ASTNodeType::Throw) || matches!(node.children[i].node_type, ASTNodeType::Push) {
            continue;
        } else {
            validate_definition(data, &mut node.children[i], false, false)
            .or_else(|_| validate_expression(data, &mut node.children[i], false))?
        }
    }
    Ok(())
}


fn validate_tuple(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {    
    for i in 0..node.children.len() {
        if matches!(node.children[i].node_type, ASTNodeType::Throw) || matches!(node.children[i].node_type, ASTNodeType::Push) {
            continue;
        } else {
            validate_definition(data, &mut node.children[i], false, true)
            .or_else(|_| validate_expression(data, &mut node.children[i], false))?
        }
    }
    Ok(())
}

fn validate_exec_tuple(is_eval: bool, is_ref: bool, data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<usize, SemanticError> {  
    let mut argc = 0;  
    for i in 0..node.children.len() {
        if matches!(node.children[i].node_type, ASTNodeType::Throw) || matches!(node.children[i].node_type, ASTNodeType::Push) {
            continue;
        } else {
            if is_eval {
                if is_ref {
                    validate_expression(data, &mut node.children[i], is_eval)?;

                    argc += 1;
                } else {
                    if validate_definition(data, &mut node.children[i], is_eval, true).is_ok() {
                        argc += 1;
                    } else {
                        validate_expression(data, &mut node.children[i], is_eval)?;
                    }
                }
            } else {
                if validate_definition(data, &mut node.children[i], is_eval, true).is_ok() {
                    if !is_ref {
                        argc += 1;
                    }
                } else {
                    validate_expression(data, &mut node.children[i], is_eval)?;
                    if is_ref {
                        argc += 1;
                    }
                }
            }
        }
    }
    Ok(argc)
}

fn validate_argc(is_ref: bool, data: &mut SemanticData, node: &mut Box<ASTNode>, pull_through: bool) -> Result<usize, SemanticError> { 
    match &node.node_type {
        ASTNodeType::Exec => {
            let mut argc = 0;  
            for i in 0..node.children[0].children.len() {
                if matches!(node.children[0].children[i].node_type, ASTNodeType::Throw) || matches!(node.children[0].children[i].node_type, ASTNodeType::Push) {
                    continue;
                } else {
                    if is!(&node.children[0].children[i].node_type, "", ASTNodeType::Set(set_type)).is_ok() {
                        if !is_ref {
                            argc += 1;
                        }
                    } else {
                        if is_ref {
                            argc += 1;
                        }
                    }
                }
            }
            Ok(argc)
        },
        ASTNodeType::Reduce => validate_argc(is_ref, data, &mut node.children[1], pull_through),
        ASTNodeType::ForEach => validate_argc(is_ref, data, &mut node.children[1], pull_through),
        ASTNodeType::Into => validate_argc(is_ref, data, &mut node.children[1], pull_through),
        ASTNodeType::ExpressionList(_) => Ok(0),
        ASTNodeType::Reference(Token::Identifier(ident)) => {
            if let Some(variable) = data.globals.variables.get(ident) {
                return variable.argc.ok_or(create_unwrapped_semantic_error!(node, "couldn't determine argc"))
            } else if let Some(scope) = data.stack.last() {
                if let  Some(variable) = scope.variables.get(ident) {
                    return variable.argc.ok_or(create_unwrapped_semantic_error!(node, "couldn't determine argc"))
                } else if pull_through {
                    if let Some(scope) = data.stack.get(data.stack.len()-2){
                        if let  Some(variable) = scope.variables.get(ident) {
                            return variable.argc.ok_or(create_unwrapped_semantic_error!(node, "couldn't determine argc"))
                        }
                    }
                }
            }
            create_semantic_error!(node, "couldn't determine argc")
        }
        _=> create_semantic_error!(node, "couldn't determine argc")
    }
}

fn validate_indexed(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    if matches!(&node.children[0].node_type, ASTNodeType::Matrix) {
        if matches!(node.children[1].node_type, ASTNodeType::Index2D) {
            validate_expression(data, &mut node.children[1].children[0], false)?;
            validate_expression(data, &mut node.children[1].children[1], false)?;
        } else {
            create_semantic_error!(node, "matrix must be indexed by a 2d index")?;
        }
    } else {
        create_semantic_error!(node, "expected matrix")?
    }
    
    Ok(())
}

fn validate_call(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    let (string, ident) = if let ASTNodeType::Call(ident) = &node.node_type {
        (false, ident)
    } else if let ASTNodeType::CallStr(ident, _) = &node.node_type {
        (true, ident)
    } else {
        create_semantic_error!(node, "expected call")?
    };

    let (_, call) = if let Some(call) = data.call_map.get(ident) {
        call
    } else {
        create_semantic_error!(node, "call type could not be found")?
    };

    if !call.any_scope() && data.stack.len() != 0{
        create_semantic_error!(node,  "call can only be called from global scope")?
    }

    if string && !call.takes_str() {
        create_semantic_error!(node,  "call cannot take a string")?
    }

    let argc = call.argc();

    if node.children.len() != argc {
        create_semantic_error!(node, "call was passed the wrong number of arguments")?
    }

    for i in 0..node.children.len() {
        validate_expression_helper(data, &mut node.children[i], false)?;
    }

    Ok(())
}

fn validate_reference(data: &mut SemanticData, node: &mut Box<ASTNode>, pull_through: bool) -> Result<(), SemanticError> {
    let reference = is!(&node.node_type, "expected reference", ASTNodeType::Reference(reference))?;
    let ident = is!(reference, "expected identifier", Token::Identifier(ident));
    if ident.is_err() {
        if data.in_expr() {
            match node.node_type {
                ASTNodeType::Reference(Token::Pop) => {
                    node.annotations.push(Annotation::StackIndex(data.pop()));
                }
                ASTNodeType::Reference(Token::Peek) => {
                    node.annotations.push(Annotation::StackIndex(data.peek()));
                }
                _ => ()
            }
        }
        return Ok(())
    }
    let ident = ident.unwrap();

    let var_type = if let Some(variable) = data.globals.variables.get(ident) {
        match variable.var_type {
            VariableType::Number => (),
            VariableType::Executable => node.annotations.push(Annotation::Executable),
            VariableType::ExpressionList => node.annotations.push(Annotation::ExpressionList),
            VariableType::Tuple => node.annotations.push(Annotation::Tuple),
            VariableType::Matrix => node.annotations.push(Annotation::Matrix),
        }
        node.annotations.push(Annotation::GlobalId(variable.id));
        if let Some(argc) = variable.argc {
            node.annotations.push(Annotation::Argc(argc));
        }
        variable.var_type.clone()
    } else if let Some(scope) = data.stack.last() {
        if let Some(variable) = scope.variables.get(ident) {
            match variable.var_type {
                VariableType::Number => (),
                VariableType::Executable => node.annotations.push(Annotation::Executable),
                VariableType::ExpressionList => node.annotations.push(Annotation::ExpressionList),
                VariableType::Tuple => node.annotations.push(Annotation::Tuple),
                VariableType::Matrix => node.annotations.push(Annotation::Matrix),
            }
            node.annotations.push(Annotation::Id(variable.id));
            if let Some(argc) = variable.argc {
                node.annotations.push(Annotation::Argc(argc));
            }
            variable.var_type.clone()
        } else {
            if pull_through {
                let len = data.stack.len();
                if let Some(prev_scope) = data.stack.get(len - 2) {
                    if let Some(variable) = prev_scope.variables.get(ident) {
                        node.annotations.push(Annotation::PullThrough(variable.id));
                        match variable.var_type {
                            VariableType::Number => (),
                            VariableType::Executable => node.annotations.push(Annotation::Executable),
                            VariableType::ExpressionList => node.annotations.push(Annotation::ExpressionList),
                            VariableType::Tuple => node.annotations.push(Annotation::Tuple),
                            VariableType::Matrix => node.annotations.push(Annotation::Matrix),
                        }
                        if let Some(argc) = variable.argc {
                            node.annotations.push(Annotation::Argc(argc));
                        }
                        variable.var_type.clone()
                    } else {
                        create_semantic_error!(node, "identifier must be defined before it is used")?
                    }
                } else {
                    create_semantic_error!(node, "identifier must be defined before it is used")?
                }
            } else {
                create_semantic_error!(node, "identifier must be defined before it is used")?
            }
        }
    } else {
        create_semantic_error!(node, "identifier must be defined before it is used")?
    };

    is!(var_type, "", VariableType::Number)
    .or_else(|_| is!(var_type, "", VariableType::ExpressionList))
    .or_else(|_| is!(var_type, "", VariableType::Executable))
    .or_else(|_| is!(var_type, "value of a variable in a expression must be a number, tuple, expression list or executable", VariableType::Tuple).map(|_|()))

}


#[cfg(test)]
mod tests {
    use crate::{parser::AbstractSyntaxTree, lexer::Tokenizer};
    use super::validate;

    #[test]
    fn test_semantic() {
        let mut tree = AbstractSyntaxTree::new(&mut Tokenizer::new(r"
            var1: 10;
            var1;
            var2: 2*2;
            var2: 4;
            var3: (1; x:2; y:1) -> [(y) -> [1]];
            var3;
            2* 2 ? (2; 2);

        ")).unwrap();

        assert_eq!(validate(&mut tree), Ok(()));
        assert_eq!(tree.to_string(true), "((((Reference(Identifier(\"var1\")){DebugInfo(1, 18), GlobalId(0)})(Number(10.0){DebugInfo(1, 21)})Set(Set){DebugInfo(1, 18)})Statement(Throw){DebugInfo(1, 22)})((Reference(Identifier(\"var1\")){DebugInfo(2, 17), GlobalId(0)})Statement(Throw){DebugInfo(2, 18)})(((Reference(Identifier(\"var2\")){DebugInfo(3, 18), GlobalId(1)})((Number(2.0){DebugInfo(3, 20)})(Number(2.0){DebugInfo(3, 22)})Operator(Multiply){DebugInfo(3, 21)})Set(Set){DebugInfo(3, 18)})Statement(Throw){DebugInfo(3, 23)})(((Reference(Identifier(\"var2\")){DebugInfo(4, 18), GlobalId(1)})(Number(4.0){DebugInfo(4, 20)})Set(Set){DebugInfo(4, 18)})Statement(Throw){DebugInfo(4, 21)})(((Reference(Identifier(\"var3\")){DebugInfo(5, 18), GlobalId(2), Argc(2), Executable})(((Number(1.0){DebugInfo(5, 21)})(Throw{DebugInfo(5, 22)})((Reference(Identifier(\"x\")){DebugInfo(5, 24), Id(0)})(Number(2.0){DebugInfo(5, 26)})Set(Set){DebugInfo(5, 22)})(Throw{DebugInfo(5, 27)})((Reference(Identifier(\"y\")){DebugInfo(5, 29), Id(1)})(Number(1.0){DebugInfo(5, 31)})Set(Set){DebugInfo(5, 27)})Tuple(3){DebugInfo(5, 32)})((((Reference(Identifier(\"y\")){DebugInfo(5, 39), PullThrough(1)})Tuple(1){DebugInfo(5, 40)})((Number(1.0){DebugInfo(5, 46)})ExpressionList(1){DebugInfo(5, 47)})Exec{DebugInfo(5, 47), Argc(0), Scope(0)})ExpressionList(1){DebugInfo(5, 48)})Exec{DebugInfo(5, 48), Argc(2), Scope(2)})Set(Set){DebugInfo(5, 18)})Statement(Throw){DebugInfo(5, 49)})((Reference(Identifier(\"var3\")){DebugInfo(6, 17), Executable, GlobalId(2), Argc(2)})Statement(Throw){DebugInfo(6, 18)})((((Number(2.0){DebugInfo(7, 14)})(Number(2.0){DebugInfo(7, 17)})Operator(Multiply){DebugInfo(7, 15)})((Number(2.0){DebugInfo(7, 22)})(Throw{DebugInfo(7, 23)})(Number(2.0){DebugInfo(7, 25)})Tuple(2){DebugInfo(7, 26)})Operator(If){DebugInfo(7, 19)})Statement(Throw){DebugInfo(7, 27)})Root{DebugInfo(0, 0), Scope(3)})");
    }

    #[test]
    fn test_semantic_definition_reference() {
        assert_semantic_eq!("var1:10;var1*2;", "((((Reference(Identifier(\"var1\")){DebugInfo(0, 5), GlobalId(0)})(Number(10.0){DebugInfo(0, 7)})Set(Set){DebugInfo(0, 5)})Statement(Throw){DebugInfo(0, 8)})(((Reference(Identifier(\"var1\")){DebugInfo(0, 12), GlobalId(0)})(Number(2.0){DebugInfo(0, 14)})Operator(Multiply){DebugInfo(0, 13)})Statement(Throw){DebugInfo(0, 15)})Root{DebugInfo(0, 0), Scope(1)})");
        assert_semantic_eq!("var1:(x:2)->[x+10];var1*2;", "((((Reference(Identifier(\"var1\")){DebugInfo(0, 5), GlobalId(0), Argc(1), Executable})((((Reference(Identifier(\"x\")){DebugInfo(0, 7), Id(0)})(Number(2.0){DebugInfo(0, 9)})Set(Set){DebugInfo(0, 6)})Tuple(1){DebugInfo(0, 10)})(((Reference(Identifier(\"x\")){DebugInfo(0, 14), Id(0)})(Number(10.0){DebugInfo(0, 17)})Operator(Add){DebugInfo(0, 15)})ExpressionList(1){DebugInfo(0, 18)})Exec{DebugInfo(0, 18), Argc(1), Scope(1)})Set(Set){DebugInfo(0, 5)})Statement(Throw){DebugInfo(0, 19)})(((Reference(Identifier(\"var1\")){DebugInfo(0, 23), Executable, GlobalId(0), Argc(1)})(Number(2.0){DebugInfo(0, 25)})Operator(Multiply){DebugInfo(0, 24)})Statement(Throw){DebugInfo(0, 26)})Root{DebugInfo(0, 0), Scope(1)})");
        assert_semantic_eq!("var1: [10,10,@+@];", "((((Reference(Identifier(\"var1\")){DebugInfo(0, 5), GlobalId(0), Argc(0), ExpressionList})((Number(10.0){DebugInfo(0, 9)})(Push{DebugInfo(0, 10)})(Number(10.0){DebugInfo(0, 12)})(Push{DebugInfo(0, 13)})((Reference(Pop){DebugInfo(0, 14), StackIndex(0)})(Reference(Pop){DebugInfo(0, 16), StackIndex(1)})Operator(Add){DebugInfo(0, 15), StackPop(2)})ExpressionList(3){DebugInfo(0, 17)})Set(Set){DebugInfo(0, 5)})Statement(Throw){DebugInfo(0, 18)})Root{DebugInfo(0, 0), Scope(1)})");
        assert_semantic_eq!("var1:#<-[10]; var1*2;","((((Reference(Identifier(\"var1\")){DebugInfo(0, 5), GlobalId(0), Argc(0), Executable})((Matrix{DebugInfo(0, 6)})((Number(10.0){DebugInfo(0, 11)})ExpressionList(1){DebugInfo(0, 12), Scope(0)})Into{DebugInfo(0, 12)})Set(Set){DebugInfo(0, 5)})Statement(Throw){DebugInfo(0, 13)})(((Reference(Identifier(\"var1\")){DebugInfo(0, 18), Executable, GlobalId(0), Argc(0)})(Number(2.0){DebugInfo(0, 20)})Operator(Multiply){DebugInfo(0, 19)})Statement(Throw){DebugInfo(0, 21)})Root{DebugInfo(0, 0), Scope(1)})");
        assert_semantic_eq!("var1:#<*[10]; var1*2;","((((Reference(Identifier(\"var1\")){DebugInfo(0, 5), GlobalId(0), Argc(0), Executable})((Matrix{DebugInfo(0, 6)})((Number(10.0){DebugInfo(0, 11)})ExpressionList(1){DebugInfo(0, 12), Scope(0)})ForEach{DebugInfo(0, 12)})Set(Set){DebugInfo(0, 5)})Statement(Throw){DebugInfo(0, 13)})(((Reference(Identifier(\"var1\")){DebugInfo(0, 18), Executable, GlobalId(0), Argc(0)})(Number(2.0){DebugInfo(0, 20)})Operator(Multiply){DebugInfo(0, 19)})Statement(Throw){DebugInfo(0, 21)})Root{DebugInfo(0, 0), Scope(1)})");
        assert_semantic_eq!("var1:#*>[10]; var1*2;","((((Reference(Identifier(\"var1\")){DebugInfo(0, 5), GlobalId(0), Argc(0), Executable})((Matrix{DebugInfo(0, 6)})((Number(10.0){DebugInfo(0, 11)})ExpressionList(1){DebugInfo(0, 12), Scope(0)})Reduce{DebugInfo(0, 12)})Set(Set){DebugInfo(0, 5)})Statement(Throw){DebugInfo(0, 13)})(((Reference(Identifier(\"var1\")){DebugInfo(0, 18), Executable, GlobalId(0), Argc(0)})(Number(2.0){DebugInfo(0, 20)})Operator(Multiply){DebugInfo(0, 19)})Statement(Throw){DebugInfo(0, 21)})Root{DebugInfo(0, 0), Scope(1)})");
        assert_semantic_eq!("var1:(x:10)->[var1];", "((((Reference(Identifier(\"var1\")){DebugInfo(0, 5), GlobalId(0), Argc(1), Executable})((((Reference(Identifier(\"x\")){DebugInfo(0, 7), Id(0)})(Number(10.0){DebugInfo(0, 10)})Set(Set){DebugInfo(0, 6)})Tuple(1){DebugInfo(0, 11)})((Reference(Identifier(\"var1\")){DebugInfo(0, 18), Executable, GlobalId(0), Argc(1)})ExpressionList(1){DebugInfo(0, 19)})Exec{DebugInfo(0, 19), Argc(1), Scope(1)})Set(Set){DebugInfo(0, 5)})Statement(Throw){DebugInfo(0, 20)})Root{DebugInfo(0, 0), Scope(1)})");
        assert_semantic_eq!("var1:10*2;", "((((Reference(Identifier(\"var1\")){DebugInfo(0, 5), GlobalId(0)})((Number(10.0){DebugInfo(0, 7)})(Number(2.0){DebugInfo(0, 9)})Operator(Multiply){DebugInfo(0, 8)})Set(Set){DebugInfo(0, 5)})Statement(Throw){DebugInfo(0, 10)})Root{DebugInfo(0, 0), Scope(1)})");
    }


    #[test]
    fn test_semantic_expression() {
        assert_semantic_eq!("1+@*3;10+2/@;20;", "((((Number(1.0){DebugInfo(0, 1)})((Reference(Pop){DebugInfo(0, 3), StackIndex(0)})(Number(3.0){DebugInfo(0, 5)})Operator(Multiply){DebugInfo(0, 4)})Operator(Add){DebugInfo(0, 2), StackPop(1)})Statement(Throw){DebugInfo(0, 6)})(((Number(10.0){DebugInfo(0, 8)})((Number(2.0){DebugInfo(0, 10)})(Reference(Pop){DebugInfo(0, 12), StackIndex(0)})Operator(Divide){DebugInfo(0, 11)})Operator(Add){DebugInfo(0, 9), StackPop(1)})Statement(Throw){DebugInfo(0, 13)})((Number(20.0){DebugInfo(0, 15)})Statement(Throw){DebugInfo(0, 16)})Root{DebugInfo(0, 0), Scope(0)})");
    }

    #[test]
    fn test_semantic_tail_recursion() {
        assert_semantic_eq!("10*10!;", "((((Number(10.0){DebugInfo(0, 2)})(Number(10.0){DebugInfo(0, 5)})Operator(Multiply){DebugInfo(0, 3), Exit})Statement(Throw){DebugInfo(0, 7)})Root{DebugInfo(0, 0), Scope(0)})");
        assert_semantic_eq!("()->[10*10!];", "((((Tuple(0){DebugInfo(0, 2)})(((Number(10.0){DebugInfo(0, 7)})(Number(10.0){DebugInfo(0, 10)})Operator(Multiply){DebugInfo(0, 8), Return})ExpressionList(1){DebugInfo(0, 12)})Exec{DebugInfo(0, 12), Argc(0), Scope(0)})Statement(Throw){DebugInfo(0, 13)})Root{DebugInfo(0, 0), Scope(0)})");
        assert_semantic_eq!("func:()->[10]; ()->[func!; ()->func!];", "((((Reference(Identifier(\"func\")){DebugInfo(0, 5), GlobalId(0), Argc(0), Executable})((Tuple(0){DebugInfo(0, 7)})((Number(10.0){DebugInfo(0, 12)})ExpressionList(1){DebugInfo(0, 13)})Exec{DebugInfo(0, 13), Argc(0), Scope(0)})Set(Set){DebugInfo(0, 5)})Statement(Throw){DebugInfo(0, 14)})(((Tuple(0){DebugInfo(0, 17)})((Reference(Identifier(\"func\")){DebugInfo(0, 24), Return, Executable, GlobalId(0), Argc(0)})(Throw{DebugInfo(0, 26)})(((Tuple(0){DebugInfo(0, 29)})(Reference(Identifier(\"func\")){DebugInfo(0, 35), Argc(0), GlobalId(0)})Exec{DebugInfo(0, 35), Argc(0), Scope(0)})TailExec{DebugInfo(0, 36)})ExpressionList(2){DebugInfo(0, 37)})Exec{DebugInfo(0, 37), Argc(0), Scope(0)})Statement(Throw){DebugInfo(0, 38)})Root{DebugInfo(0, 0), Scope(1)})");
    }

    #[test]
    fn test_semantic_exec() {
        assert_semantic_eq!("(x:10)->[x];", "((((((Reference(Identifier(\"x\")){DebugInfo(0, 2), Id(0)})(Number(10.0){DebugInfo(0, 5)})Set(Set){DebugInfo(0, 1)})Tuple(1){DebugInfo(0, 6)})((Reference(Identifier(\"x\")){DebugInfo(0, 10), Id(0)})ExpressionList(1){DebugInfo(0, 11)})Exec{DebugInfo(0, 11), Argc(1), Scope(1)})Statement(Throw){DebugInfo(0, 12)})Root{DebugInfo(0, 0), Scope(0)})");
    }

    #[test]
    fn test_semantic_exec_extended() {
        assert_semantic_eq!("#<-[10];", "((((Matrix{DebugInfo(0, 1)})((Number(10.0){DebugInfo(0, 6)})ExpressionList(1){DebugInfo(0, 7), Scope(0)})Into{DebugInfo(0, 7)})Statement(Throw){DebugInfo(0, 8)})Root{DebugInfo(0, 0), Scope(0)})");
        assert_semantic_eq!("{0;10;2}<*[@];", "(((((Number(0.0){DebugInfo(0, 2)})(Number(10.0){DebugInfo(0, 5)})(Number(2.0){DebugInfo(0, 7)})RangeComplex{DebugInfo(0, 8)})((Reference(Pop){DebugInfo(0, 12), StackIndex(0), StackPop(1)})ExpressionList(1){DebugInfo(0, 13), Scope(0)})ForEach{DebugInfo(0, 13)})Statement(Throw){DebugInfo(0, 14)})Root{DebugInfo(0, 0), Scope(0)})");
        assert_semantic_eq!("{0;10;2}*>[@];", "(((((Number(0.0){DebugInfo(0, 2)})(Number(10.0){DebugInfo(0, 5)})(Number(2.0){DebugInfo(0, 7)})RangeComplex{DebugInfo(0, 8)})((Reference(Pop){DebugInfo(0, 12), StackIndex(0), StackPop(1)})ExpressionList(1){DebugInfo(0, 13), Scope(0)})Reduce{DebugInfo(0, 13)})Statement(Throw){DebugInfo(0, 14)})Root{DebugInfo(0, 0), Scope(0)})");
    }

}