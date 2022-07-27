use std::{collections::HashMap, fmt};

use crate::parser::{AbstractSyntaxTree, ASTNode, ASTNodeType, Annotation};
use crate::lexer::Token;

#[macro_use] 
mod macros {

    #[macro_export]
    macro_rules! expr {
        ($e:expr) => {
            $e
        }
    }

    #[macro_export]
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

    #[macro_export]
    macro_rules! is_true {
        ($a:expr, $err:tt, $b:expr, $op:tt) => {
            {
                if expr!($a $op $b) {
                    Ok(())
                } else {
                    Err(SemanticError::new(stringify!($err)))
                }
            }
        };
    }
    
    #[macro_export]
    macro_rules! create_semantic_error {
        ($node:ident, $err:tt) => {
            {
                for i in 0..$node.annotations.len() {
                    if let Annotation::DebugInfo(line, index) = $node.annotations[i] {
                        return Err(SemanticError::new(&format!(stringify!($err at line: {} and index: {}), line, index)))
                    }
                }
                Err(SemanticError::new(stringify!($err)))
            }
        };
    }

    #[macro_export]
    macro_rules! create_unwrapped_semantic_error {
        ($node:ident, $err:tt) => {
            {
                for i in 0..$node.annotations.len() {
                    if let Annotation::DebugInfo(pos, line, line_index) = $node.annotations[i] {
                        return SemanticError::new(&format!(stringify!($err at line: {} and index: {}), line, pos - line_index))
                    }
                }
                SemanticError::new(stringify!($err))
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
    pub fn node_to_type(data: &SemanticData, node: &Box<ASTNode>) -> Result<VariableType, SemanticError> {
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
            ASTNodeType::Reference(ref_type) => match ref_type {
                Token::Identifier(ident) => {
                    if let Some(variable) = data.globals.variables.get(ident) {
                        return Ok(variable.var_type)
                    } else if let Some(scope) = data.stack.last() {
                        if let  Some(variable) = scope.variables.get(ident) {
                            return Ok(variable.var_type)
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
}


impl Scope {
    pub fn new() -> Scope {
        Scope { variables: HashMap::new(), id_front: 0 }
    }

    pub fn new_id(&mut self) -> usize {
        self.id_front += 1;
        self.id_front - 1
    }
}


pub struct SemanticData{
    stack: Vec<Scope>,
    globals: Scope,
}


impl SemanticData {
    pub fn new() -> SemanticData {
        SemanticData { stack: vec![], globals: Scope::new() }
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
// name: 1000 + 200;
fn validate_definition(data: &mut SemanticData, node: &mut Box<ASTNode>, pull_through: bool, allow_creation: bool) -> Result<(), SemanticError> {
    let set_type = is!(&node.node_type, "expected definition", ASTNodeType::Set(set_type))?;
    let var_type = VariableType::node_to_type(data, &node.children[1])?;

    let mut init = false;
    {
        let argc =  if matches!(var_type, VariableType::Executable) || matches!(var_type, VariableType::ExpressionList) {
            Some(validate_argc(false, data, &mut node.children[1])?)
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
        ASTNodeType::ExpressionList(_) => validate_exec( false, data, &mut node.children[1]),
        ASTNodeType::Exec => validate_exec( false, data, &mut node.children[1]),
        ASTNodeType::Reduce => validate_extended_exec(false, data, &mut node.children[1]),
        ASTNodeType::ForEach => validate_extended_exec(false, data, &mut node.children[1]),
        ASTNodeType::Into => validate_extended_exec(false, data, &mut node.children[1]),
        ASTNodeType::Tuple(_) => validate_tuple(data, &mut node.children[1]),
        _ => validate_expression(data, &mut node.children[1], pull_through),
    }.or_else(|_| {
        if !matches!(set_type, Token::Set) {
            if !matches!(var_type, VariableType::Number) {
                create_semantic_error!(node, "only numbers may be set with an operator")
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    });


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
    match node.node_type {
        ASTNodeType::Operator(Token::If) => {
            validate_expression(data, &mut node.children[0], pull_through)?;
            let len = is!(node.children[1].node_type, "expected a tuple", ASTNodeType::Tuple(len))?;
            if len < 2 {
                return create_semantic_error!(node, "if requires a tuple with a length of at least 2")
            }
            validate_tuple(data, &mut node.children[1])
        },
        ASTNodeType::Operator(_) => {
            validate_expression(data, &mut node.children[0], pull_through)?;
            validate_expression(data, &mut node.children[1], pull_through)
        },
        _ => validate_value(data, node, pull_through),
    }
}

fn validate_value(data: &mut SemanticData, node: &mut Box<ASTNode>, pull_through: bool) -> Result<(), SemanticError> {
    match node.node_type {
        ASTNodeType::Exec => validate_exec( true, data, node),
        ASTNodeType::Reduce => validate_extended_exec(true, data, node),
        ASTNodeType::ForEach => validate_extended_exec(true, data, node),
        ASTNodeType::Into => validate_extended_exec(true, data, node),
        ASTNodeType::ExpressionList(_) => validate_expression_list(data, node),
        ASTNodeType::Indexed => validate_indexed(data, node),
        ASTNodeType::Reference(_) => {
            let var_type = VariableType::node_to_type(data, node);
            if matches!(var_type, Ok(VariableType::Tuple)) ||
                matches!(var_type, Ok(VariableType::Matrix)) {
                    return create_semantic_error!(node, "reference cannot be to a tuple or matrix in an expression")
            }
            validate_reference(data, node, pull_through)
        },
        ASTNodeType::Number(_) => Ok(()),
        _ => create_semantic_error!(node, "invalid value")
    }
}

fn validate_exec(is_eval: bool, data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    if node.children.len() < 2 {
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

                if let Some(scope) = data.stack.last() {
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
        ASTNodeType::TupleConstructor => Ok(()),
        ASTNodeType::Range => Ok(()),
        ASTNodeType::RangeComplex => Ok(()),
        ASTNodeType::Matrix => Ok(()),
        ASTNodeType::Tuple(_) => validate_tuple(data, node),
        ASTNodeType::Reference(Token::Identifier(ident)) => {
            if let Some(variable) = data.globals.variables.get(ident) {
                if !matches!(variable.var_type, VariableType::Matrix) && 
                    !matches!(variable.var_type, VariableType::Tuple)  {
                        return create_semantic_error!(node, "variable must be a matrix or tuple type");
                } 
                node.children[0].annotations.push(Annotation::GlobalId(variable.id));
                if matches!(variable.var_type, VariableType::Executable) {
                    node.children[0].annotations.push(Annotation::Executable);
                } else if matches!(variable.var_type, VariableType::ExpressionList)  {
                    node.children[0].annotations.push(Annotation::ExpressionList);
                }
                is!(variable.var_type, "variable must be defined before it is used", VariableType::Matrix).map(|_|())
                .or_else(|_| is!(variable.var_type, "variable must be defined before it is used", VariableType::Tuple).map(|_|()))
            } else {
                let scope = is!(data.stack.last(), "variable must be defined before it is used", Some(scope))?;
                let variable = is!(scope.variables.get(ident), "variable must be defined before it is used", Some(variable))?;
                if !matches!(variable.var_type, VariableType::Matrix) && 
                    !matches!(variable.var_type, VariableType::Tuple)  {
                        return create_semantic_error!(node, "variable must be a matrix or tuple type");
                } 
                node.children[0].annotations.push(Annotation::Id(variable.id));
                if matches!(variable.var_type, VariableType::Executable) {
                    node.children[0].annotations.push(Annotation::Executable);
                } else if matches!(variable.var_type, VariableType::ExpressionList)  {
                    node.children[0].annotations.push(Annotation::ExpressionList);
                }
                is!(variable.var_type, "variable must be defined before it is used", VariableType::Matrix).map(|_|())
                .or_else(|_| is!(variable.var_type, "variable must be defined before it is used", VariableType::Tuple).map(|_|()) )           }
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
    Ok(argc)
}

fn validate_argc(is_ref: bool, data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<usize, SemanticError> { 
    match node.node_type {
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
        ASTNodeType::Reduce => validate_argc(is_ref, data, &mut node.children[1]),
        ASTNodeType::ForEach => validate_argc(is_ref, data, &mut node.children[1]),
        ASTNodeType::Into => validate_argc(is_ref, data, &mut node.children[1]),
        ASTNodeType::ExpressionList(_) => Ok(0),
        _=> create_semantic_error!(node, "")
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
        let reference = is!(&node.children[0].node_type, "expected identifier", ASTNodeType::Reference(reference))?;
        let ident = is!(reference, "expected identifier", Token::Identifier(ident))?;
        let var_type = if let Some(variable) = data.globals.variables.get(ident) {
            node.children[0].annotations.push(Annotation::GlobalId(variable.id));
            variable.var_type
        } else if let Some(scope) = data.stack.last() {
            if let Some(variable) = scope.variables.get(ident) {
                node.children[0].annotations.push(Annotation::Id(variable.id));
                variable.var_type
            } else {
                create_semantic_error!(node, "identifier must be defined before it is used")?
            }
        } else {
            create_semantic_error!(node, "identifier must be defined before it is used")?
        }.clone();
        if matches!(node.children[1].node_type, ASTNodeType::Index) {
            validate_expression(data, &mut node.children[1].children[0], false)?;
            is!(var_type, "variable must be a tuple", VariableType::Tuple)?
        } else {
            create_semantic_error!(node, "matrix must be indexed by a 1d index")?;
        }
    }
    
    Ok(())
}

fn validate_reference(data: &mut SemanticData, node: &mut Box<ASTNode>, pull_through: bool) -> Result<(), SemanticError> {
    let reference = is!(&node.node_type, "expected reference", ASTNodeType::Reference(reference))?;
    let ident = is!(reference, "expected identifier", Token::Identifier(ident));
    if ident.is_err() {
        return Ok(())
    }
    let ident = ident.unwrap();

    let var_type = if let Some(variable) = data.globals.variables.get(ident) {
        node.annotations.push(Annotation::GlobalId(variable.id));
        if let Some(argc) = variable.argc {
            node.annotations.push(Annotation::Argc(argc));
        }
        if matches!(variable.var_type, VariableType::Executable) {
            node.annotations.push(Annotation::Executable);
        } else if matches!(variable.var_type, VariableType::ExpressionList)  {
            node.annotations.push(Annotation::ExpressionList);
        }
        variable.var_type.clone()
    } else if let Some(scope) = data.stack.last() {
        if let Some(variable) = scope.variables.get(ident) {
            node.annotations.push(Annotation::Id(variable.id));
            if let Some(argc) = variable.argc {
                node.annotations.push(Annotation::Argc(argc));
            }
            if matches!(variable.var_type, VariableType::Executable) {
                node.annotations.push(Annotation::Executable);
            } else if matches!(variable.var_type, VariableType::ExpressionList)  {
                node.annotations.push(Annotation::ExpressionList);
            }
            variable.var_type.clone()
        } else {
            if pull_through {
                let len = data.stack.len();
                if let Some(prev_scope) = data.stack.get(len - 2) {
                    if let Some(variable) = prev_scope.variables.get(ident) {
                        node.annotations.push(Annotation::PullThrough(variable.id));
                        if let Some(argc) = variable.argc {
                            node.annotations.push(Annotation::Argc(argc));
                        }
                        if matches!(variable.var_type, VariableType::Executable) {
                            node.annotations.push(Annotation::Executable);
                        } else if matches!(variable.var_type, VariableType::ExpressionList)  {
                            node.annotations.push(Annotation::ExpressionList);
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
            var0: (1; 0);
            var1: 10;
            var1;
            var2: 2*2;
            var2: 4;
            var3: (1; x:2; y:1) -> [(y) -> [1]];
            2* 2 ? (2; 2);
            var0(1);

        ")).unwrap();

        assert_eq!(validate(&mut tree), Ok(()));
        println!("{}", tree.to_pretty_string(true));
    }
}