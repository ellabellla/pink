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

#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum VariableType {
    Number,
    Executable,
    Tuple(usize),
    Matrix(usize, usize),
}

#[allow(dead_code)]
impl VariableType {
    pub fn node_to_type(data: &SemanticData, node: &Box<ASTNode>) -> Result<VariableType, SemanticError> {
        match &node.node_type {
            ASTNodeType::Operator(_) => Ok(VariableType::Number),
            ASTNodeType::Exec => Ok(VariableType::Executable),
            ASTNodeType::Reduce => Ok(VariableType::Executable),
            ASTNodeType::ForEach => Ok(VariableType::Executable),
            ASTNodeType::Into => Ok(VariableType::Executable),
            ASTNodeType::ExpressionList(_) => Ok(VariableType::Executable),
            ASTNodeType::Tuple(size) => Ok(VariableType::Tuple(*size)),
            ASTNodeType::Indexed => Ok(VariableType::Number),
            ASTNodeType::Meta2D(width, height) => Ok(VariableType::Matrix(*width, *height)),
            ASTNodeType::Meta(size) => Ok(VariableType::Tuple(*size)),
            ASTNodeType::Reference(ref_type) => match ref_type {
                Token::Identifier(ident) => {
                    if let Some(variable) = data.globals.get(ident) {
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

#[allow(dead_code)]
#[derive(Debug)]
struct Variable {
    id: usize,
    var_type: VariableType,
}

impl Variable {
    pub fn new(id: usize, var_type: VariableType) -> Variable {
        Variable { id, var_type }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Scope {
    variables: HashMap<String, Variable>,
    id_front: usize
}

#[allow(dead_code)]
impl Scope {
    pub fn new() -> Scope {
        Scope { variables: HashMap::new(), id_front: 0 }
    }

    pub fn new_id(&mut self) -> usize {
        self.id_front += 1;
        self.id_front - 1
    }
}

#[allow(dead_code)]
struct SemanticData{
    stack: Vec<Scope>,
    globals: HashMap<String, Variable>,
    global_id_front: usize,
}

#[allow(dead_code)]
impl SemanticData {
    pub fn new() -> SemanticData {
        SemanticData { stack: vec![], globals: HashMap::<String, Variable>::new(), global_id_front: 0 }
    }

    pub fn new_global_id(&mut self) -> usize {
        self.global_id_front += 1;
        self.global_id_front - 1
    }
}

#[allow(dead_code)]
pub fn validate(ast: &mut AbstractSyntaxTree) -> Result<(), SemanticError> {
    let mut data = SemanticData::new();
    if let ASTNodeType::Root = ast.root.node_type {
        for i in 0..ast.root.children.len() {
            validate_statement(&mut data, &mut ast.root.children[i])?;
        }
        Ok(())
    } else {
        let root = &mut ast.root;
        create_semantic_error!(root, "expected statements")
    }
}

fn validate_statement(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    for i in 0..node.children.len() {
        if let ASTNodeType::Set(_) = node.children[i].node_type {
            validate_definition(data, &mut node.children[i])?;
        } else {
            validate_expression(data, &mut node.children[i])?;
        }
    }
    Ok(())
}

fn validate_definition(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    let set_type = is!(&node.node_type, "expected definition", ASTNodeType::Set(set_type))?;
    validate_expression(data, &mut node.children[1])?;
    let reference = is!(&node.children[0].node_type, "expected identifier in definition", ASTNodeType::Reference(reference))?;
    let ident = is!(reference, "expected identifier in definition", Token::Identifier(ident))?;
    let var_type = VariableType::node_to_type(data, &node.children[1])?;
    

    if let Some(variable) = data.globals.get(ident) {
        node.children[0].annotations.push(Annotation::GlobalId(variable.id));
        match variable.var_type {
            VariableType::Number => return Ok(()),
            _ => return create_semantic_error!(node, "non number variables cannot be redefined or modified"),
        }
    }

    if let Some(scope) = data.stack.last_mut() {
        if let Some(variable) = scope.variables.get(ident) {
            node.children[0].annotations.push(Annotation::Id(variable.id));
            match variable.var_type {
                VariableType::Number => Ok(()),
                _ => return create_semantic_error!(node, "non number variables cannot be redefined or modified"),
            }
        } else {
            match set_type {
                Token::Set => {
                    let id = scope.new_id();
                    scope.variables.insert(ident.clone(), Variable::new(id, var_type));
                    node.children[0].annotations.push(Annotation::Id(id));
                    Ok(())
                },
                _ => {
                    create_semantic_error!(node, "a variable must first be defined before it is modified")
                }
            }
        }
    } else {
        match set_type {
            Token::Set => {
                let id = data.new_global_id();
                data.globals.insert(ident.clone(), Variable::new(id, var_type));
                node.children[0].annotations.push(Annotation::GlobalId(id));
                Ok(())
            },
            _ => {
                create_semantic_error!(node, "a variable must first be defined before it is modified")
            }
        }
    }
}

fn validate_expression(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    match node.node_type {
        ASTNodeType::Operator(Token::If) => {
            validate_expression(data, &mut node.children[0])?;
            let len = is!(node.children[1].node_type, "expected a tuple", ASTNodeType::Tuple(len))?;
            if len < 2 {
                return create_semantic_error!(node, "if requires a tuple with a length of at least 2")
            }
            validate_tuple(data, &mut node.children[1])
        },
        ASTNodeType::Operator(_) => {
            validate_expression(data, &mut node.children[0])?;
            validate_expression(data, &mut node.children[1])
        },
        _ => validate_value(data, node),
    }
}

fn validate_value(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    match node.node_type {
        ASTNodeType::Exec => validate_exec(data, node),
        ASTNodeType::Reduce => validate_extended_exec(data, node),
        ASTNodeType::ForEach => validate_extended_exec(data, node),
        ASTNodeType::Into => validate_extended_exec(data, node),
        ASTNodeType::ExpressionList(_) => validate_expression_list(data, node),
        ASTNodeType::Tuple(_) => validate_tuple(data, node),
        ASTNodeType::Indexed => validate_indexed(data, node),
        ASTNodeType::Reference(_) => validate_value_reference(data, node),
        ASTNodeType::Number(_) => Ok(()),
        _ => create_semantic_error!(node, "invalid value")
    }
}

fn validate_exec(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    data.stack.push(Scope::new());
    let res = validate_expression_list(data, node)
    .or_else(|_| 
        validate_tuple(data, &mut node.children[0])
        .and_then(|_| {
            let ident = is!(&node.children[0].node_type, "expected identifier", ASTNodeType::Reference(reference))
            .and_then(|reference| is!(reference, "expected identifier", Token::Identifier(ident)));

            if let Ok(ident) = ident {
                if let Some(scope) = data.stack.last() {
                    if let Some(variable) = scope.variables.get(ident) {
                        if matches!(variable.var_type, VariableType::Executable) {
                            node.children[0].annotations.push(Annotation::Id(variable.id));
                            return Ok(())
                        }
                    }
                }
                if let Some(variable) = data.globals.get(ident) {
                    if matches!(variable.var_type, VariableType::Executable) {
                        node.children[0].annotations.push(Annotation::GlobalId(variable.id));
                        return Ok(())
                    }
                }
            }


            validate_expression_list(data, &mut node.children[1])
        })
    );    
    let scope = data.stack.pop().unwrap();
    if res.is_ok() {
        node.annotations.push(Annotation::Scope(scope));
    }
    res
}

fn validate_extended_exec(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    validate_exec(data, &mut node.children[1])?;
    match &node.children[0].node_type {
        ASTNodeType::Meta(_) => Ok(()),
        ASTNodeType::Tuple(_) => validate_tuple(data, node),
        ASTNodeType::Indexed => validate_indexed(data, node),
        ASTNodeType::Reference(Token::Identifier(ident)) => {
            if let Some(variable) = data.globals.get(ident) {
                node.children[0].annotations.push(Annotation::GlobalId(variable.id));
                is!(variable.var_type, "variable must be defined before it is used", VariableType::Matrix(width, height)).map(|_|())
                .or_else(|_| is!(variable.var_type, "variable must be defined before it is used", VariableType::Tuple(size)).map(|_|()))
            } else {
                let scope = is!(data.stack.last(), "variable must be defined before it is used", Some(scope))?;
                let variable = is!(scope.variables.get(ident), "variable must be defined before it is used", Some(variable))?;
                node.children[0].annotations.push(Annotation::Id(variable.id));
                is!(variable.var_type, "variable must be defined before it is used", VariableType::Matrix(width, height)).map(|_|())
                .or_else(|_| is!(variable.var_type, "variable must be defined before it is used", VariableType::Tuple(size)).map(|_|()) )           }
        },
        _ => create_semantic_error!(node, "invalid lhs of into")
    }
}

fn validate_expression_list(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    for i in 0..node.children.len() {
        if matches!(node.children[i].node_type, ASTNodeType::Throw) || matches!(node.children[i].node_type, ASTNodeType::Push) {
            continue;
        } else {
            validate_definition(data, &mut node.children[i])
            .or_else(|_| validate_expression(data, &mut node.children[i]))?
        }
    }
    Ok(())
}


fn validate_tuple(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {    
    for i in 0..node.children.len() {
        if matches!(node.children[i].node_type, ASTNodeType::Throw) || matches!(node.children[i].node_type, ASTNodeType::Push) {
            continue;
        } else {
            validate_definition(data, &mut node.children[i])
            .or_else(|_| validate_expression(data, &mut node.children[i]))?
        }
    }
    Ok(())
}

fn validate_indexed(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    let reference = is!(&node.children[0].node_type, "expected identifier", ASTNodeType::Reference(reference))?;
    let ident = is!(reference, "expected identifier", Token::Identifier(ident))?;
    let variable = if let Some(variable) = data.globals.get(ident) {
        node.children[0].annotations.push(Annotation::GlobalId(variable.id));
        variable
    } else if let Some(scope) = data.stack.last() {
        if let Some(variable) = scope.variables.get(ident) {
            node.children[0].annotations.push(Annotation::Id(variable.id));
            variable
        } else {
            create_semantic_error!(node, "identifier must be defined before it is used")?
        }
    } else {
        create_semantic_error!(node, "identifier must be defined before it is used")?
    };
    match node.children[1].node_type {
        ASTNodeType::Meta(size) => {
            let variable_size = is!(variable.var_type, "variable must be a tuple", VariableType::Tuple(size))?;
            is_true!(size, "index exceeds bounds of tuple", variable_size, <)?;
        }, 
        ASTNodeType::Meta2D(width, height) => {
            let (variable_width, variable_height) = is!(variable.var_type, "variable must be a matrix", VariableType::Matrix(width, height))?;
            is_true!(width, "x index exceeds bounds of matrix", variable_width, <)?;
            is_true!(height, "y index exceeds bounds of matrix", variable_height, <)?;
        },
        _ => (),
    }
    Ok(())
}

fn validate_value_reference(data: &mut SemanticData, node: &mut Box<ASTNode>) -> Result<(), SemanticError> {
    let reference = is!(&node.node_type, "expected reference", ASTNodeType::Reference(reference))?;
    let ident = is!(reference, "expected identifier", Token::Identifier(ident));
    if ident.is_err() {
        return Ok(())
    }
    let ident = ident.unwrap();

    let variable = if let Some(variable) = data.globals.get(ident) {
        node.annotations.push(Annotation::GlobalId(variable.id));
        variable
    } else if let Some(scope) = data.stack.last() {
        if let Some(variable) = scope.variables.get(ident) {
            node.annotations.push(Annotation::Id(variable.id));
            variable
        } else {
            create_semantic_error!(node, "identifier must be defined before it is used")?
        }
    } else {
        create_semantic_error!(node, "identifier must be defined before it is used")?
    };

    is!(variable.var_type, "", VariableType::Number)
    .or_else(|_| is!(variable.var_type, "value of a variable in a expression must be a number or tuple", VariableType::Tuple(size)).map(|_|()))
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
            var3: (1; x:2) -> [x];
            2* 2 ? (2; 2);
        "));

        assert_eq!(validate(&mut tree), Ok(()));
        println!("{}", tree.to_pretty_string(true));
    }
}