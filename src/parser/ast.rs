use crate::lexer::{Token, Tokenizer};

use super::parse;

pub enum AST_Node_Type {
    Operator(Token),
    Definition(String),
    
    ExpressionList,
    Expression,

    FunctionDefinition,

    Exec,
    
    Number(f64),
    TupleLiteral(Vec<f64>),
    Tuple,
    Matrix(usize, usize),

    Root,
}

pub struct ASTNode {
    pub node_type: AST_Node_Type,
    pub children: Vec<Box<ASTNode>>,
}

pub struct AbstractSyntaxTree {
    pub root: ASTNode
}

impl AbstractSyntaxTree {
    pub fn new(tokenizer: Tokenizer) -> AbstractSyntaxTree {
        parse(tokenizer)
    }
}