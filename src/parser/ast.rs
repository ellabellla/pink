use crate::lexer::{Token, Tokenizer};

use super::parse;

#[derive(Debug)]
pub enum StatementType {
    Push,
    Throw,
}

#[derive(Debug)]
pub enum ASTNodeType {
    Operator(Token),

    Exec,
    Reduce,
    ForEach,
    Into,

    ExpressionList,
    Push,
    Throw,

    Set(Token),

    Tuple,

    Indexed,
    
    Meta2D(usize, usize),
    Meta(usize),

    Reference(Token),
    Number(f64),

    Statement(StatementType),

    Root,
}

pub struct ASTNode {
    pub node_type: ASTNodeType,
    pub children: Vec<Box<ASTNode>>,
}

impl ASTNode {
    pub fn new(node_type: ASTNodeType, children: Vec<Box<ASTNode>>) ->  ASTNode{
        ASTNode { node_type, children }
    }
}

pub struct AbstractSyntaxTree {
    pub root: Box<ASTNode>
}

impl AbstractSyntaxTree {
    pub fn new(tokenizer: &mut Tokenizer) -> AbstractSyntaxTree {
        parse(tokenizer)
    }

    pub fn to_string(&self) -> String {
        let mut output = vec![];
        AbstractSyntaxTree::to_string_helper(&self.root, &mut output);

        output.iter().collect()
    }

    fn to_string_helper(node: &Box<ASTNode>, output: &mut Vec<char>) {
        output.push('(');
        for i in 0..node.children.len() {
            AbstractSyntaxTree::to_string_helper(&node.children[i], output);
        }
        for c in format!("{:?}", node.node_type).chars() {
            output.push(c);
        }
        output.push(')');
    }

    pub fn to_pretty_string(&self) -> String {
        let mut output = vec![];
        AbstractSyntaxTree::to_pretty_helper(&self.root, &mut output, 0);

        output.iter().collect()
    }

    fn to_pretty_helper(node: &Box<ASTNode>, output: &mut Vec<char>, depth: usize) {
        output.push('\n');
        for _ in 0..depth {
            output.push(' ');
        }
        output.push('(');
        
        for i in 0..node.children.len() {
            AbstractSyntaxTree::to_pretty_helper(&node.children[i], output, depth + 1);
        }

        if node.children.len() != 0 {
            output.push('\n');
            for _ in 0..depth+1 {
                output.push(' ');
            }
        }
        for c in format!("{:?}", node.node_type).chars() {
            output.push(c);
        }

        if node.children.len() != 0 {
            output.push('\n');
            for _ in 0..depth {
                output.push(' ');
            }
        }
        output.push(')');
    }
}