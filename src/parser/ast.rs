use crate::lexer::{Token, Tokenizer};

use super::parse;

#[allow(dead_code)]
#[derive(Debug)]
pub enum StatementType {
    Push,
    Throw,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum ASTNodeType {
    Operator(Token),

    Exec,
    Reduce,
    ForEach,
    Into,

    ExpressionList(usize),
    Push,
    Throw,

    Set(Token),

    Tuple(usize),

    Indexed,
    
    Meta2D(usize, usize),
    Meta(usize),

    Reference(Token),
    Number(f64),

    Statement(StatementType),

    Root,
}

#[allow(dead_code)]
pub enum Annotation {
    Id(usize),
    DebugInfo(usize, usize, usize),
}

pub struct ASTNode {
    pub node_type: ASTNodeType,
    pub children: Vec<Box<ASTNode>>,
    pub annotations: Vec<Annotation>,
}

impl ASTNode {
    pub fn new(node_type: ASTNodeType, children: Vec<Box<ASTNode>>, debug_info: (usize, usize, usize)) ->  ASTNode{
        ASTNode { node_type, children, annotations: vec![Annotation::DebugInfo(debug_info.0, debug_info.1, debug_info.2)] }
    }
}

#[allow(dead_code)]

pub struct AbstractSyntaxTree {
    pub root: Box<ASTNode>
}

#[allow(dead_code)]
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