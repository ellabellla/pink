use crate::lexer::{Tokenizer, Token};
use super::{AbstractSyntaxTree, ASTNode, AST_Node_Type};

#[macro_use] 
mod macros {
    #[macro_export]
    macro_rules! get_next {
        ($parser:ident.$tokenizer:ident, $fallback:ident, Token::$pattern:ident($($vars:ident)*) ) => {
            $parser.$tokenizer.next().and_then(|token| 
                match token {
                    Token::$pattern($($vars)*) => Some($($vars)*),
                    _ => {
                        $parser.$tokenizer.seek($fallback);
                        None
                    }
                });
        };
    }
}

struct Parser {
    tokenizer: Tokenizer,
    tree: AbstractSyntaxTree,
}

pub fn parse(tokenizer: Tokenizer) -> AbstractSyntaxTree {
    let mut tree = AbstractSyntaxTree{root: ASTNode{node_type: AST_Node_Type::Root, children: vec![]}};
    let mut parser = Parser{tokenizer, tree};

    parse_root(&mut parser);

    parser.tree
}

fn parse_root(parser: &mut Parser) {
    while let Some(child) = parse_define(parser) {
        println!("{}", parser.tokenizer.pos());
        parser.tree.root.children.push(child);
    }
    println!("{}", parser.tokenizer.pos());
    println!("len {}", parser.tree.root.children.len());
}

fn parse_define(parser: &mut Parser) -> Option<Box<ASTNode>> {
    let mut fallback = parser.tokenizer.pos();

    let ident = get_next!(parser.tokenizer, fallback, Token::IDENTIFIER(ident))?;

    is_next(&mut parser.tokenizer, Token::SET, fallback)?;

    let definition = get_next!(parser.tokenizer, fallback, Token::NUMBER(num))?;
    let definition = Box::new(ASTNode{node_type:AST_Node_Type::Number(definition), children: vec![]});
    /*let definition = parser.tokenizer.next().and_then(|token| 
        match token {
            Token::NUMBER(num) => Some(Box::new(ASTNode{node_type:AST_Node_Type::Number(num), children: vec![]})),
            _=> parse_expression(parser),
        })?;
    */

    is_next(&mut parser.tokenizer, Token::COMMA, fallback)?;

    Some(Box::new(ASTNode{node_type: AST_Node_Type::Definition(ident), children: vec![definition]}))
}

fn parse_expression(parser: &mut Parser) -> Option<Box<ASTNode>> {
    None
}

fn parse_matrix(parser: &mut Parser) -> Option<Box<ASTNode>> {
    let mut fallback = parser.tokenizer.pos();

    is_next(&mut parser.tokenizer, Token::OPEN_BRACKET, fallback)?;

    let width = get_next!(parser.tokenizer, fallback, Token::NUMBER(num))?;
    
    is_next(&mut parser.tokenizer, Token::RANGE, fallback)?;


    let height = parser.tokenizer.next().and_then(|token| 
        match token {
            Token::NUMBER(num) => Some(num),
            _=>None,
        })?;

    is_next(&mut parser.tokenizer, Token::CLOSE_BRACKET, fallback)?;

    Some(Box::new(ASTNode{node_type: AST_Node_Type::Matrix(width.floor() as usize, height.floor() as usize), children: vec![]}))
    
}

fn is_next(tokenizer: &mut Tokenizer, token: Token, fallback: usize) -> Option<()> {
    let next = tokenizer.next();
    if let Some(next_token) = next {
        if !matches!(next_token, token) {
            tokenizer.seek(fallback);
            None
        } else {
            Some(())
        }
    } else {
        tokenizer.seek(fallback);
        None
    }
}


#[cfg(test)]
mod tests {
    use crate::lexer::Tokenizer;

    use super::parse;

    #[test]
    fn test_parser() {
        let input = "variable:10,variable2:20,";
        let mut tokenizer = Tokenizer::new(input);
        let tree = parse(tokenizer);
        let a = 10;

    }
}
