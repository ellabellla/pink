use std::fmt;

use crate::lexer::{Tokenizer, Token};
use super::{AbstractSyntaxTree, ASTNode, ASTNodeType, StatementType};

#[macro_use] 
mod macros {
    #[macro_export]
    macro_rules! get_next {
        ($parser:ident.$tokenizer:ident, $fallback:ident, Token::$pattern:ident($($vars:ident)*) ) => {
            if let Some(token) = $parser.$tokenizer.next() { 
                match token {
                    Token::$pattern($($vars)*) => Ok($($vars)*),
                    _ => {
                        $parser.$tokenizer.seek($fallback);
                        let (pos, line, line_index) = $fallback;
                        Err(ParseError::new(format!(stringify!(expected Token::$pattern at line: {} and index: {}), line, pos - line_index)))
                    }
                }
            } else {
                let (pos, line, line_index) = $fallback;
                Err(ParseError::new(format!(stringify!(expected Token::$pattern at line: {} and index: {}), line, pos - line_index)))
            }
        };
    }

    #[macro_export]
    macro_rules! is_next_any {
        ($parser:ident.$tokenizer:ident, $fallback:ident, $(Token::$pattern:ident$(($($vars:ident)*))? $(,)?)+) => {
            if let Some(token) = $parser.$tokenizer.next() { 
                match token {
                    $(Token::$pattern$(($($vars)*))?=> Ok(token),)+
                    _ => {
                        $parser.$tokenizer.seek($fallback);
                        let (pos, line, line_index) = $fallback;
                        Err(ParseError::new(format!(stringify!(expected $(Token::$pattern, )* at line: {} and index: {}), line, pos - line_index)))
                    }
                }
            } else {
                let (pos, line, line_index) = $fallback;
                Err(ParseError::new(format!(stringify!(expected $(Token::$pattern, )* at line: {} and index: {}), line, pos - line_index)))
            }
        };
    }

    #[macro_export]
    macro_rules! is_next {
        ($parser:ident.$tokenizer:ident, $fallback:ident, Token::$pattern:ident$(($($vars:ident)*))?) => {
            if let Some(token) = $parser.$tokenizer.next() { 
                match token.clone() {
                    Token::$pattern$(($($vars)*))?=> Ok(token),
                    _ => {
                        $parser.$tokenizer.seek($fallback);
                        let (pos, line, line_index) = $fallback;
                        Err(ParseError::new(format!(stringify!(expected Token::$pattern at line: {} and index: {}), line, pos - line_index)))
                    }
                }
            } else {
                let (pos, line, line_index) = $fallback;
                Err(ParseError::new(format!(stringify!(expected Token::$pattern at line: {} and index: {}), line, pos - line_index)))
            }
        };
    }

    #[macro_export]
    macro_rules! is_next_continue {
        ($parser:ident.$tokenizer:ident, Token::$pattern:ident$(($($vars:ident)*))? ) => {
            if let fallback = $parser.$tokenizer.pos() {
                if let Some(token) = $parser.$tokenizer.next() { 
                    match token {
                        Token::$pattern$(($($vars)*))?=> Ok(token),
                        _ => {
                            $parser.$tokenizer.seek(fallback);
                            let (pos, line, line_index) = fallback;
                            Err(ParseError::new(format!(stringify!(expected Token::$pattern at line: {} and index: {}), line, pos - line_index)))
                        }
                    } 
                } else {
                    let (pos, line, line_index) = fallback;
                    Err(ParseError::new(format!(stringify!(expected Token::$pattern at line: {} and index: {}), line, pos - line_index)))
                }
            } else {
                panic!("unreachable code");
            }
        };
    }

    #[macro_export]
    macro_rules! or {
        ($parser:ident.$tokenizer:ident, $err:tt, $fallback:ident, $($parse:ident $(,)?)+) => {
            $(
                if let Ok(node) = $parse($parser) {
                    Ok(node)
                } else
            )+
            {
                let err = create_parse_error!($parser.$tokenizer, $err);
                $parser.$tokenizer.seek($fallback);
                err
            }
        };
    }

    #[macro_export]
    macro_rules! or_continue {
        ($parser:ident.$tokenizer:ident, $err:tt, $($parse:ident $(,)?)+) => {
            $(
                if let Ok(node) = $parse($parser) {
                    Ok(node)
                } else
            )+
            {
                create_parse_error!($parser.$tokenizer, $err)
            }
        };
    }

    #[macro_export]
    macro_rules! get_or_fallback {
        ($parser:ident.$tokenizer:ident, $fallback:ident, $parse:ident) => {
            match $parse($parser) {
                Ok(node) => Ok(node),
                Err(err) => {
                    $parser.$tokenizer.seek($fallback);
                    Err(err)
                }
            }
        };
    }

    #[macro_export]
    macro_rules! create_parse_error {
        ($parser:ident.$tokenizer:ident, $err:tt) => {
            if let (pos, line, line_index) = $parser.$tokenizer.pos() {
                Err(ParseError::new(format!(stringify!($err at line: {} and index: {}), line, pos - line_index)))
            } else {
                panic!("unreachable code");
            }
        };
    }

    #[macro_export]
    macro_rules! create_token_parse_error {
        ($parser:ident.$tokenizer:ident, $(Token::$pattern:ident$(($($vars:ident)*))? $(,)?)+) => {
            if let (pos, line, line_index) = $parser.$tokenizer.pos() {
                Err(ParseError::new(format!(stringify!(expected $(Token::$pattern, )* at line: {} and index: {}), line, pos - line_index)))
            } else {
                panic!("unreachable code");
            }
        };
    }

    #[macro_export]
    macro_rules! assert_parse_eq {
        ($input:tt, $output:tt) => {
            assert_eq!(AbstractSyntaxTree::new(&mut Tokenizer::new($input)).to_string(), $output);
        };
    }
}

pub struct ParseError {
    msg: String
}

impl ParseError {
    pub fn new(msg: String) -> ParseError {
        ParseError { msg }
    }

    pub fn to_string(&self) -> String {
        self.msg.clone()
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

struct Parser<'a> {
    tokenizer: &'a mut Tokenizer,
    tree: AbstractSyntaxTree,
}

pub fn parse(tokenizer: &mut Tokenizer) -> AbstractSyntaxTree {
    let mut tree = AbstractSyntaxTree{root: Box::new(ASTNode::new(ASTNodeType::Root, vec![]))};
    let mut parser = Parser{tokenizer: tokenizer, tree};

    if let Err(err) = parse_root(&mut parser) {
        panic!("{}", err.to_string());
    }

    parser.tree
}

fn parse_root(parser: &mut Parser) -> Result<(), ParseError> {
    let mut fallback = parser.tokenizer.pos();
    let mut statement = parse_statement(parser);
    while let Ok(child) = statement {
        parser.tree.root.children.push(child);
        if is_next_continue!(parser.tokenizer, Token::EOF).is_ok() {
            return Ok(())
        }
        statement = parse_statement(parser);
    }
    parser.tokenizer.seek(fallback);
    Err(statement.err().unwrap())
}


fn parse_setters(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let token = is_next_any!(parser.tokenizer, fallback, 
        Token::Set, 
        Token::AddAndSet, 
        Token::SubtractAndSet, 
        Token::MultiplyAndSet,
        Token::DivideAndSet,
    )?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Set(token), vec![])))
}

fn parse_number(parser: &mut Parser) ->  Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let number = get_next!(parser.tokenizer, fallback, Token::Number(_num))?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Number(number), vec![])))
}


fn parse_reference(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut token = create_parse_error!(parser.tokenizer, "expected a reference") ;
    if let Ok(found_token) =  is_next_continue!(parser.tokenizer, Token::Pop) {
        token = Ok(found_token);
    } else if let Ok(found_token) = is_next_continue!(parser.tokenizer, Token::Peek) {
        token = Ok(found_token);
    } else {
        token = Ok(Token::Identifier(get_next!(parser.tokenizer, fallback, Token::Identifier(ident))?));
    }

    if let Ok(token) = token {
        Ok(Box::new(ASTNode::new(ASTNodeType::Reference(token), vec![])))
    } else {
        Err(token.err().unwrap())
    }
}

fn parse_value(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    if let Ok(node) = parse_indexed_value(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_reference(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_number(parser) {
        return Ok(node)
    } else if let Ok(found_token) = is_next_continue!(parser.tokenizer, Token::Not) {
        let number = get_next!(parser.tokenizer, fallback, Token::Number(_num))?;
        Ok(Box::new(ASTNode{node_type: ASTNodeType::Operator(found_token), children: vec![
            Box::new(ASTNode::new(ASTNodeType::Number(number), vec![]))
        ]}))
    } else if let Ok(found_token) = is_next_continue!(parser.tokenizer, Token::True) {
        Ok(Box::new(ASTNode::new(ASTNodeType::Number(1.0), vec![])))
    } else if let Ok(found_token) = is_next_continue!(parser.tokenizer, Token::False) {
        Ok(Box::new(ASTNode::new(ASTNodeType::Number(0.0), vec![])))
    } else if let Ok(node) = parse_exec(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_reduce(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_for_each(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_into(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_tuple(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_meta(parser) {
        return Ok(node)
    } else {
        let err = create_parse_error!(parser.tokenizer, "expected a value");
        parser.tokenizer.seek(fallback);
        err
    }
}

fn parse_operator_1st(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let token = is_next_any!(parser.tokenizer, fallback, 
        Token::Multiply,
        Token::Divide,
    )?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Operator(token), vec![])))
}

fn parse_operator_2nd(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let token = is_next_any!(parser.tokenizer, fallback,
        Token::Add,
        Token::Subtract,
    )?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Operator(token), vec![])))
}

fn parse_operator_3rd(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let token = is_next_any!(parser.tokenizer, fallback,
        Token::Equals,
        Token::LessThan,
        Token::GreaterThan,
        Token::LessThanOrEqual,
        Token::GreaterThanOrEqual,
    )?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Operator(token), vec![])))
}

fn parse_operator_4th(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let token = is_next_any!(parser.tokenizer, fallback, 
        Token::And,
        Token::Or,
        Token::Xor,
    )?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Operator(token), vec![])))
}


fn parse_expression(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let found_negative = is_next_continue!(parser.tokenizer, Token::Subtract).is_ok();
    let mut lhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_3rd)?;

    if found_negative {
        let mut operator = Box::new(ASTNode::new(ASTNodeType::Operator(Token::Subtract), vec![
            Box::new(ASTNode::new(ASTNodeType::Number(0.0), vec![])),
            lhs
        ]));
        lhs = operator;
    }

    while let Ok(mut operator) = parse_operator_4th(parser) {
        let rhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_3rd)?;

        operator.children.push(lhs);
        operator.children.push(rhs);
        lhs = operator;
    }  

    Ok(lhs)
}

fn parse_expression_3rd(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut lhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_2nd)?;

    while let Ok(mut operator) = parse_operator_3rd(parser) {
        let rhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_2nd)?;

        operator.children.push(lhs);
        operator.children.push(rhs);
        lhs = operator;
    }  

    Ok(lhs)
}

fn parse_expression_2nd(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut lhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_1st)?;

    while let Ok(mut operator) = parse_operator_2nd(parser) {
        let rhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_1st)?;

        operator.children.push(lhs);
        operator.children.push(rhs);
        lhs = operator;
    }  

    Ok(lhs)
}

fn parse_expression_1st(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut lhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_value)?;

    while let Ok(mut operator) = parse_operator_1st(parser) {
        let rhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_value)?;

        operator.children.push(lhs);
        operator.children.push(rhs);
        lhs = operator;
    }  

    Ok(lhs)
}

fn parse_expression_value(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    Ok(or!(parser.tokenizer, "expected a value", fallback, parse_value)?)
}

/*fn parse_expression_bracketed(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    is_next!(parser.tokenizer, fallback, Token::OpenParentheses)?;
    let expression = get_or_fallback!(parser.tokenizer, fallback, parse_expression)?;
    is_next!(parser.tokenizer, fallback, Token::CloseParentheses)?;
    Ok(expression)
}*/


fn parse_definition(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let identifier = is_next!(parser.tokenizer, fallback, Token::Identifier(_ident))?;
    let mut setter: Box<ASTNode> = get_or_fallback!(parser.tokenizer, fallback, parse_setters)?;
    setter.children.push(Box::new(ASTNode{node_type:ASTNodeType::Reference(identifier), children: vec![]}));
    let expression: Box<ASTNode> = get_or_fallback!(parser.tokenizer, fallback, parse_expression)?;
    setter.children.push(expression);

    Ok(setter)
}

fn parse_strict_definition(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut setter = Box::new(ASTNode::new(ASTNodeType::Set(Token::Set), vec![]));
    let identifier = is_next!(parser.tokenizer, fallback, Token::Identifier(_ident))?;
    setter.children.push(Box::new(ASTNode{node_type:ASTNodeType::Reference(identifier), children: vec![]}));
    is_next!(parser.tokenizer, fallback, Token::Set)?;
    let expression: Box<ASTNode> = get_or_fallback!(parser.tokenizer, fallback, parse_expression)?;
    setter.children.push(expression);

    Ok(setter)
}

fn parse_expression_list(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    
    is_next!(parser.tokenizer, fallback, Token::OpenBracket)?;

    let mut children = vec![];
    if let Ok(node) = or_continue!(parser.tokenizer, "expected an expression or definition", parse_definition, parse_expression) {
        children.push(node);
    } else if is_next_continue!(parser.tokenizer, Token::CloseBracket).is_ok() {
        return Ok(Box::new(ASTNode{node_type: ASTNodeType::ExpressionList, children}))
    } else {
        return create_parse_error!(parser.tokenizer, "expected an expression or definition")
    }

    if is_next_continue!(parser.tokenizer, Token::CloseBracket).is_ok() {
        return Ok(Box::new(ASTNode{node_type: ASTNodeType::ExpressionList, children}))
    } else if is_next_continue!(parser.tokenizer, Token::SeparateAndPush).is_ok() {
        children.push(Box::new(ASTNode::new(ASTNodeType::Push, vec![])))
    } else if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
        children.push(Box::new(ASTNode::new(ASTNodeType::Throw, vec![])))
    } else {
        let err = create_token_parse_error!(parser.tokenizer, Token::CloseBracket, Token::SeparateAndPush, Token::Separate);
        parser.tokenizer.seek(fallback);
        return err 
    }

    loop {
        if is_next_continue!(parser.tokenizer, Token::CloseBracket).is_ok() {
            return Ok(Box::new(ASTNode{node_type: ASTNodeType::ExpressionList, children}))
        }

        children.push(or!(parser.tokenizer, "expected an expression or definition", fallback, parse_expression, parse_definition)?);

        if is_next_continue!(parser.tokenizer, Token::SeparateAndPush).is_ok() {
            children.push(Box::new(ASTNode::new(ASTNodeType::Push, vec![])))
        } else if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
            children.push(Box::new(ASTNode::new(ASTNodeType::Throw, vec![])))
        } else if is_next_continue!(parser.tokenizer, Token::CloseBracket).is_ok() {
            return Ok(Box::new(ASTNode{node_type: ASTNodeType::ExpressionList, children}))
        } else {
            let err = create_token_parse_error!(parser.tokenizer, Token::CloseBracket, Token::SeparateAndPush, Token::Separate);
            parser.tokenizer.seek(fallback);
            return err
        }
    }
    
}


fn parse_exec(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut res = create_parse_error!(parser.tokenizer, "expected expression list or tuple") ;
    if let Ok(tuple) = parse_tuple(parser) {
        if is_next_continue!(parser.tokenizer, Token::Exec).is_ok() {
            if let Ok(expression_list) = parse_expression_list(parser) {
                res =Ok(Box::new(ASTNode::new(ASTNodeType::Exec, vec![tuple, expression_list])))
            }
        }
    }
    if res.is_ok() {
        res
    } else {
        parser.tokenizer.seek(fallback);
        parse_expression_list(parser)
    }
}


fn  parse_into(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let data = or!(parser.tokenizer, "expected a meta, tuple or reference", fallback, parse_meta, parse_tuple, parse_indexed_value, parse_reference)?;
    is_next!(parser.tokenizer, fallback, Token::Into)?;
    let exec =get_or_fallback!(parser.tokenizer, fallback, parse_exec)?;

    Ok(Box::new(ASTNode{node_type:ASTNodeType::Into, children: vec![data, exec]}))
}

fn  parse_for_each(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let data = or!(parser.tokenizer, "expected a meta, tuple or reference", fallback, parse_meta, parse_tuple, parse_indexed_value, parse_reference)?;
    is_next!(parser.tokenizer, fallback, Token::ForEach)?;
    let exec =get_or_fallback!(parser.tokenizer, fallback, parse_exec)?;

    Ok(Box::new(ASTNode{node_type:ASTNodeType::ForEach, children: vec![data, exec]}))
}

fn  parse_reduce(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let data = or!(parser.tokenizer, "expected a meta, tuple or reference", fallback, parse_meta, parse_tuple, parse_indexed_value, parse_reference)?;
    is_next!(parser.tokenizer, fallback, Token::Reduce)?;
    let exec =get_or_fallback!(parser.tokenizer, fallback, parse_exec)?;

    Ok(Box::new(ASTNode{node_type:ASTNodeType::Reduce, children: vec![data, exec]}))
}


fn  parse_tuple(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    
    is_next!(parser.tokenizer, fallback, Token::OpenParentheses)?;

    let mut children = vec![];
    if let Ok(node) = or_continue!(parser.tokenizer, "expected an expression or strict definition", parse_strict_definition, parse_expression) {
        children.push(node);
    } else if is_next_continue!(parser.tokenizer, Token::CloseParentheses).is_ok() {
        return Ok(Box::new(ASTNode{node_type: ASTNodeType::Tuple, children}))
    } else {
        return create_parse_error!(parser.tokenizer, "expected an expression or strict definition");
    }

    loop {
        if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
            children.push(Box::new(ASTNode::new(ASTNodeType::Throw, vec![])))
        } else if is_next_continue!(parser.tokenizer, Token::CloseParentheses).is_ok() {
            return Ok(Box::new(ASTNode{node_type: ASTNodeType::Tuple, children}))
        } else {
            let err = create_token_parse_error!(parser.tokenizer, Token::Separate, Token::CloseParentheses);
            parser.tokenizer.seek(fallback);
            return err
        }

        children.push(or!(parser.tokenizer, "expected an expression or strict definition", fallback, parse_expression, parse_strict_definition)?);
    }
}

fn parse_indexed_value(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let reference = get_or_fallback!(parser.tokenizer, fallback, parse_reference)?;
    let meta = get_or_fallback!(parser.tokenizer, fallback, parse_meta)?;

    Ok(Box::new(ASTNode{node_type:ASTNodeType::Indexed, children: vec![reference, meta]}))
}

fn parse_meta(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let mut fallback = parser.tokenizer.pos();

    is_next!(parser.tokenizer, fallback, Token::OpenBrace)?;

    let width = get_next!(parser.tokenizer, fallback, Token::Number(num))?;
    let mut height = create_parse_error!(parser.tokenizer, "expected a value") ;

    if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
        height = Ok(get_next!(parser.tokenizer, fallback, Token::Number(num))?);
    }

    let ast_type = if let Ok(height) = height {
        ASTNodeType::Meta2D(width.floor() as usize, height.floor() as usize)
    } else {
        ASTNodeType::Meta(width.floor() as usize)
    };

    is_next!(parser.tokenizer, fallback, Token::CloseBrace)?;

    Ok(Box::new(ASTNode::new(ast_type, vec![])))
    
}

fn parse_statement(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut statement_type = StatementType::Push;
    let mut children  = vec![];
    children.push(or!(parser.tokenizer, "expected an expression or definition", fallback, parse_definition, parse_expression)?);
    

    if is_next_continue!(parser.tokenizer, Token::SeparateAndPush).is_ok() {
        statement_type = StatementType::Push;
    } else if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
        statement_type = StatementType::Throw;
    } else {
        let err = create_token_parse_error!(parser.tokenizer, Token::SeparateAndPush, Token::Separate);
        parser.tokenizer.seek(fallback);
        return err;
    }
    
    Ok(Box::new(ASTNode { node_type: ASTNodeType::Statement(statement_type), children: children }))
}


#[cfg(test)]
mod tests {
    use crate::{lexer::Tokenizer, parser::AbstractSyntaxTree};

    use super::parse;

    #[test]
    fn test_arithmetic() {
        assert_parse_eq!(r"
            2+2*3-1/2;
        ", "(((((Number(2.0))((Number(2.0))(Number(3.0))Operator(Multiply))Operator(Add))((Number(1.0))(Number(2.0))Operator(Divide))Operator(Subtract))Statement(Throw))Root)");
    }

    #[test]
    fn test_arithmetic_bracketed() {
        assert_parse_eq!(r"
            (2+2)*6;
        ", "((((((Number(2.0))(Number(2.0))Operator(Add))Tuple)(Number(6.0))Operator(Multiply))Statement(Throw))Root)");
    }

    #[test]
    fn test_comparator() {
        assert_parse_eq!(r"
            1+2<3;
        ", "(((((Number(1.0))(Number(2.0))Operator(Add))(Number(3.0))Operator(LessThan))Statement(Throw))Root)");
    }

    #[test]
    fn test_logic() {
        assert_parse_eq!(r"
            2*3and10*2;
        ", "(((((Number(2.0))(Number(3.0))Operator(Multiply))((Number(10.0))(Number(2.0))Operator(Multiply))Operator(And))Statement(Throw))Root)");
    }

    #[test]
    fn test_refs() {
        assert_parse_eq!(r"
            var*2;
            2*@;
            @@*@;
        ", "((((Reference(Identifier(\"var\")))(Number(2.0))Operator(Multiply))Statement(Throw))(((Number(2.0))(Reference(Pop))Operator(Multiply))Statement(Throw))(((Reference(Peek))(Reference(Pop))Operator(Multiply))Statement(Throw))Root)");
    }

    #[test]
    fn test_values() {
        assert_parse_eq!(r"
             10;
             !10;
             true;
             false;
        ", "(((Number(10.0))Statement(Throw))(((Number(10.0))Operator(Not))Statement(Throw))((Number(1.0))Statement(Throw))((Number(0.0))Statement(Throw))Root)");
    }

    #[test]
    fn test_meta() {
        assert_parse_eq!(r"
             {1},
             {1;2},
             var{1;2};
        ", "(((Meta(1))Statement(Push))((Meta2D(1, 2))Statement(Push))(((Reference(Identifier(\"var\")))(Meta2D(1, 2))Indexed)Statement(Throw))Root)");
    }

    #[test]
    fn test_func() {
        assert_parse_eq!(r"()->[];", "((((Tuple)(ExpressionList)Exec)Statement(Throw))Root)");

        assert_parse_eq!(r"()*>[];", "((((Tuple)(ExpressionList)Reduce)Statement(Throw))Root)");

        assert_parse_eq!(r"()<-[];", "((((Tuple)(ExpressionList)Into)Statement(Throw))Root)");

        assert_parse_eq!(r"()<*[];", "((((Tuple)(ExpressionList)ForEach)Statement(Throw))Root)");
    }

    #[test]
    fn test_expression_list() {
        assert_parse_eq!("[];", "(((ExpressionList)Statement(Throw))Root)");
        assert_parse_eq!("[1,2];", "((((Number(1.0))(Push)(Number(2.0))ExpressionList)Statement(Throw))Root)");
        assert_parse_eq!("[1;2];", "((((Number(1.0))(Throw)(Number(2.0))ExpressionList)Statement(Throw))Root)");
        assert_parse_eq!("[1+2];", "(((((Number(1.0))(Number(2.0))Operator(Add))ExpressionList)Statement(Throw))Root)");
        assert_parse_eq!("[var:10];", "(((((Reference(Identifier(\"var\")))(Number(10.0))Set(Set))ExpressionList)Statement(Throw))Root)");
    }

    #[test]
    fn test_tuple() {
        assert_parse_eq!("();", "(((Tuple)Statement(Throw))Root)");
        assert_parse_eq!("(1;2);", "((((Number(1.0))(Throw)(Number(2.0))Tuple)Statement(Throw))Root)");
        assert_parse_eq!("(1+2);", "(((((Number(1.0))(Number(2.0))Operator(Add))Tuple)Statement(Throw))Root)");
        assert_parse_eq!("(var:10);", "(((((Reference(Identifier(\"var\")))(Number(10.0))Set(Set))Tuple)Statement(Throw))Root)");
    }

}
