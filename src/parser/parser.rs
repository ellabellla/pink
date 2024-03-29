use std::{fmt, vec};

use crate::lexer::{Tokenizer, Token};
use super::{AbstractSyntaxTree, ASTNode, ASTNodeType, StatementType, Annotation};

#[macro_use] 
mod macros {
    
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

    
    macro_rules! is_next_continue {
        ($tokenizer:expr, Token::$pattern:ident$(($($vars:ident)*))? ) => {
            {
                let fallback = $tokenizer.pos();
                if let Some(token) = $tokenizer.next() { 
                    match token.clone() {
                        Token::$pattern$(($($vars)*))?=> Ok(token),
                        _ => {
                            $tokenizer.seek(fallback);
                            let (pos, line, line_index) = fallback;
                            Err(ParseError::new(format!(stringify!(expected Token::$pattern at line: {} and index: {}), line, pos - line_index)))
                        }
                    } 
                } else {
                    let (pos, line, line_index) = fallback;
                    Err(ParseError::new(format!(stringify!(expected Token::$pattern at line: {} and index: {}), line, pos - line_index)))
                }
            }
        };
    }

    
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

    
    macro_rules! create_parse_error {
        ($parser:ident.$tokenizer:ident, $err:tt) => {
            {
                let (pos, line, line_index) = $parser.$tokenizer.pos();
                Err(ParseError::new(format!(stringify!($err at line: {} and index: {}), line, pos - line_index)))
            }
        };
    }

    
    macro_rules! create_token_parse_error {
        ($parser:ident.$tokenizer:ident, $(Token::$pattern:ident$(($($vars:ident)*))? $(,)?)+) => {
            {
                let (pos, line, line_index) = $parser.$tokenizer.pos();
                Err(ParseError::new(format!(stringify!(expected $(Token::$pattern, )* at line: {} and index: {}), line, pos - line_index)))
            }
        };
    }

    #[cfg(test)]
    macro_rules! assert_parse_eq {
        ($input:tt, $output:tt) => {
            assert_eq!(AbstractSyntaxTree::new(&mut Tokenizer::new($input)).unwrap().to_string(false), $output);
        };
    }
}

#[derive(Debug)]
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

pub fn parse(tokenizer: &mut Tokenizer) -> Result<AbstractSyntaxTree, ParseError> {
    let tree = AbstractSyntaxTree{root: Box::new(ASTNode::new(ASTNodeType::Root, vec![], Annotation::pos_to_debug(tokenizer.pos())))};
    let mut parser = Parser{tokenizer: tokenizer, tree};

    parse_root(&mut parser)?;

    Ok(parser.tree)
}

fn parse_root(parser: &mut Parser) -> Result<(), ParseError> {
    let fallback = parser.tokenizer.pos();
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
    Ok(Box::new(ASTNode::new(ASTNodeType::Set(token), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn parse_number(parser: &mut Parser) ->  Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let number = get_next!(parser.tokenizer, fallback, Token::Number(_num))?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Number(number), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}


fn parse_reference(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let next = match parser.tokenizer.next() {
        Some(token) => token,
        None => return create_parse_error!(parser.tokenizer, "expected a reference"),
    };
    if let Token::Identifier(ident) = next {
        if let Ok((string, children)) = parse_call_internal(parser) {
            let ast_type = match string {
                Some(string) => ASTNodeType::CallStr(ident, string),
                None => ASTNodeType::Call(ident),
            };
            Ok(Box::new(ASTNode::new(ast_type, children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
        } else {
            Ok(Box::new(ASTNode::new(ASTNodeType::Reference(Token::Identifier(ident)), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
        }
    } else {
        Ok(Box::new(ASTNode::new(ASTNodeType::Reference(
            if matches!(next, Token::Pop) || matches!(next, Token::Peek) ||
                matches!(next, Token::Width) || matches!(next, Token::Height) {
                next
            } else {
                parser.tokenizer.seek(fallback);
                return create_parse_error!(parser.tokenizer, "expected reference")
            }
        ), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
    }
}

fn parse_call_internal(parser: &mut Parser) -> Result<(Option<String>, Vec<Box<ASTNode>>), ParseError> {
    let fallback = parser.tokenizer.pos();
    if let Ok(_) = is_next_continue!(parser.tokenizer, Token::Call) {
        let string = if let Ok(Token::ListString(string)) = is_next_continue!(parser.tokenizer, Token::ListString(_string)) {
            Some(string.clone())
        }  else {
            None
        };

        let mut children = vec![get_or_fallback!(parser.tokenizer, fallback, parse_expression)?];
        loop {
            if is_next_continue!(parser.tokenizer, Token::Separate).is_err() {
                if is_next_continue!(parser.tokenizer, Token::Call).is_ok() {
                    break;
                } else {
                    parser.tokenizer.seek(fallback);
                    return create_parse_error!(parser.tokenizer, "expected close of call")
                }
            }

            children.push(get_or_fallback!(parser.tokenizer, fallback, parse_expression)?);

            if is_next_continue!(parser.tokenizer, Token::Call).is_ok() {
                break;
            }
        }

        Ok((string, children))
    } else {
        parser.tokenizer.seek(fallback);
        create_parse_error!(parser.tokenizer, "expected external call arguments")
    }
}

fn parse_value(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    if let Ok(node) = parse_indexed_value(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_number(parser) {
        return Ok(node)
    } else if let Ok(found_token) = is_next_continue!(parser.tokenizer, Token::Not) {
        let number = get_next!(parser.tokenizer, fallback, Token::Number(_num))?;
        Ok(Box::new(ASTNode::new(ASTNodeType::Operator(found_token), vec![
            Box::new(ASTNode::new(ASTNodeType::Number(number), vec![], Annotation::pos_to_debug(parser.tokenizer.pos())))
        ], Annotation::pos_to_debug(parser.tokenizer.pos()))))
    } else if is_next_continue!(parser.tokenizer, Token::True).is_ok() {
        Ok(Box::new(ASTNode::new(ASTNodeType::Number(1.0), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
    } else if is_next_continue!(parser.tokenizer, Token::False).is_ok() {
        Ok(Box::new(ASTNode::new(ASTNodeType::Number(0.0), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
    } else if let Ok(node) = parse_tail_exec(parser) {
        return Ok(node)
    }else if let Ok(node) = parse_exec(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_reduce(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_for_each(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_into(parser) {
        return Ok(node)
    } else if let Ok(node) = parse_reference(parser) {
        return Ok(node)
    }  else {
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
    Ok(Box::new(ASTNode::new(ASTNodeType::Operator(token), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn parse_operator_2nd(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let token = is_next_any!(parser.tokenizer, fallback,
        Token::Add,
        Token::Subtract,
    )?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Operator(token), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
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
    Ok(Box::new(ASTNode::new(ASTNodeType::Operator(token), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn parse_operator_4th(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let token = is_next_any!(parser.tokenizer, fallback, 
        Token::And,
        Token::Or,
        Token::Xor,
    )?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Operator(token), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn parse_operator_5th(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let token = is_next_any!(parser.tokenizer, fallback, 
        Token::If,
    )?;
    Ok(Box::new(ASTNode::new(ASTNodeType::Operator(token), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}



fn parse_expression(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let found_negative = is_next_continue!(parser.tokenizer, Token::Subtract).is_ok();
    let mut lhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_4th)?;

    if found_negative {
        let operator = Box::new(ASTNode::new(ASTNodeType::Operator(Token::Subtract), vec![
            Box::new(ASTNode::new(ASTNodeType::Number(0.0), vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))),
            lhs
        ], Annotation::pos_to_debug(parser.tokenizer.pos())));
        lhs = operator;
    }

    while let Ok(mut operator) = parse_operator_5th(parser) {
        let rhs = match operator.node_type {
            ASTNodeType::Operator(Token::If) => {
                get_or_fallback!(parser.tokenizer, fallback, parse_tuple)?
            },
            _ => {
                get_or_fallback!(parser.tokenizer, fallback, parse_expression_4th)?
            }
        };

        operator.children.push(lhs);
        operator.children.push(rhs);
        lhs = operator;
    }

    if is_next_continue!(parser.tokenizer, Token::Return).is_ok() {
        lhs.annotations.push(Annotation::Return);
    }

    Ok(lhs)
}

fn parse_expression_4th(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut lhs = get_or_fallback!(parser.tokenizer, fallback, parse_expression_3rd)?;

    while let Ok(mut operator) = parse_operator_4th(parser) {
        let rhs = match operator.node_type {
            ASTNodeType::Operator(Token::If) => {
                get_or_fallback!(parser.tokenizer, fallback, parse_tuple)?
            },
            _ => {
                get_or_fallback!(parser.tokenizer, fallback, parse_expression_3rd)?
            }
        };

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
    setter.children.push(Box::new(ASTNode::new(ASTNodeType::Reference(identifier),  vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))));
    let expression: Box<ASTNode> = or!(parser.tokenizer, "expected meta, expression or tuple", fallback, parse_expression)?;
    setter.children.push(expression);

    Ok(setter)
}

fn parse_strict_definition(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut setter = Box::new(ASTNode::new(ASTNodeType::Set(Token::Set), vec![], Annotation::pos_to_debug(parser.tokenizer.pos())));
    let identifier = is_next!(parser.tokenizer, fallback, Token::Identifier(_ident))?;
    setter.children.push(Box::new(ASTNode::new(ASTNodeType::Reference(identifier),  vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))));
    is_next!(parser.tokenizer, fallback, Token::Set)?;
    let expression: Box<ASTNode> = or!(parser.tokenizer, "expected meta, expression or tuple", fallback, parse_expression)?;
    setter.children.push(expression);

    Ok(setter)
}

fn parse_expression_list(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    
    is_next!(parser.tokenizer, fallback, Token::OpenBracket)?;

    let mut size = 0;
    let mut children = vec![];
    if let Ok(node) = or_continue!(parser.tokenizer, "expected an expression or definition", parse_definition, parse_expression) {
        children.push(node);
        size += 1;
    } else if is_next_continue!(parser.tokenizer, Token::CloseBracket).is_ok() {
        return Ok(Box::new(ASTNode::new( ASTNodeType::ExpressionList(size), children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
    } else {
        return create_parse_error!(parser.tokenizer, "expected an expression or definition")
    }

    if is_next_continue!(parser.tokenizer, Token::CloseBracket).is_ok() {
        return Ok(Box::new(ASTNode::new( ASTNodeType::ExpressionList(size), children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
    } else if is_next_continue!(parser.tokenizer, Token::SeparateAndPush).is_ok() {
        children.push(Box::new(ASTNode::new(ASTNodeType::Push, vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
    } else if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
        children.push(Box::new(ASTNode::new(ASTNodeType::Throw, vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
    } else {
        let err = create_token_parse_error!(parser.tokenizer, Token::CloseBracket, Token::SeparateAndPush, Token::Separate);
        parser.tokenizer.seek(fallback);
        return err 
    }

    loop {
        if is_next_continue!(parser.tokenizer, Token::CloseBracket).is_ok() {
            return Ok(Box::new(ASTNode::new( ASTNodeType::ExpressionList(size), children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
        }

        children.push(or!(parser.tokenizer, "expected an expression or definition", fallback, parse_definition, parse_expression)?);
        size += 1;

        if is_next_continue!(parser.tokenizer, Token::SeparateAndPush).is_ok() {
            children.push(Box::new(ASTNode::new(ASTNodeType::Push, vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
        } else if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
            children.push(Box::new(ASTNode::new(ASTNodeType::Throw, vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
        } else if is_next_continue!(parser.tokenizer, Token::CloseBracket).is_ok() {
            return Ok(Box::new(ASTNode::new( ASTNodeType::ExpressionList(size), children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
        } else {
            let err = create_token_parse_error!(parser.tokenizer, Token::CloseBracket, Token::SeparateAndPush, Token::Separate);
            parser.tokenizer.seek(fallback);
            return err
        }
    }
    
}


fn parse_tail_exec(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let children = vec![parse_exec(parser)?];
    is_next!(parser.tokenizer, fallback, Token::Return)?;

    Ok(Box::new(ASTNode::new(ASTNodeType::TailExec,  children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn parse_exec(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let mut res = create_parse_error!(parser.tokenizer, "expected expression list or tuple") ;
    if let Ok(tuple) =  get_or_fallback!(parser.tokenizer, fallback, parse_tuple) {
        if is_next_continue!(parser.tokenizer, Token::Exec).is_ok() {
            if let Ok(expression_list) = get_or_fallback!(parser.tokenizer, fallback, parse_expression_list) {
                res = Ok(Box::new(ASTNode::new(ASTNodeType::Exec, vec![tuple, expression_list], Annotation::pos_to_debug(parser.tokenizer.pos()))))
            }
        }
    }
    if res.is_ok() {
        return res
    }
    if let Ok(tuple) = get_or_fallback!(parser.tokenizer, fallback, parse_value_tuple) {
        if is_next_continue!(parser.tokenizer, Token::Exec).is_ok() {
            let ident = get_next!(parser.tokenizer, fallback, Token::Identifier(_ident))?;
            res = Ok(Box::new(ASTNode::new(ASTNodeType::Exec, vec![
                tuple,
                Box::new(ASTNode::new(ASTNodeType::Reference(Token::Identifier(ident)), vec![], Annotation::pos_to_debug(parser.tokenizer.pos())))    
            ], Annotation::pos_to_debug(parser.tokenizer.pos()))))
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
    let data = or!(parser.tokenizer, "expected a tuple or reference", fallback, parse_matrix)?;
    is_next!(parser.tokenizer, fallback, Token::Into)?;
    let exec =get_or_fallback!(parser.tokenizer, fallback, parse_exec)?;

    Ok(Box::new(ASTNode::new(ASTNodeType::Into,  vec![data, exec], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn  parse_for_each(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let data = or!(parser.tokenizer, "expected a range, tuple or reference", fallback, parse_matrix, parse_range)?;
    is_next!(parser.tokenizer, fallback, Token::ForEach)?;
    let exec =get_or_fallback!(parser.tokenizer, fallback, parse_exec)?;

    Ok(Box::new(ASTNode::new(ASTNodeType::ForEach,  vec![data, exec], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn  parse_reduce(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let data = or!(parser.tokenizer, "expected a range, tuple or reference", fallback, parse_matrix, parse_range)?;
    is_next!(parser.tokenizer, fallback, Token::Reduce)?;
    let exec =get_or_fallback!(parser.tokenizer, fallback, parse_exec)?;

    Ok(Box::new(ASTNode::new(ASTNodeType::Reduce,  vec![data, exec], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}


fn  parse_tuple(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    
    is_next!(parser.tokenizer, fallback, Token::OpenParentheses)?;

    let mut children = vec![];
    let mut size = 0;
    if let Ok(node) = or_continue!(parser.tokenizer, "expected an expression or strict definition", parse_strict_definition, parse_expression) {
        children.push(node);
        size += 1;
    } else if is_next_continue!(parser.tokenizer, Token::CloseParentheses).is_ok() {
        return Ok(Box::new(ASTNode::new( ASTNodeType::Tuple(size), children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
    } else {
        return create_parse_error!(parser.tokenizer, "expected an expression or strict definition");
    }

    loop {
        if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
            children.push(Box::new(ASTNode::new(ASTNodeType::Throw, vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
        } else if is_next_continue!(parser.tokenizer, Token::CloseParentheses).is_ok() {
            return Ok(Box::new(ASTNode::new( ASTNodeType::Tuple(size), children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
        } else {
            let err = create_token_parse_error!(parser.tokenizer, Token::Separate, Token::CloseParentheses);
            parser.tokenizer.seek(fallback);
            return err
        }
        
        size += 1;
        children.push(or!(parser.tokenizer, "expected an expression or strict definition", fallback, parse_strict_definition, parse_expression)?);
    }
}


fn  parse_value_tuple(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    
    is_next!(parser.tokenizer, fallback, Token::OpenParentheses)?;

    let mut children = vec![];
    let mut size = 0;
    if let Ok(node) = or_continue!(parser.tokenizer, "expected an expression or strict definition", parse_expression) {
        children.push(node);
        size += 1;
    } else if is_next_continue!(parser.tokenizer, Token::CloseParentheses).is_ok() {
        return Ok(Box::new(ASTNode::new( ASTNodeType::Tuple(size), children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
    } else {
        return create_parse_error!(parser.tokenizer, "expected an expression or strict definition");
    }

    loop {
        if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
            children.push(Box::new(ASTNode::new(ASTNodeType::Throw, vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
        } else if is_next_continue!(parser.tokenizer, Token::CloseParentheses).is_ok() {
            return Ok(Box::new(ASTNode::new( ASTNodeType::Tuple(size), children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
        } else {
            let err = create_token_parse_error!(parser.tokenizer, Token::Separate, Token::CloseParentheses);
            parser.tokenizer.seek(fallback);
            return err
        }
        
        size += 1;
        children.push(or!(parser.tokenizer, "expected an expression or strict definition", fallback, parse_expression)?);
    }
}

fn parse_indexed_value(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let reference = or!(parser.tokenizer, "expected a matrix or reference", fallback, parse_matrix)?;
    let meta = get_or_fallback!(parser.tokenizer, fallback, parse_index)?;

    Ok(Box::new(ASTNode::new(ASTNodeType::Indexed,  vec![reference, meta], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn parse_matrix(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();

    is_next!(parser.tokenizer, fallback, Token::Matrix)?;

    Ok(Box::new(ASTNode::new(ASTNodeType::Matrix, vec![], Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn parse_range(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();

    is_next!(parser.tokenizer, fallback, Token::OpenBrace)?;

    let mut children = vec![];
    
    children.push(parse_expression(parser)?);
    
    is_next!(parser.tokenizer, fallback, Token::Separate)?;
    
    children.push(parse_expression(parser)?);

    let ast_type = if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
        children.push(parse_expression(parser)?);
        ASTNodeType::RangeComplex
    } else {
        ASTNodeType::Range
    };

    is_next!(parser.tokenizer, fallback, Token::CloseBrace)?;

    Ok(Box::new(ASTNode::new(ast_type, children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn parse_index(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();

    is_next!(parser.tokenizer, fallback, Token::OpenParentheses)?;

    let mut children = vec![];
    
    children.push(parse_expression(parser)?);

    let ast_type = if is_next_continue!(parser.tokenizer, Token::Separate).is_ok() {
        children.push(parse_expression(parser)?);
        ASTNodeType::Index2D
    } else {
        ASTNodeType::Index
    };

    is_next!(parser.tokenizer, fallback, Token::CloseParentheses)?;

    Ok(Box::new(ASTNode::new(ast_type, children, Annotation::pos_to_debug(parser.tokenizer.pos()))))
}

fn parse_statement(parser: &mut Parser) -> Result<Box<ASTNode>, ParseError> {
    let fallback = parser.tokenizer.pos();
    let statement_type;
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
    
    Ok(Box::new(ASTNode::new( ASTNodeType::Statement(statement_type),  children , Annotation::pos_to_debug(parser.tokenizer.pos()))))
}


#[cfg(test)]
mod tests {
    use crate::{lexer::Tokenizer, parser::AbstractSyntaxTree};
    
    #[test]
    fn test_arithmetic() {
        assert_parse_eq!(
            r"2+2*3-1/2;",
            "(((((Number(2.0))((Number(2.0))(Number(3.0))Operator(Multiply))Operator(Add))((Number(1.0))(Number(2.0))Operator(Divide))Operator(Subtract))Statement(Throw))Root)"
        );
    }

    #[test]
    fn test_arithmetic_if() {
        assert_parse_eq!(
            r"2*2 ? (2; 2);",
            "(((((Number(2.0))(Number(2.0))Operator(Multiply))((Number(2.0))(Throw)(Number(2.0))Tuple(2))Operator(If))Statement(Throw))Root)"
        );
    }

    #[test]
    fn test_comparator() {
        assert_parse_eq!(
            r"1+2<3;", 
            "(((((Number(1.0))(Number(2.0))Operator(Add))(Number(3.0))Operator(LessThan))Statement(Throw))Root)"
        );
    }

    #[test]
    fn test_logic() {
        assert_parse_eq!(
            r"2*3and10*2;", 
            "(((((Number(2.0))(Number(3.0))Operator(Multiply))((Number(10.0))(Number(2.0))Operator(Multiply))Operator(And))Statement(Throw))Root)"
        );
    }

    #[test]
    fn test_refs() {
        assert_parse_eq!(
            r"
                var*2;
                2*@;
                @@*@;
            ", 
            "((((Reference(Identifier(\"var\")))(Number(2.0))Operator(Multiply))Statement(Throw))(((Number(2.0))(Reference(Pop))Operator(Multiply))Statement(Throw))(((Reference(Peek))(Reference(Pop))Operator(Multiply))Statement(Throw))Root)"
        );
    }

    #[test]
    fn test_values() {
        assert_parse_eq!(
            r"
                10;
                !10;
                true;
                false;
            ", 
            "(((Number(10.0))Statement(Throw))(((Number(10.0))Operator(Not))Statement(Throw))((Number(1.0))Statement(Throw))((Number(0.0))Statement(Throw))Root)"
        );
    }

    #[test]
    fn test_call() {
        assert_parse_eq!(r"sin|10|;", "((((Number(10.0))Call(\"sin\"))Statement(Throw))Root)");
        assert_parse_eq!(r"pow|10;2|;", "((((Number(10.0))(Number(2.0))Call(\"pow\"))Statement(Throw))Root)");
    }

    #[test]
    fn test_range() {
        assert_parse_eq!(r"{10;20;2}<*[];", "(((((Number(10.0))(Number(20.0))(Number(2.0))RangeComplex)(ExpressionList(0))ForEach)Statement(Throw))Root)");
        assert_parse_eq!(r"{10;20}<*[];", "(((((Number(10.0))(Number(20.0))Range)(ExpressionList(0))ForEach)Statement(Throw))Root)");
    }

    #[test]
    fn test_func() {
        assert_parse_eq!(r"()->[];", "((((Tuple(0))(ExpressionList(0))Exec)Statement(Throw))Root)");

        assert_parse_eq!(r"()->[]!;", "(((((Tuple(0))(ExpressionList(0))Exec)TailExec)Statement(Throw))Root)");

        assert_parse_eq!(r"{2;3}*>[];", "(((((Number(2.0))(Number(3.0))Range)(ExpressionList(0))Reduce)Statement(Throw))Root)");

        assert_parse_eq!(r"#<-[];", "((((Matrix)(ExpressionList(0))Into)Statement(Throw))Root)");

        assert_parse_eq!(r"{2;3}<*[];", "(((((Number(2.0))(Number(3.0))Range)(ExpressionList(0))ForEach)Statement(Throw))Root)");
    }

    #[test]
    fn test_index() {
        assert_parse_eq!(r"#(10;20);", "((((Matrix)((Number(10.0))(Number(20.0))Index2D)Indexed)Statement(Throw))Root)");
    }

    #[test]
    fn test_expression_list() {
        assert_parse_eq!("[];", "(((ExpressionList(0))Statement(Throw))Root)");
        assert_parse_eq!("[1,2];", "((((Number(1.0))(Push)(Number(2.0))ExpressionList(2))Statement(Throw))Root)");
        assert_parse_eq!("[1;2];", "((((Number(1.0))(Throw)(Number(2.0))ExpressionList(2))Statement(Throw))Root)");
        assert_parse_eq!("[1+2];", "(((((Number(1.0))(Number(2.0))Operator(Add))ExpressionList(1))Statement(Throw))Root)");
        assert_parse_eq!("[var:10];", "(((((Reference(Identifier(\"var\")))(Number(10.0))Set(Set))ExpressionList(1))Statement(Throw))Root)");
    }

    #[test]
    fn test_definition() {
        assert_parse_eq!("var:10*10;", "((((Reference(Identifier(\"var\")))((Number(10.0))(Number(10.0))Operator(Multiply))Set(Set))Statement(Throw))Root)");
        assert_parse_eq!("var+:10*10;", "((((Reference(Identifier(\"var\")))((Number(10.0))(Number(10.0))Operator(Multiply))Set(AddAndSet))Statement(Throw))Root)");
    }

    #[test]
    fn test_matrix() {
        assert_parse_eq!("# <- [1];", "((((Matrix)((Number(1.0))ExpressionList(1))Into)Statement(Throw))Root)");
        assert_parse_eq!("#(10;10);", "((((Matrix)((Number(10.0))(Number(10.0))Index2D)Indexed)Statement(Throw))Root)");
    }
}
