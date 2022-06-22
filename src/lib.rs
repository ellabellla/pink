use wasm_bindgen::prelude::*;
mod lexer;
use crate::lexer::tokenizer::Tokenizer;

#[wasm_bindgen]
extern {

}

#[wasm_bindgen]
pub fn compile(input: &str) {
    let mut tokenizer = Tokenizer::new(input);
    tokenizer.next();
}