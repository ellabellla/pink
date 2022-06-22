use wasm_bindgen::prelude::*;
mod tokenizer;
mod charstream;
mod token_lookup;
use crate::tokenizer::Tokenizer;

#[wasm_bindgen]
extern {

}

#[wasm_bindgen]
pub fn compile(input: &str) {
    let mut tokenizer = Tokenizer::new(input);
    tokenizer.next();
}