use wasm_bindgen::prelude::*;
mod lexer;
mod vm;
use crate::lexer::*;
use crate::vm::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn compile(input: &str) {
    let mut tokenizer = Tokenizer::new(input);
    
    while let Some(token) = tokenizer.next() {
        log(format!("{:?}", token).as_str());
    }
}