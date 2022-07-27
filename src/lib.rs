use wasm_bindgen::prelude::*;
mod lexer;
mod vm;
mod parser;
mod semantic;
mod code_gen;
use crate::lexer::*;
use crate::vm::*;
use crate::parser::*;
use crate::semantic::*;
use crate::code_gen::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    fn alert(s: &str);
}

#[wasm_bindgen]
pub struct Compiled {
    asm: String,
    start: usize,
    globals: usize,
}

#[wasm_bindgen]
impl Compiled {
    #[wasm_bindgen(getter)]
    pub fn asm(&self) -> String {
        self.asm.to_string()
    }

    #[wasm_bindgen(getter)]
    pub fn start(&self) -> usize {
        self.start
    }

    #[wasm_bindgen(getter)]
    pub fn globals(&self) -> usize {
        self.globals
    }
}

#[wasm_bindgen]
pub fn compile(input: &str) -> Result<Compiled, String> {
    let mut tokenizer = Tokenizer::new(input);

    let mut tree = AbstractSyntaxTree::new(&mut tokenizer).map_err(|err| err.to_string())?;
    validate(&mut tree).map_err(|err| err.to_string())?;
    let code = Code::new(&mut tree).map_err(|err| err.to_string())?;

    let (globals, start, instrs) = code.to_instrs();
    let asm = Code::from_instr_to_string((globals, start, instrs), false);

    Ok(Compiled{globals, start, asm})
}

#[wasm_bindgen]
pub fn run(program: Compiled) -> Result<Matrix, String> {
    let instrs = Code::from_string_to_instr(&program.asm).map_err(|err| err.to_string())?;
    let mut vm = VM::new((500, 500), program.globals, 100, instrs, program.start);
    vm.run().map_err(|err| err.to_string())?;
    let matrix = *vm.get_matrix();

    Ok(matrix)
}