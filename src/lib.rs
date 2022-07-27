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

#[wasm_bindgen(raw_module = "../www/js/extern.js")]
extern "C" {
    fn println(s: &str);
}

#[wasm_bindgen]
pub struct Compiled {
    asm: String,
    pretty_asm: String,
    start: usize,
    globals: usize,
}

#[wasm_bindgen]
impl Compiled {
    #[wasm_bindgen(constructor)]
    pub fn new(asm: String, pretty_asm: String, start: usize, globals: usize) -> Compiled {
        Compiled { asm, pretty_asm, start, globals }
    }

    #[wasm_bindgen(getter)]
    pub fn asm(&self) -> String {
        self.asm.to_string()
    }

    #[wasm_bindgen(getter)]
    pub fn pretty_asm(&self) -> String {
        self.pretty_asm.to_string()
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
    let pretty_asm = Code::from_instr_to_string(&instrs, true);
    let asm = Code::from_instr_to_string(&instrs, false);

    Ok(Compiled{globals, start, asm, pretty_asm})
}

#[wasm_bindgen]
pub fn run(program: Compiled, width: usize, height: usize) -> Result<Matrix, String> {
    let instrs = Code::from_string_to_instr(&program.asm).map_err(|err| err.to_string())?;
    let mut vm = VM::new((width, height), program.globals, 100, instrs, program.start, Box::new(Printer{}));
    vm.run().map_err(|err| err.to_string())?;
    let matrix = *vm.get_matrix();

    Ok(matrix)
}

struct Printer {
}

impl ExternPrintLn for Printer {
    fn println_num(&self, msg: f64) {
        println(&format!("{}", msg));
    }

    fn println_str(&self, msg: &str) {
        println(msg);
    }
}