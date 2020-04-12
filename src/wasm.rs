use crate::error::ErrorHandler;
use crate::opcode;
use crate::parse::Function as ForkFunction;

#[derive(Debug)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
}

pub struct Function {
    pub params: Vec<Type>,
    pub results: Vec<Type>,
    pub type_index: usize, // Used by encode
    pub body: Vec<opcode::Instr>,
    pub export_name: Option<String>,
}

pub struct Compiler {
    error_handler: ErrorHandler,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            error_handler: ErrorHandler::new(),
        }
    }

    pub fn compile(&mut self, funs: Vec<ForkFunction>) -> Vec<Function> {
        let mut wasm_funs = Vec::new();

        for fun in funs.iter() {
            let mut params = Vec::new();
            let results = Vec::new();

            for param in fun.params.iter() {
                params.push(Type::I32)
            }

            let export_name = if fun.exported {
                if fun.ident == "Main" {
                    Some(String::from("_start")) // WASI main function
                } else {
                    if fun.ident == "main" {
                        self.error_handler
                            .report(0, "Main function must be capitalized") // TODO report line
                    }
                    Some(fun.ident.clone())
                }
            } else {
                None
            };

            wasm_funs.push(Function {
                params: params,
                results: results,
                type_index: std::usize::MAX,
                body: Vec::new(),
                export_name: export_name,
            })
        }

        wasm_funs
    }
}
