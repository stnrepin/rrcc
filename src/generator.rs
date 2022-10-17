use crate::parser::Ast;

pub struct Asm;

impl Asm {
    pub fn to_string(self) -> String {
        "".to_string()
    }
}

pub fn gen(_ast: &Ast) -> Asm {
    Asm {}
}
