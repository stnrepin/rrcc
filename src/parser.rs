#![allow(dead_code)]

use crate::lexer::{Lexer, Radix, Tag, Token};

// Based on <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntegerType {
    Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerExpr {
    value: Vec<u8>,
    typpe: IntegerType,
}

impl IntegerExpr {
    fn parse(ctx: &ParserContext) -> Box<IntegerExpr> {
        let tok = ctx.peek();
        let radix = match tok.tag {
            Tag::Integer(r) => r,
            _ => panic!("Unexpected integer token {:?}", tok.tag),
        };
        let value = Self::string_to_int_bytes(&tok.value.as_ref().unwrap(), radix);
        let expr = IntegerExpr {
            value,
            typpe: IntegerType::Int,
        };
        Box::new(expr)
    }

    fn string_to_int_bytes(s: &Vec<char>, radix: Radix) -> Vec<u8> {
        s.iter()
            .map(|ch| {
                ch.to_digit(radix as u32)
                    .unwrap_or_else(|| panic!("Incorrect number digit at '{:?}'", s))
                    as u8
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimaryExpr {
    Integer(Box<IntegerExpr>),
}

impl PrimaryExpr {
    fn parse(ctx: &ParserContext) -> PrimaryExpr {
        match ctx.peek_tag() {
            Tag::Integer(_) => PrimaryExpr::Integer(IntegerExpr::parse(&ctx)),
            _ => panic!("unexpected token"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operation {
    Positive,
    Negative,
    Sum,
    Difference,
    Multiplication,
    Division,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpr {
    op: Operation,
    rhs: Expr,
}

impl UnaryExpr {
    fn parse(ctx: &ParserContext) -> UnaryExpr {
        UnaryExpr {
            op: Operation::Positive,
            rhs: Expr::parse(&ctx),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    op: Operation,
    lhs: Expr,
    rhs: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Primary(PrimaryExpr),
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
}

impl Expr {
    fn parse(ctx: &ParserContext) -> Expr {
        Expr::Primary(PrimaryExpr::parse(&ctx))
    }
}

struct ParserContext<'a> {
    toks: &'a [Token],
}

impl<'a> ParserContext<'_> {
    fn new(lexer: &'a Lexer) -> ParserContext {
        ParserContext {
            toks: lexer.tokens.as_slice(),
        }
    }

    fn peek(&self) -> &Token {
        self.toks.first().expect("Unexpected end of file")
    }

    fn peek_exp_pred<P>(&self, pred: P) -> &Token
    where
        P: Fn(Tag) -> bool,
    {
        let tok = self.peek();
        if pred(tok.tag) {
            panic!("Unexpected token {:?}", tok.tag);
        }
        tok
    }

    fn peek_exp(&self, tag: Tag) -> &Token {
        self.peek_exp_pred(|t| t == tag)
    }

    fn peek_tag(&self) -> Tag {
        self.peek().tag
    }

    fn consume(&mut self) {
        let _ = self.toks.take(..1);
    }
}

pub struct Parser<'a> {
    lexer: &'a Lexer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
    expr: Expr,
}

impl Parser<'_> {
    pub fn new<'a>(lexer: &'a Lexer) -> Parser<'a> {
        Parser { lexer }
    }

    pub fn run(&self) -> Ast {
        let ctx = ParserContext::new(&self.lexer);
        Ast {
            expr: Expr::parse(&ctx),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use std::panic;

    fn run_parser(s: &str) -> Ast {
        let mut l = Lexer::new_s(s);
        l.run();
        Parser::new(&l).run()
    }

    #[test]
    fn test_primary_expr_integer_parsing() {
        fn unwrap_ast(a: &Ast) -> Vec<u8> {
            match a.expr.clone() {
                Expr::Primary(prim_expr) => match prim_expr {
                    PrimaryExpr::Integer(int) => int.value,
                },
                _ => panic!(),
            }
        }

        let bin = unwrap_ast(&run_parser("0b010"));
        assert_eq!(bin, &[0, 1, 0]);

        let oct = unwrap_ast(&run_parser("01273"));
        assert_eq!(oct, &[1, 2, 7, 3]);

        let dec = unwrap_ast(&run_parser("256"));
        assert_eq!(dec, &[2, 5, 6]);

        let hex = unwrap_ast(&run_parser("0x1f3"));
        assert_eq!(hex, &[1, 15, 3]);
    }

    #[test]
    fn test_sum_int() {
        let ast = run_parser("12+2");

        let exp = Ast {
            expr: Expr::Primary(PrimaryExpr::Integer(Box::new(IntegerExpr {
                value: Vec::new(),
                typpe: IntegerType::Int,
            }))),
        };

        assert_eq!(ast, exp);
    }
}
