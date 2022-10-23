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
    fn parse(ctx: &mut ParserContext) -> Box<IntegerExpr> {
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
        ctx.consume();
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
    fn parse(ctx: &mut ParserContext) -> PrimaryExpr {
        match ctx.peek_tag() {
            Tag::Integer(_) => PrimaryExpr::Integer(IntegerExpr::parse(ctx)),
            _ => panic!("unexpected token"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operation {
    None,
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
    rhs: PrimaryExpr,
}

impl UnaryExpr {
    fn parse(ctx: &mut ParserContext) -> Option<Box<UnaryExpr>> {
        let tag = ctx.peek_tag();
        let op: Operation;
        match tag {
            Tag::Integer(_) => {
                return Some(Box::new(UnaryExpr {
                    op: Operation::None,
                    rhs: PrimaryExpr::parse(ctx),
                }))
            }
            Tag::Plus => op = Operation::Positive,
            Tag::Minus => op = Operation::Negative,
            _ => return None,
        };
        ctx.consume();
        match UnaryExpr::parse(ctx) {
            Some(mut e) => {
                e.op = op;
                Some(e)
            }
            None => panic!(
                "unexpected token '{:?}' for unary expression",
                ctx.peek_tag()
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    op: Operation,
    lhs: Expr,
    rhs: Expr,
}

impl BinaryExpr {
    fn parse(ctx: &mut ParserContext, lhs: Expr) -> Box<BinaryExpr> {
        let op = match ctx.peek_tag() {
            Tag::Plus => Operation::Sum,
            Tag::Minus => Operation::Division,
            Tag::Asterisk => Operation::Multiplication,
            Tag::Slash => Operation::Division,
            _ => panic!(
                "unexpected operation token '{:?}' in binary expression",
                ctx.peek_tag()
            ),
        };

        ctx.consume();
        let rhs = Expr::parse(ctx);

        Box::new(BinaryExpr { op, lhs, rhs })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
}

impl Expr {
    fn parse(ctx: &mut ParserContext) -> Expr {
        let lhs = match UnaryExpr::parse(ctx) {
            Some(ue) => Expr::Unary(ue),
            None => panic!("unexpected token '{:?}' in expression", ctx.peek_tag()),
        };

        if ctx.peek_tag() == Tag::Eof {
            return lhs;
        }

        Expr::Binary(BinaryExpr::parse(ctx, lhs))
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
        if !pred(tok.tag) {
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
        let mut ctx = ParserContext::new(&self.lexer);
        let ast = Ast {
            expr: Expr::parse(&mut ctx),
        };
        ctx.peek_exp(Tag::Eof);
        ast
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
    fn test_primary_expr_with_int() {
        fn unwrap_ast(a: &Ast) -> Vec<u8> {
            match a.expr.clone() {
                Expr::Unary(ue) => match ue.rhs {
                    PrimaryExpr::Integer(int) => {
                        assert_eq!(ue.op, Operation::None);
                        int.value
                    }
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
    fn test_unary_minus_int() {
        let ast = run_parser("-23");
        let exp = Ast {
            expr: Expr::Unary(Box::new(UnaryExpr {
                op: Operation::Negative,
                rhs: PrimaryExpr::Integer(Box::new(IntegerExpr {
                    value: vec![2, 3],
                    typpe: IntegerType::Int,
                })),
            })),
        };
        assert_eq!(ast, exp);
    }

    #[test]
    fn test_sum_int() {
        let ast = run_parser("12+-2");

        let lhs = run_parser("12").expr;
        let rhs = run_parser("-2").expr;

        let exp = Ast {
            expr: Expr::Binary(Box::new(BinaryExpr {
                op: Operation::Sum,
                lhs,
                rhs,
            })),
        };

        assert_eq!(ast, exp);
    }

    #[test]
    fn teset_sum_and_mul() {
        let ast = run_parser("2 + 3*4");

        let t1 = run_parser("2").expr;
        let t2 = run_parser("3").expr;
        let t3 = run_parser("4").expr;

        let exp = Ast {
            expr: Expr::Binary(Box::new(BinaryExpr {
                op: Operation::Sum,
                lhs: t1,
                rhs: Expr::Binary(Box::new(BinaryExpr {
                    op: Operation::Multiplication,
                    lhs: t2,
                    rhs: t3,
                })),
            })),
        };

        assert_eq!(ast, exp);
    }
}
