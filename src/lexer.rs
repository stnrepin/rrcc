use std::clone::Clone;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tag {
    None,
    Eof,
    // Identifier,
    // Keyword,
    Integer(Radix),
    // Float,
    // Str,
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    Asterisk,
    Slash,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexdecimal = 16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub tag: Tag,
    pub value: Option<Vec<char>>,
}

impl Token {
    fn new(tag: Tag, value: &[char]) -> Token {
        Token {
            tag,
            value: Some(value.to_vec()),
        }
    }

    fn new_emp(tag: Tag) -> Token {
        Token { tag, value: None }
    }

    fn empty() -> Token {
        Self::new_emp(Tag::None)
    }

    #[allow(dead_code)]
    fn new_str(tag: Tag, value: &str) -> Token {
        let chars: Vec<char> = value.chars().collect();
        Self::new(tag, &chars)
    }

    fn new_eof() -> Token {
        Self::new_emp(Tag::Eof)
    }
}

pub struct Lexer {
    source_text: Vec<char>,
    cur_pos: usize,
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new_s<S: AsRef<str>>(src: S) -> Lexer {
        Lexer {
            source_text: src.as_ref().chars().collect(),
            cur_pos: 0,
            tokens: Vec::new(),
        }
    }

    pub fn run(&mut self) {
        self.run_parse();
    }

    fn can_next(&self, off: usize) -> bool {
        self.cur_pos + off < self.source_text.len()
    }

    fn take(&mut self, n: usize) {
        self.cur_pos += n;
    }

    fn peek(&self, off: usize) -> Option<char> {
        if self.can_next(off) {
            Some(self.source_text[self.cur_pos + off])
        } else {
            None
        }
    }

    fn peek_while<P>(&self, pred: P) -> usize
    where
        P: Fn(char) -> bool,
    {
        let mut i = 0usize;
        while self.peek(i).map_or(false, |c| pred(c)) {
            i += 1;
        }
        i
    }

    fn add_eof_tok(&mut self) {
        self.tokens.push(Token::new_eof());
    }

    fn run_parse(&mut self) {
        while self.can_next(0) {
            let tok = None // NOFORMAT
                .or_else(|| self.skip_whitespaces())
                .or_else(|| self.skip_whitespaces())
                .or_else(|| self.scan_number())
                .or_else(|| self.scan_seq("++", |_| Token::new_emp(Tag::PlusPlus)))
                .or_else(|| self.scan_seq("+", |_| Token::new_emp(Tag::Plus)))
                .or_else(|| self.scan_seq("--", |_| Token::new_emp(Tag::MinusMinus)))
                .or_else(|| self.scan_seq("-", |_| Token::new_emp(Tag::Minus)))
                .or_else(|| self.scan_seq("*", |_| Token::new_emp(Tag::Asterisk)))
                .or_else(|| self.scan_seq("/", |_| Token::new_emp(Tag::Slash)));

            match tok {
                Some(t) => self.tokens.push(t),
                None => break,
            }
        }
        if self.can_next(0) {
            panic!("Unexpected token at {}", self.cur_pos + 1);
        } else {
            self.add_eof_tok();
        }
    }

    fn skip_whitespaces(&mut self) -> Option<Token> {
        let len = self.peek_while(|c| c.is_whitespace());
        self.take(len);
        None
    }

    fn scan_number(&mut self) -> Option<Token> {
        let mut radix = Radix::Decimal;

        None // NOFORMAT
            .or_else(|| {
                self.scan_seq("0b", |_| {
                    radix = Radix::Binary;
                    Token::empty()
                })
            })
            .or_else(|| {
                self.scan_seq("0x", |_| {
                    radix = Radix::Hexdecimal;
                    Token::empty()
                })
            })
            .or_else(|| {
                self.scan_seq("0", |_| {
                    radix = Radix::Octal;
                    Token::empty()
                })
            });

        self.scan_pred(
            |c| {
                let is_numeric = c.is_ascii_digit();
                let is_digit_in_radix = c.is_digit(radix as u32);
                if is_numeric && !is_digit_in_radix {
                    panic!("Invalid digit '{}' in {}-radix system", c, radix as u32);
                }
                is_digit_in_radix
            },
            |s| Token::new(Tag::Integer(radix), s),
        )
    }

    fn scan_pred<P, F>(&mut self, pred: P, fact: F) -> Option<Token>
    where
        P: Fn(char) -> bool,
        F: Fn(&[char]) -> Token,
    {
        let mut i = 0usize;
        while self.peek(i).map_or(false, |c| pred(c)) {
            i += 1;
        }
        let pos = self.cur_pos;
        if i > 0 {
            self.take(i);
            Some(fact(&self.source_text[pos..(pos + i)]))
        } else {
            None
        }
    }

    fn peek_seq(&mut self, seq: &str) -> Option<usize> {
        if !self.can_next(seq.len()) {
            return None;
        }
        let mut i = 0usize;
        for s in seq.chars() {
            if self.peek(i).map_or(false, |c| c != s) {
                return None;
            }
            i += 1;
        }

        return Some(i);
    }

    fn scan_seq<F>(&mut self, seq: &str, mut fact: F) -> Option<Token>
    where
        F: FnMut(&str) -> Token,
    {
        let len = match self.peek_seq(seq) {
            Some(len) => len,
            None => return None,
        };
        self.take(len);
        Some(fact(seq))
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Radix, Tag, Token};
    use std::panic;

    fn run_lexer(s: &str) -> Vec<Token> {
        let mut l = Lexer::new_s(s);
        l.run();
        l.tokens.to_vec()
    }

    fn add_eof(mut toks: Vec<Token>) -> Vec<Token> {
        toks.push(Token::new_eof());
        toks
    }

    #[test]
    fn test_lex_eof() {
        let toks = run_lexer("  ");

        let exps = vec![Token::new_eof()];

        assert_eq!(toks, exps);
    }

    fn tok_int10(s: &str) -> Token {
        Token::new_str(Tag::Integer(Radix::Decimal), s)
    }

    #[test]
    fn test_lex_only_integer() {
        let toks = run_lexer("  123 ");
        let exps = add_eof(vec![tok_int10("123")]);
        assert_eq!(toks, exps);

        let toks = run_lexer("0x12a4");
        let exps = add_eof(vec![Token::new_str(
            Tag::Integer(Radix::Hexdecimal),
            "12a4",
        )]);
        assert_eq!(toks, exps);

        let toks = run_lexer("0b010");
        let exps = add_eof(vec![Token::new_str(Tag::Integer(Radix::Binary), "010")]);
        assert_eq!(toks, exps);
    }

    #[test]
    fn test_lex_integers_with_bad_digits() {
        let bin_bad = panic::catch_unwind(|| run_lexer("0b123"));
        assert!(bin_bad.is_err());

        let oct_bad = panic::catch_unwind(|| run_lexer("012h"));
        assert!(oct_bad.is_err());

        let dec_bad = panic::catch_unwind(|| run_lexer("123a"));
        assert!(dec_bad.is_err());

        let hex_bad = panic::catch_unwind(|| run_lexer("0xbag"));
        assert!(hex_bad.is_err());
    }

    #[test]
    fn test_lex_integers_and_arith_ops() {
        let toks = run_lexer("2 + 2*2");

        let exps = add_eof(vec![
            tok_int10("2"),
            Token::new_emp(Tag::Plus),
            tok_int10("2"),
            Token::new_emp(Tag::Asterisk),
            tok_int10("2"),
        ]);

        assert_eq!(toks, exps);
    }

    #[test]
    fn test_lex_integer_inc() {
        let toks = run_lexer("-2+++--1 ");

        let exps = add_eof(vec![
            Token::new_emp(Tag::Minus),
            tok_int10("2"),
            Token::new_emp(Tag::PlusPlus),
            Token::new_emp(Tag::Plus),
            Token::new_emp(Tag::MinusMinus),
            tok_int10("1"),
        ]);

        assert_eq!(toks, exps);
    }

    #[test]
    fn test_lex_ast_with_slash() {
        let toks = run_lexer("10*/11");

        let exps = add_eof(vec![
            tok_int10("10"),
            Token::new_emp(Tag::Asterisk),
            Token::new_emp(Tag::Slash),
            tok_int10("11"),
        ]);

        assert_eq!(toks, exps);
    }
}
