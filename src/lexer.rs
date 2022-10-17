use std::clone::Clone;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tag {
    // None,
    Eof,
    // Identifier,
    // Keyword,
    Integer,
    // Float,
    // Str,
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    Asterisk,
    Slash,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    tag: Tag,
    value: Option<Vec<char>>,
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
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(src: String) -> Lexer {
        Lexer {
            source_text: src.chars().collect(),
            cur_pos: 0,
            tokens: Vec::new(),
        }
    }

    pub fn run(&mut self) -> Vec<Token> {
        self.run_parse();
        self.tokens.clone()
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

    fn add_tok_and_take(&mut self, tok: Token, len: usize) {
        self.tokens.push(tok);
        self.take(len);
    }

    fn add_eof_tok(&mut self) {
        self.tokens.push(Token::new_eof());
    }

    fn run_parse(&mut self) {
        while self.can_next(0) {
            let tokenized = false // NOFORMAT
                || self.skip_whitespaces()
                || self.scan_pred(|c| c.is_numeric(), |num| Token::new(Tag::Integer, num))
                || self.scan_seq("++", |_| Token::new_emp(Tag::PlusPlus))
                || self.scan_seq("+", |_| Token::new_emp(Tag::Plus))
                || self.scan_seq("--", |_| Token::new_emp(Tag::MinusMinus))
                || self.scan_seq("-", |_| Token::new_emp(Tag::Minus))
                || self.scan_seq("*", |_| Token::new_emp(Tag::Asterisk))
                || self.scan_seq("/", |_| Token::new_emp(Tag::Slash));

            if !tokenized {
                break;
            }
        }
        if self.can_next(0) {
            panic!("Unexpected token at {}", self.cur_pos + 1);
        } else {
            self.add_eof_tok();
        }
    }

    fn skip_whitespaces(&mut self) -> bool {
        let len = self.peek_while(|c| c.is_whitespace());
        self.take(len);
        false
    }

    fn scan_pred<P, F>(&mut self, pred: P, fact: F) -> bool
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
            self.add_tok_and_take(fact(&self.source_text[pos..(pos + i)]), i);
            true
        } else {
            false
        }
    }

    fn scan_seq<F>(&mut self, seq: &str, fact: F) -> bool
    where
        F: Fn(&str) -> Token,
    {
        if !self.can_next(seq.len()) {
            return false;
        }
        let mut i = 0usize;
        for s in seq.chars() {
            if self.peek(i).map_or(false, |c| c != s) {
                return false;
            }
            i += 1;
        }
        self.add_tok_and_take(fact(seq), i);
        true
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Tag, Token};

    fn s(s: &str) -> String {
        return s.to_string();
    }

    fn add_eof(mut toks: Vec<Token>) -> Vec<Token> {
        toks.push(Token::new_eof());
        toks
    }

    #[test]
    fn test_lex_eof() {
        let toks = Lexer::new(s("  ")).run();

        let exps = vec![Token::new_eof()];

        assert_eq!(toks, exps);
    }

    #[test]
    fn test_lex_only_integer() {
        let toks = Lexer::new(s("  123 ")).run();

        let exps = add_eof(vec![Token::new_str(Tag::Integer, "123")]);

        assert_eq!(toks, exps);
    }

    #[test]
    fn test_lex_integers_and_arith_ops() {
        let toks = Lexer::new(s("2 + 2*2")).run();

        let exps = add_eof(vec![
            Token::new_str(Tag::Integer, "2"),
            Token::new_emp(Tag::Plus),
            Token::new_str(Tag::Integer, "2"),
            Token::new_emp(Tag::Asterisk),
            Token::new_str(Tag::Integer, "2"),
        ]);

        assert_eq!(toks, exps);
    }

    #[test]
    fn test_lex_integer_inc() {
        let toks = Lexer::new(s("-2+++--1 ")).run();

        let exps = add_eof(vec![
            Token::new_emp(Tag::Minus),
            Token::new_str(Tag::Integer, "2"),
            Token::new_emp(Tag::PlusPlus),
            Token::new_emp(Tag::Plus),
            Token::new_emp(Tag::MinusMinus),
            Token::new_str(Tag::Integer, "1"),
        ]);

        assert_eq!(toks, exps);
    }

    #[test]
    fn test_lex_ast_with_slash() {
        let toks = Lexer::new(s("10*/11")).run();

        let exps = add_eof(vec![
            Token::new_str(Tag::Integer, "10"),
            Token::new_emp(Tag::Asterisk),
            Token::new_emp(Tag::Slash),
            Token::new_str(Tag::Integer, "11"),
        ]);

        assert_eq!(toks, exps);
    }
}
