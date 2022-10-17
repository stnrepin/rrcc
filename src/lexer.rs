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
    value: Vec<char>,
}

impl Token {
    fn new(tag: Tag, value: &[char]) -> Token {
        Token {
            tag,
            value: value.to_vec(),
        }
    }

    #[allow(dead_code)]
    fn new_str(tag: Tag, value: &str) -> Token {
        let chars: Vec<char> = value.chars().collect();
        Self::new(tag, &chars)
    }

    #[allow(dead_code)]
    fn new_eof() -> Token {
        Token {
            tag: Tag::Eof,
            value: Vec::new(),
        }
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

    fn add_tok_and_take(&mut self, tag: Tag, len: usize) {
        let value = &self.source_text[self.cur_pos..(self.cur_pos + len)];
        let tok = Token::new(tag, value);

        self.tokens.push(tok);
        self.take(len);
    }

    fn run_parse(&mut self) {
        while self.can_next(0) {
            self.skip_whitespaces();
            let lex = self.peek(0);
            if lex.is_none() {
                self.add_tok_and_take(Tag::Eof, 0);
                return;
            }
            let lex = lex.expect("internal compiler error: EOF is not handled");
            let (tag, len) = self.tok(lex);
            self.add_tok_and_take(tag, len);
        }
        self.add_tok_and_take(Tag::Eof, 0);
    }

    fn tok(&self, lex: char) -> (Tag, usize) {
        // @todo Remove explicit length for non-value tokens.

        match lex {
            int if int.is_numeric() => {
                let len = self.peek_while(|c| c.is_numeric());
                (Tag::Integer, len)
            }
            '+' => match self.peek(1) {
                Some('+') => return (Tag::PlusPlus, 2),
                _ => (Tag::Plus, 1),
            },
            '-' => match self.peek(1) {
                Some('-') => return (Tag::MinusMinus, 2),
                _ => (Tag::Minus, 1),
            },
            '*' => (Tag::Asterisk, 1),
            '/' => (Tag::Slash, 1),
            _ => panic!("Unexpected lexeme at {}", self.cur_pos + 1),
        }
    }

    fn skip_whitespaces(&mut self) {
        let len = self.peek_while(|c| c.is_whitespace());
        self.take(len);
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Tag, Token};

    fn s(s: &str) -> String {
        return s.to_string();
    }

    fn add_eof(mut toks: Vec<Token>) -> Vec<Token> {
        toks.push(Token::new_str(Tag::Eof, ""));
        toks
    }

    #[test]
    fn test_lex_eof() {
        let toks = Lexer::new(s("  ")).run();

        let exps = vec![Token::new_eof()];
        assert!(toks == exps);
    }

    #[test]
    fn test_lex_only_integer() {
        let toks = Lexer::new(s("  123 ")).run();

        let exps = add_eof(vec![Token::new_str(Tag::Integer, "123")]);
        assert!(toks == exps);
    }

    #[test]
    fn test_lex_integers_and_arith_ops() {
        let toks = Lexer::new(s("2 + 2*2")).run();

        let exps = add_eof(vec![
            Token::new_str(Tag::Integer, "2"),
            Token::new_str(Tag::Plus, "+"),
            Token::new_str(Tag::Integer, "2"),
            Token::new_str(Tag::Asterisk, "*"),
            Token::new_str(Tag::Integer, "2"),
        ]);

        assert!(toks == exps);
    }

    #[test]
    fn test_lex_integer_inc() {
        let toks = Lexer::new(s("-2+++--1 ")).run();

        let exps = add_eof(vec![
            Token::new_str(Tag::Minus, "-"),
            Token::new_str(Tag::Integer, "2"),
            Token::new_str(Tag::PlusPlus, "++"),
            Token::new_str(Tag::Plus, "+"),
            Token::new_str(Tag::MinusMinus, "--"),
            Token::new_str(Tag::Integer, "1"),
        ]);

        assert!(toks == exps);
    }

    #[test]
    fn test_lex_ast_with_slash() {
        let toks = Lexer::new(s("10*/11")).run();

        let exps = add_eof(vec![
            Token::new_str(Tag::Integer, "10"),
            Token::new_str(Tag::Asterisk, "*"),
            Token::new_str(Tag::Slash, "/"),
            Token::new_str(Tag::Integer, "11"),
        ]);

        assert!(toks == exps);
    }
}
