use bigdecimal::BigDecimal;
use std::str::FromStr;
use logos::{Lexer, Logos, skip};
use nom::{
    bytes::complete::{tag, take_till1, take_until},
    error::Error as NomError,
};
use num_bigint::BigInt;
fn keyword_get(lex: &mut Lexer<Token>) -> Option<Keyword> {
    let output: (&str, &str) =
        take_till1::<_, &str, NomError<&str>>(|c| c == '/')(lex.slice()).unwrap();
    if output.0.starts_with('/') {
        let name = tag::<&str, &str, NomError<&str>>("/")(output.0).unwrap().0;
        if output.1 == "" {
            return Some(Keyword {
                namespace: None,
                name: name.to_string(),
            });
        } else {
            return Some(Keyword {
                namespace: Some(output.1.get(1..).unwrap().to_string()),
                name: name.to_string(),
            });
        }
    } else {
        let name = tag::<&str, &str, NomError<&str>>(":")(output.1).unwrap().0;
        return Some(Keyword {
            namespace: None,
            name: name.to_string(),
        });
    }
}

fn symbol_get(lex: &mut Lexer<Token>) -> Option<Symbol> {
    let output: (&str, &str) =
        take_till1::<_, &str, NomError<&str>>(|c| c == '/')(lex.slice()).unwrap();
    if output.0.starts_with('/') {
        if output.1 == "" {
            return Some(Symbol {
                namespace: None,
                name: output.0.to_string(),
            });
        } else {
            return Some(Symbol {
                namespace: Some(output.1.to_string()),
                name: output.0.get(1..).unwrap().to_string(),
            });
        }
    } else {
        return Some(Symbol {
            namespace: None,
            name: output.1.to_string(),
        });
    }
}
#[derive(Debug, PartialEq)]
pub struct Symbol {
    namespace: Option<String>,
    name: String,
}
#[derive(Debug, PartialEq)]
pub struct Keyword {
    namespace: Option<String>,
    name: String,
}
fn string_callback(lex: &mut Lexer<Token>) -> Option<String> {
    Some(
        take_until::<&str, &str, NomError<&str>>("\"")(
            tag::<&str, &str, NomError<&str>>("\"")(lex.slice())
                .unwrap()
                .0,
        )
        .unwrap()
        .1
        .to_string(),
    )
}
#[derive(Debug, PartialEq, Logos)]
pub enum Token {
    #[regex(r":[^\s]+", keyword_get)]
    Keyword(Keyword),
    #[token(r"nil")]
    Nil,
    #[regex(r"\s", skip)]
    Space,
    #[regex(r#"[^:^\s][^\s^"]+"#, symbol_get)]
    Symbol(Symbol),
    #[token(r"\n", skip)]
    NewLine,
    #[regex(r"[\t|\s\s\s\s]", skip, priority = 2)]
    Tab,
    #[token(r"{")]
    MapStart,
    #[token(r"}")]
    SetMapEnd,
    #[token(r"[")]
    VectorStart,
    #[token(r"]")]
    VectorEnd,
    #[regex(r#""[^"]*""#, string_callback, priority = 3)]
    Str(String),
    #[regex(r";\s?[^\n]*", skip, priority = 1)]
    Comment,
    #[token(r"#{")]
    SetStart,
    #[regex(r"\d+", |lex| BigInt::from_str(lex.slice()).unwrap(), priority = 3)]
    BigInt(BigInt),
    #[regex(r"\d+\.\d+", |lex| BigDecimal::from_str(lex.slice()).unwrap(), priority = 3)]
    BigDec(BigDecimal),
    #[error]
    Error,
}

#[inline(always)]
pub fn lex_str(input: &str) -> Vec<Token> {
    Token::lexer(input).collect()
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_tests() {
        let mut lexer = Token::lexer(":hello");
        assert_eq!(
            lexer.next(),
            Some(Token::Keyword(Keyword {
                name: "hello".to_string(),
                namespace: None
            }))
        );
        let mut lexer = Token::lexer(":hello/world");
        assert_eq!(
            lexer.next(),
            Some(Token::Keyword(Keyword {
                namespace: Some("hello".to_string()),
                name: "world".to_string()
            }))
        )
    }
    #[test]
    fn nil_tests() {
        let mut lexer = Token::lexer("nil");
        assert_eq!(lexer.next(), Some(Token::Nil))
    }
    #[test]
    fn symbol_tests() {
        let mut lexer = Token::lexer("hello ");
        assert_eq!(
            lexer.next(),
            Some(Token::Symbol(Symbol {
                namespace: None,
                name: "hello".to_string()
            }))
        );
        let mut lexer = Token::lexer("hello/world");
        assert_eq!(
            lexer.next(),
            Some(Token::Symbol(Symbol {
                namespace: Some("hello".to_string()),
                name: "world".to_string()
            }))
        );
    }
    #[test]
    fn string_tests() {
        let mut lexer = Token::lexer("\"Hello world\"");
        assert_eq!(lexer.next(), Some(Token::Str("Hello world".to_string())))
    }
    #[test]
    fn comment_tests() {
        let mut lexer = Token::lexer("; abc");
        assert_eq!(lexer.next(), None)
    }
    #[test]
    fn vector_tests() {
        let tokens = lex_str("[ \"Hello\" \"World\" ]");
        assert_eq!(tokens, vec![Token::VectorStart, Token::Str("Hello".to_string()), Token::Str("World".to_string()), Token::VectorEnd])
    }
    #[test]
    fn maps_tests() {
        let tokens = lex_str("{ \"A\" \"B\" }");
        assert_eq!(tokens, vec![Token::MapStart, Token::Str("A".to_string()), Token::Str("B".to_string()), Token::SetMapEnd])
    }
    #[test]
    fn set_tests() {
        let tokens = lex_str("#{ \"A\" \"B\" }");
        assert_eq!(tokens, vec![Token::SetStart, Token::Str("A".to_string()), Token::Str("B".to_string()), Token::SetMapEnd])
    }
    #[test]
    fn bigint_tests() {
        let mut lexer = Token::lexer("12");
        assert_eq!(lexer.next(), Some(Token::BigInt(BigInt::from_str("12").unwrap())))
    }
    #[test]
    fn bigdec_tests() {
        let mut lexer = Token::lexer("12.3");
        assert_eq!(lexer.next(), Some(Token::BigDec(BigDecimal::from_str("12.3").unwrap())))
    }
}
