use logos::{Lexer, Logos};
use nom::{
    bytes::complete::{tag, take_till1},
    error::Error as NomError,
};

fn keyword_get(lex: &mut Lexer<Token>) -> Option<Keyword> {
    let output: (&str, &str) =
        take_till1::<_, &str, NomError<&str>>(|c| c == '/')(lex.slice()).unwrap();
    if output.0.starts_with('/') {
        let ident = tag::<&str, &str, NomError<&str>>("/")(output.0).unwrap().0;
        if output.1 == "" {
            return Some(Keyword {
                namespace: None,
                ident: ident.to_string(),
            });
        } else {
            return Some(Keyword {
                namespace: Some(output.1.to_string().get(1..).unwrap().to_string()),
                ident: ident.to_string(),
            });
        }
    } else {
        let ident = tag::<&str, &str, NomError<&str>>(":")(output.1).unwrap().0;
        return Some(Keyword {
            namespace: None,
            ident: ident.to_string(),
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
                ident: output.0.to_string(),
            });
        } else {
            return Some(Symbol {
                namespace: Some(output.1.to_string()),
                ident: output.0.get(1..).unwrap().to_string(),
            });
        }
    } else {
        return Some(Symbol {
            namespace: None,
            ident: output.1.to_string(),
        });
    }
}
#[derive(Debug, PartialEq)]
pub struct Symbol {
    namespace: Option<String>,
    ident: String,
}
#[derive(Debug, PartialEq)]
pub struct Keyword {
    namespace: Option<String>,
    ident: String,
}
#[derive(Debug, PartialEq, Logos)]
pub enum Token {
    #[error]
    #[regex(r"", logos::skip)]
    Error,
    #[regex(r":[^\s]+", keyword_get)]
    Keyword(Keyword),
    #[token(r"nil")]
    Nil,
    #[regex(r"[^:][^\s]+", symbol_get)]
    Symbol(Symbol),
    #[regex(r"\s", logos::skip)]
    Space,
    #[token("\n")]
    NewLine,
    #[token("{")]
    MapStart,
    #[token("}")]
    MapEnd,
    #[token("[")]
    SetStart,
    #[token("]")]
    SetEnd,
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
                ident: "hello".to_string(),
                namespace: None
            }))
        );
        let mut lexer = Token::lexer(":hello/world");
        assert_eq!(
            lexer.next(),
            Some(Token::Keyword(Keyword {
                namespace: Some("hello".to_string()),
                ident: "world".to_string()
            }))
        )
    }
    #[test]
    fn nil_tests() {
        let mut lexer = Token::lexer("nil");
        assert_eq!(lexer.next(), Some(Token::Nil))
    }
    #[test]
    fn symbol_test() {
        let mut lexer = Token::lexer("hello ");
        assert_eq!(
            lexer.next(),
            Some(Token::Symbol(Symbol {
                namespace: None,
                ident: "hello".to_string()
            }))
        );
        let mut lexer = Token::lexer("hello/world");
        assert_eq!(
            lexer.next(),
            Some(Token::Symbol(Symbol {
                namespace: Some("hello".to_string()),
                ident: "world".to_string()
            }))
        );
    }
}
