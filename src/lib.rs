use bigdecimal::BigDecimal;
use biiter::BiIterator;
use logos::{skip, Lexer, Logos};
use nom::{
    bytes::complete::{tag, take_till1, take_until},
    error::Error as NomError,
};
use num_bigint::BigInt;
use std::convert::From;
use std::{
    collections::{BTreeMap, BTreeSet},
    str::FromStr,
};
fn keyword_get(lex: &mut Lexer<Token>) -> Option<Keyword> {
    let output: (&str, &str) =
        take_till1::<_, &str, NomError<&str>>(|c| c == '/')(lex.slice()).expect("Should not occur");
    if output.0.starts_with('/') {
        let name = tag::<&str, &str, NomError<&str>>("/")(output.0).expect("Should not occur").0;
        if output.1 == "" {
            return Some(Keyword {
                namespace: None,
                name: name.to_string(),
            });
        } else {
            return Some(Keyword {
                namespace: Some(output.1.get(1..).expect("Should not occur").to_string()),
                name: name.to_string(),
            });
        }
    } else {
        let name = tag::<&str, &str, NomError<&str>>(":")(output.1).expect("Should not occur").0;
        return Some(Keyword {
            namespace: None,
            name: name.to_string(),
        });
    }
}
fn bigdecimal_lex(lex: &mut Lexer<Token>) -> BigDecimal {
    let output = tag::<&str, &str, NomError<&str>>("M")(lex.slice());
    match output {
        Ok(num) => return BigDecimal::from_str(num.0).expect("Should not occur"),
        Err(_) => return BigDecimal::from_str(lex.slice()).expect("Should not occur"),
    }
}
fn symbol_get(lex: &mut Lexer<Token>) -> Option<Symbol> {
    let output: (&str, &str) =
        take_till1::<_, &str, NomError<&str>>(|c| c == '/')(lex.slice()).expect("Should not occur");
    if output.0.starts_with('/') {
        if output.1 == "" {
            return Some(Symbol {
                namespace: None,
                name: output.0.to_string(),
            });
        } else {
            return Some(Symbol {
                namespace: Some(output.1.to_string()),
                name: output.0.get(1..).expect("Should not occur").to_string(),
            });
        }
    } else {
        return Some(Symbol {
            namespace: None,
            name: output.1.to_string(),
        });
    }
}
fn tag_get(lex: &mut Lexer<Token>) -> Option<Symbol> {
    let output = take_till1::<_, &str, NomError<&str>>(|c| c == '/')(
        tag::<&str, &str, NomError<&str>>("#")(lex.slice())
            .expect("Should not occur")
            .0,
    )
    .expect("Should not occur");
    if output.0.starts_with('/') {
        if output.1 == "" {
            return Some(Symbol {
                namespace: None,
                name: output.0.to_string(),
            });
        } else {
            return Some(Symbol {
                namespace: Some(output.1.to_string()),
                name: output.0.get(1..).expect("Should not occur").to_string(),
            });
        }
    } else {
        return Some(Symbol {
            namespace: None,
            name: output.1.to_string(),
        });
    }
}
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Symbol {
    namespace: Option<String>,
    name: String,
}
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Keyword {
    namespace: Option<String>,
    name: String,
}
fn string_callback(lex: &mut Lexer<Token>) -> Option<String> {
    Some(
        take_until::<&str, &str, NomError<&str>>("\"")(
            tag::<&str, &str, NomError<&str>>("\"")(lex.slice())
                .expect("Should not occur")
                .0,
        )
        .expect("Should not occur")
        .1
        .to_string(),
    )
}
#[derive(Debug, PartialEq, Logos, Clone)]
enum Token {
    #[regex(r":[^\s]+", keyword_get)]
    Keyword(Keyword),
    #[token(r"nil")]
    Nil,
    #[regex(r"\s", skip)]
    Space,
    #[regex(r#"[^:^\s^#^;][^\s^"]+"#, symbol_get)]
    Symbol(Symbol),
    #[token(r"#_")]
    Discard,
    #[token(r"\n", skip)]
    NewLine,
    #[regex(r#"#[^\s^"]+"#, tag_get)]
    Tag(Symbol),
    #[regex(r"[\t|\s\s\s\s]", skip, priority = 2)]
    Tab,
    #[regex(r"\\.", callback = |lex| {tag::<&str, &str, NomError<&str>>(r"\")(lex.slice()).unwrap().0.parse::<char>().expect("Should not occur")})]
    Char(char),
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
    #[regex(r";\s?[^\n]*", skip, priority = 2)]
    Comment,
    #[token(r"#{")]
    SetStart,
    #[regex(r"\d+", |lex| BigInt::from_str(lex.slice()).expect("Should not occur"), priority = 3)]
    BigInt(BigInt),
    #[regex(r"M?\d+\.\d+", bigdecimal_lex)]
    BigDec(BigDecimal),
    #[error]
    Error,
}
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Character(char),
    String(String),
    Symbol(Symbol),
    Keyword(Keyword),
    BigInt(BigInt),
    BigDec(BigDecimal),
    Vector(Vec<Value>),
    Map(BTreeMap<Value, Value>),
    TaggedElement(Symbol, Box<Value>),
    Set(BTreeSet<Value>),
}
fn equal(v1: &Value, v2: &Value) -> bool {
    match (v1, v2) {
        (Value::Nil, Value::Nil) => true,
        (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
        (Value::String(s1), Value::String(s2)) => s1 == s2,
        (Value::Character(c1), Value::Character(c2)) => c1 == c2,
        (Value::Symbol(s1), Value::Symbol(s2)) => s1 == s2,
        (Value::Keyword(k1), Value::Keyword(k2)) => k1 == k2,
        (Value::BigInt(bi1), Value::BigInt(bi2)) => bi1 == bi2,
        (Value::BigDec(bd1), Value::BigDec(bd2)) => bd1 == bd2,
        (Value::Vector(vals1), Value::Vector(vals2)) => vals1 == vals2,
        (Value::TaggedElement(tag1, value1), Value::TaggedElement(tag2, value2)) => {
            equal(&Value::Symbol(tag1.clone()), &Value::Symbol(tag2.clone()))
                && equal(value1, value2)
        }
        _ => false,
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        equal(self, other)
    }
}
impl Value {
    fn parse_tokens(input: Vec<Token>) -> Vec<Value> {
        let mut values = Vec::new();
        let mut iter: BiIterator<Token> =
            BiIterator::from(input.iter().cloned().collect::<Vec<Token>>());
        while let Some(i) = iter.next() {
            match i {
                Token::Keyword(key) => values.push(Value::Keyword(key)),
                Token::Nil => values.push(Value::Nil),
                Token::Space => {}
                Token::Symbol(symbol) => values.push(Value::Symbol(symbol)),
                Token::NewLine => {}
                Token::Tag(symbol) => values.push(Value::TaggedElement(
                    symbol,
                    Box::new(Value::new(
                        iter.peek().expect("Must have element after tag").to_owned(),
                    )),
                )),
                Token::Tab => {}
                Token::Char(c) => values.push(Value::Character(c)),
                Token::MapStart => todo!(),
                Token::SetMapEnd => todo!(),
                Token::VectorStart => todo!(),
                Token::VectorEnd => todo!(),
                Token::Str(str) => values.push(Value::String(str)),
                Token::Comment => {}
                Token::SetStart => todo!(),
                Token::BigInt(int) => values.push(Value::BigInt(int)),
                Token::BigDec(dec) => values.push(Value::BigDec(dec)),
                Token::Error => {},
                Token::Discard => {}
            }
        }
        values
    }
    fn new(token: Token) -> Value {
        todo!()
    }
}
fn lex_str(input: &str) -> Vec<Token> {
    Token::lexer(input.trim()).collect()
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
        assert_eq!(
            tokens,
            vec![
                Token::VectorStart,
                Token::Str("Hello".to_string()),
                Token::Str("World".to_string()),
                Token::VectorEnd
            ]
        )
    }
    #[test]
    fn maps_tests() {
        let tokens = lex_str("{ \"A\" \"B\" }");
        assert_eq!(
            tokens,
            vec![
                Token::MapStart,
                Token::Str("A".to_string()),
                Token::Str("B".to_string()),
                Token::SetMapEnd
            ]
        )
    }
    #[test]
    fn set_tests() {
        let tokens = lex_str("#{ \"A\" \"B\" }");
        assert_eq!(
            tokens,
            vec![
                Token::SetStart,
                Token::Str("A".to_string()),
                Token::Str("B".to_string()),
                Token::SetMapEnd
            ]
        )
    }
    #[test]
    fn bigint_tests() {
        let mut lexer = Token::lexer("12");
        assert_eq!(
            lexer.next(),
            Some(Token::BigInt(BigInt::from_str("12").expect("Should not occur")))
        )
    }
    #[test]
    fn bigdec_tests() {
        let mut lexer = Token::lexer("12.3");
        assert_eq!(
            lexer.next(),
            Some(Token::BigDec(BigDecimal::from_str("12.3").expect("Should not occur")))
        );
        let mut lexer = Token::lexer("M12.3");
        assert_eq!(
            lexer.next(),
            Some(Token::BigDec(BigDecimal::from_str("12.3").expect("Should not occur")))
        );
    }
    #[test]
    fn tag_tests() {
        let mut lexer = Token::lexer("#hello");
        assert_eq!(
            lexer.next(),
            Some(Token::Tag(Symbol {
                namespace: None,
                name: "hello".to_string()
            }))
        )
    }
    #[test]
    fn discard_tests() {
        let mut lexer = Token::lexer("#_");
        assert_eq!(lexer.next(), Some(Token::Discard))
    }
}
