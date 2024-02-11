use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use regex::Regex;

pub fn parse(input: &str) -> Result<JohnValue, String> {
    let tokens = tokenize(input);
    let mut parser = JohnParser::new(tokens);
    // Either Top level object or value
    match parser.peek() {
        Some(Token::Identifier(_)) => {
            parser.parse_tl_john_object()
        }
        Some(_) => {
            parser.parse_john_value()
        }
        None => {
            Err("Empty input".to_string())
        }
    }
}

struct JohnParser {
    tokens: Vec<Token>,
    index: usize,
}

impl JohnParser {
    fn new(tokens: Vec<Token>) -> JohnParser {
        JohnParser {
            tokens,
            index: 0,
        }
    }

    fn next(&mut self) -> Option<&Token> {
        if self.index < self.tokens.len() {
            let token = &self.tokens[self.index];
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<&Token> {
        if self.index < self.tokens.len() {
            Some(&self.tokens[self.index])
        } else {
            None
        }
    }

    fn parse_tl_john_object(&mut self) -> Result<JohnValue, String> {
        let mut object = HashMap::new();
        while let Some(Token::Identifier(key)) = self.next() {
            let k = key.to_string();
            match self.peek() {
                Some(_) => { 
                    object.insert(k, self.parse_john_value()?);
                }
                None => {
                    return Err("Invalid value".to_string())
                }
            }
        }
        Ok(JohnValue::JohnObject(object))
    }

    fn parse_john_value(&mut self) -> Result<JohnValue, String> {
        match self.next() {
            Some(token) => match token {
                Token::Value(value) => {
                    parse_john_primitive(value)
                }
                Token::OpenCurly => {
                    self.parse_john_object()
                }
                Token::OpenSquare => {
                    self.parse_john_array()
                }
                Token::OpenParen => {
                    self.parse_john_tuple()
                }
                _ => {
                    Err(format!("Unexpected {}", token))
                }
            }
            _ => {
                Err("Invalid value".to_string())
            }
        }
    }

    fn parse_john_object(&mut self) -> Result<JohnValue, String> {
        let mut object = HashMap::new();
        while let Some(Token::Identifier(key)) = self.next() { // should discard closing curly
            let k = key.to_string();
            match self.peek() {
                Some(_) => { 
                    object.insert(k, self.parse_john_value()?);
                }
                None => {
                    return Err("Invalid value".to_string())
                }
            }
        }
        Ok(JohnValue::JohnObject(object))
    }

    fn parse_john_array(&mut self) -> Result<JohnValue, String> {
        let mut array = vec![];
        while let Ok(value) = self.parse_john_value() {
            array.push(value);
        }
        Ok(JohnValue::JohnArray(array))
        // Funny thing, any construct can end with any bracket in this implementation...
    }

    fn parse_john_tuple(&mut self) -> Result<JohnValue, String> {
        let mut tuple = vec![];
        while let Ok(value) = self.parse_john_value() {
            tuple.push(value);
        }
        Ok(JohnValue::JohnTuple(tuple))
    }
}

fn parse_john_primitive(value: &String) -> Result<JohnValue, String> {
    if let Ok(int) = value.parse::<i64>() {
        Ok(JohnValue::JohnInt(int))
    } else if let Ok(float) = value.parse::<f64>() {
        Ok(JohnValue::JohnFloat(float))
    } else if value == "true" {
        Ok(JohnValue::JohnBool(true))
    } else if value == "false" {
        Ok(JohnValue::JohnBool(false))
    } else if value.starts_with("\"") && value.ends_with("\"") {
        Ok(JohnValue::JohnString(value[1..value.len() - 1].to_string()))
    } else if value.starts_with("'") && value.ends_with("'") {
        Ok(JohnValue::JohnChar(value.chars().nth(1).unwrap()))
    } else if value == "abyss" || value == "#" {
        Ok(JohnValue::JohnAbyss)
    } else if value.starts_with("v") {
        let parts: Vec<&str> = value[1..].split('.').collect();
        let major = parts[0].parse::<i64>().unwrap();
        let minor = parts[1].parse::<i64>().unwrap();
        if parts.len() == 2 {
            Ok(JohnValue::JohnVersion(major, minor, None, None))
        } else if parts.len() == 3 {
            Ok(JohnValue::JohnVersion(major, minor, Some(parts[2].parse::<i64>().unwrap()), None))
        } else if parts.len() == 4 {
            Ok(JohnValue::JohnVersion(major, minor, Some(parts[2].parse::<i64>().unwrap()), Some(parts[3].parse::<i64>().unwrap())))
        } else {
            Err("Invalid version".to_string())
        }
    } else if value.starts_with("*") {
        Ok(JohnValue::JohnIndex(false, value[1..].parse::<i64>().unwrap()))
    } else if value.starts_with("^") {
        Ok(JohnValue::JohnIndex(true, value[1..].parse::<i64>().unwrap()))
    } else if value.contains("..") {
        let parts: Vec<&str> = value.split("..").collect();
        let start = parts[0].parse::<i64>().unwrap();
        let end = parts[1].parse::<i64>().unwrap();
        if parts.len() == 2 {
            Ok(JohnValue::JohnRange(start, end, None))
        } else if parts.len() == 3 {
            Ok(JohnValue::JohnRange(start, end, Some(parts[2].parse::<i64>().unwrap())))
        } else {
            Err("Invalid range".to_string())
        }
    } else {
        Err("Invalid value".to_string())
    }
}

pub fn minify(input: &str) -> String {
    let re = Regex::new(r"\s*([\{\[\(\)\]\}])\s*").unwrap();
    let tokens = tokenize(input);
    return re.replace_all(
        &tokens
            .iter()
            .map(|t| t.into())
            .collect::<Vec<String>>()
            .join(" "),
        "$1"
    ).into();
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '{' => tokens.push(Token::OpenCurly),
            '}' => tokens.push(Token::CloseCurly),
            '[' => tokens.push(Token::OpenSquare),
            ']' => tokens.push(Token::CloseSquare),
            '(' => tokens.push(Token::OpenParen),
            ')' => tokens.push(Token::CloseParen),
            _ if c.is_whitespace() || c == ':' || c == ',' || c == ';' => {}
            // parse anything that isn't a delimeter; differentiation in "into" impl
            _ => {
                let mut s = String::new();
                s.push(c);
                while let Some(&c) = chars.peek() {
                    if !c.is_whitespace() && c != ':' && c != ',' && c != ';' && c != '{' && c != '}' && c != '[' && c != ']' && c != '(' && c != ')' {
                        s.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(s.into());
            }
        }
    }
    tokens
}

#[derive(Debug)]
#[derive(PartialEq)]
enum Token {
    OpenCurly,
    CloseCurly,
    OpenSquare,
    CloseSquare,
    OpenParen,
    CloseParen,
    Value(String),
    Identifier(String)
}

impl Into<String> for &Token {
    fn into(self) -> String {
        match self {
            Token::OpenCurly => "{".to_string(),
            Token::CloseCurly => "}".to_string(),
            Token::OpenSquare => "[".to_string(),
            Token::CloseSquare => "]".to_string(),
            Token::OpenParen => "(".to_string(),
            Token::CloseParen => ")".to_string(),
            Token::Value(s) => s.to_string(),
            Token::Identifier(s) => s.to_string(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", Into::<String>::into(self))
    }
}

impl From<String> for Token {
    fn from(s: String) -> Self {
        match s.as_str() {
            "{" => Token::OpenCurly,
            "}" => Token::CloseCurly,
            "[" => Token::OpenSquare,
            "]" => Token::CloseSquare,
            "(" => Token::OpenParen,
            ")" => Token::CloseParen,
            _ => {
                // the power of JOHN is that i literally don't have to worry about context
                let re = Regex::new(r"^[a-zA-Z_]\w*$").unwrap();
                if re.is_match(&s) && s != "true" && s != "false" && s != "abyss" {
                    Token::Identifier(s)
                } else {
                    Token::Value(s)
                }
            }
        }
    }
}

#[derive(Debug)]
#[derive(PartialEq)]
pub enum JohnValue {
    JohnAbyss,
    JohnBool(bool),
    JohnInt(i64),
    JohnFloat(f64),
    JohnString(String),
    JohnChar(char),
    JohnArray(Vec<JohnValue>),
    JohnObject(HashMap<String, JohnValue>),
    JohnTuple(Vec<JohnValue>),
    JohnRange(i64, i64, Option<i64>),
    JohnIndex(bool, i64),
    JohnVersion(i64, i64, Option<i64>, Option<i64>),
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_tokenize() {
        assert_eq!(tokenize(r#"
            {
                a: 1,
                b 2;
                c [
                    3,
                    4
                    { d: "hi" }
                ], g (1 "2" 3)
            }
        "#), vec![
            Token::OpenCurly,
            Token::Identifier("a".to_string()),
            Token::Value("1".to_string()),
            Token::Identifier("b".to_string()),
            Token::Value("2".to_string()),
            Token::Identifier("c".to_string()),
            Token::OpenSquare,
            Token::Value("3".to_string()),
            Token::Value("4".to_string()),
            Token::OpenCurly,
            Token::Identifier("d".to_string()),
            Token::Value("\"hi\"".to_string()),
            Token::CloseCurly,
            Token::CloseSquare,
            Token::Identifier("g".to_string()),
            Token::OpenParen,
            Token::Value("1".to_string()),
            Token::Value("\"2\"".to_string()),
            Token::Value("3".to_string()),
            Token::CloseParen,
            Token::CloseCurly,
        ]);
    }

    #[test]
    fn test_minify() {
        assert_eq!(minify(r"
            {
                a: 1,
                b 2;
                c 3
            }
        "), r"{a 1 b 2 c 3}");
    }

    #[test]
    fn test_value_int() {
        assert_eq!(parse("1"), Ok(JohnValue::JohnInt(1)));
    }

    #[test]
    fn test_value_float() {
        assert_eq!(parse("1.25"), Ok(JohnValue::JohnFloat(1.25)));
    }

    #[test]
    fn test_value_string() {
        assert_eq!(parse(r#""hello""#), Ok(JohnValue::JohnString("hello".to_string())));
    }

    #[test]
    fn test_value_char() {
        assert_eq!(parse("'a'"), Ok(JohnValue::JohnChar('a')));
    }

    #[test]
    fn test_value_bool() {
        assert_eq!(parse("true"), Ok(JohnValue::JohnBool(true)));
        assert_eq!(parse("false"), Ok(JohnValue::JohnBool(false)));
    }

    #[test]
    fn test_value_abyss() {
        assert_eq!(parse("#"), Ok(JohnValue::JohnAbyss));
        assert_eq!(parse("abyss"), Ok(JohnValue::JohnAbyss));
    }

    #[test]
    fn test_value_range() {
        assert_eq!(parse("1..5"), Ok(JohnValue::JohnRange(1, 5, None)));
        assert_eq!(parse("1..5..2"), Ok(JohnValue::JohnRange(1, 5, Some(2))));
    }

    #[test]
    fn test_value_index() {
        assert_eq!(parse("*1"), Ok(JohnValue::JohnIndex(false, 1)));
        assert_eq!(parse("^1"), Ok(JohnValue::JohnIndex(true, 1)));
    }

    #[test]
    fn test_value_version() {
        assert_eq!(parse("v1.2"), Ok(JohnValue::JohnVersion(1, 2, None, None)));
        assert_eq!(parse("v1.2.3"), Ok(JohnValue::JohnVersion(1, 2, Some(3), None)));
        assert_eq!(parse("v1.2.3.4"), Ok(JohnValue::JohnVersion(1, 2, Some(3), Some(4))));
    }

    #[test]
    fn test_tl_object() {
        assert_eq!(parse("a 1 b 2 c 3"), Ok(JohnValue::JohnObject(
            vec![
                ("a".to_string(), JohnValue::JohnInt(1)),
                ("b".to_string(), JohnValue::JohnInt(2)),
                ("c".to_string(), JohnValue::JohnInt(3)),
            ].into_iter().collect()
        )));
    }

    #[test]
    fn test_value_array() {
        assert_eq!(parse("[1, 2, 3]"), Ok(JohnValue::JohnArray(
            vec![
                JohnValue::JohnInt(1),
                JohnValue::JohnInt(2),
                JohnValue::JohnInt(3),
            ]
        )));
    }

    #[test]
    fn test_value_tuple() {
        assert_eq!(parse("(1, 2, 3)"), Ok(JohnValue::JohnTuple(
            vec![
                JohnValue::JohnInt(1),
                JohnValue::JohnInt(2),
                JohnValue::JohnInt(3),
            ]
        )));
    }

    #[test]
    fn test_value_tuple_different_types() {
        assert_eq!(parse(r#"(1, "2", 3.0)"#), Ok(JohnValue::JohnTuple(
            vec![
                JohnValue::JohnInt(1),
                JohnValue::JohnString("2".to_string()),
                JohnValue::JohnFloat(3.0),
            ]
        )));
    }

    #[test]
    fn test_value_object() {
        assert_eq!(parse(r#"
            {
                a: 1,
                b 2;
                c 3
            }
        "#), Ok(JohnValue::JohnObject(
            vec![
                ("a".to_string(), JohnValue::JohnInt(1)),
                ("b".to_string(), JohnValue::JohnInt(2)),
                ("c".to_string(), JohnValue::JohnInt(3)),
            ].into_iter().collect()
        )));
    }

    #[test]
    fn test_value_object_nested() {
        assert_eq!(parse(r#"
            {
                a: 1,
                b 2;
                c [
                    3,
                    4
                    { d: "hi" }
                ], g (1 "2" 3)
            }
        "#), Ok(JohnValue::JohnObject(
            vec![
                ("a".to_string(), JohnValue::JohnInt(1)),
                ("b".to_string(), JohnValue::JohnInt(2)),
                ("c".to_string(), JohnValue::JohnArray(
                    vec![
                        JohnValue::JohnInt(3),
                        JohnValue::JohnInt(4),
                        JohnValue::JohnObject(
                            vec![
                                ("d".to_string(), JohnValue::JohnString("hi".to_string())),
                            ].into_iter().collect()
                        ),
                    ]
                )),
                ("g".to_string(), JohnValue::JohnTuple(
                    vec![
                        JohnValue::JohnInt(1),
                        JohnValue::JohnString("2".to_string()),
                        JohnValue::JohnInt(3),
                    ]
                )),
            ].into_iter().collect()
        )));
    }
}
