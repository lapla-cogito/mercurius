#![allow(unused)]

use std::str::FromStr as _;

#[derive(Debug, PartialEq)]
pub struct Stylesheet {
    pub items: Vec<Item>,
}

#[derive(Debug, PartialEq)]
pub enum Item {
    Rule(Rule),
    AtRule(AtRule),
}

#[derive(Debug, PartialEq)]
pub struct Rule {
    pub selectors: Vec<Selector>,
    pub declarations: Vec<Declaration>,
}

impl Rule {
    fn matches(&self, target: &str) -> bool {
        for selector in &self.selectors {
            if let Selector::Simple(tag_name) = selector {
                for tag_name in tag_name.tag_name.iter().flatten() {
                    if tag_name == target {
                        return true;
                    }
                }
            }
        }
        false
    }
}

#[derive(Debug, PartialEq)]
pub enum Selector {
    Universal,
    Simple(SimpleSelector),
    Type {
        tag_name: String,
    },
    Attribute {
        tag_name: String,
        op: AttributeSelectorOp,
        attribute: String,
        value: String,
    },
    Class {
        class_name: String,
    },
}

impl Selector {
    pub fn matches(&self, target: &crate::parser::dom::Node) -> Option<bool> {
        match self {
            Selector::Simple(s) => {
                if let crate::parser::dom::NodeType::Element(e) = &target.node_type {
                    for tag_name in s.tag_name.iter().flatten() {
                        if tag_name == &e.tag_name {
                            return Some(true);
                        }
                    }
                    return Some(false);
                }
                None
            }
            Selector::Type { tag_name } => {
                if let crate::parser::dom::NodeType::Element(e) = &target.node_type {
                    if tag_name == &e.tag_name {
                        return Some(true);
                    }
                    return Some(false);
                }
                None
            }
            Selector::Attribute {
                tag_name,
                op,
                attribute,
                value,
            } => {
                if let crate::parser::dom::NodeType::Element(e) = &target.node_type {
                    if tag_name == &e.tag_name {
                        if let Some(v) = e.attributes.get(attribute) {
                            match op {
                                AttributeSelectorOp::Eq => {
                                    return Some(v == value);
                                }
                                AttributeSelectorOp::Contain => {
                                    return Some(v.contains(value));
                                }
                            }
                        }
                    }
                }
                Some(false)
            }
            Selector::Class { class_name } => {
                if let crate::parser::dom::NodeType::Element(e) = &target.node_type {
                    if let Some(class) = e.attributes.get("class") {
                        return Some(class.split(' ').any(|c| c == class_name));
                    }
                }
                Some(false)
            }
            Selector::Universal => Some(true),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct SimpleSelector {
    pub tag_name: Vec<Option<String>>,
}

#[derive(Debug, PartialEq)]
pub enum AttributeSelectorOp {
    Eq,
    Contain,
}

#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub value: Value,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Keyword(String),
    Length(f32, Unit),
    Color(Color),
}

#[derive(Debug, PartialEq)]
pub enum Color {
    Rgb(u8, u8, u8),
    Rgba(u8, u8, u8, f32),
    Hsl(u8, u8, u8),
    Hsla(u8, u8, u8, f32),
}

#[derive(Debug, PartialEq)]
pub enum Unit {
    Px,
    Em,
    Ex,
    In,
    Cm,
    Mm,
    Pt,
    Pc,
    Ch,
    Rem,
    Vw,
    Vh,
    Vmin,
    Vmax,
    Q,
}

#[derive(Debug, PartialEq)]
pub enum AtRule {
    Media(String, Vec<Rule>),
}

fn simple_selector(input: &str) -> nom::IResult<&str, Selector> {
    let allow_list = [',', ' ', '.', '[', ']', '='];

    let (rest, name) =
        super::util::css_sanitize(nom::bytes::complete::take_while1(move |c: char| {
            c.is_alphanumeric() || allow_list.contains(&c)
        }))(input)?;

    Ok((
        rest,
        Selector::Simple(SimpleSelector {
            tag_name: name
                .split(',')
                .map(|s| {
                    if s.trim().is_empty() {
                        None
                    } else {
                        Some(s.trim().to_string())
                    }
                })
                .collect(),
        }),
    ))
}

fn declaration(input: &str) -> nom::IResult<&str, Declaration> {
    let (rest, name) = super::util::css_sanitize(nom::bytes::complete::take_while1(|c: char| {
        c.is_alphanumeric() || c == '-'
    }))(input)?;
    let (rest, _) = super::util::ws(nom::bytes::complete::tag(":"))(rest)?;
    let (rest, value) = value(rest)?;
    let (rest, _) = super::util::css_sanitize(nom::bytes::complete::tag(";"))(rest)?;

    Ok((
        rest,
        Declaration {
            name: name.to_string(),
            value,
        },
    ))
}

fn parse_length(input: &str) -> nom::IResult<&str, Value> {
    let (rest, number) = nom::combinator::map_res(
        nom::combinator::recognize(nom::sequence::pair(
            nom::character::complete::digit1,
            nom::combinator::opt(nom::sequence::pair(
                nom::character::complete::char('.'),
                nom::character::complete::digit1,
            )),
        )),
        |s: &str| s.parse::<f32>(),
    )(input)?;

    let (rest, unit) = nom::branch::alt((
        nom::bytes::complete::tag("px"),
        nom::bytes::complete::tag("em"),
        nom::bytes::complete::tag("ex"),
        nom::bytes::complete::tag("in"),
        nom::bytes::complete::tag("cm"),
        nom::bytes::complete::tag("mm"),
        nom::bytes::complete::tag("pt"),
        nom::bytes::complete::tag("pc"),
        nom::bytes::complete::tag("ch"),
        nom::bytes::complete::tag("rem"),
        nom::bytes::complete::tag("vw"),
        nom::bytes::complete::tag("vh"),
        nom::bytes::complete::tag("vmin"),
        nom::bytes::complete::tag("vmax"),
        nom::bytes::complete::tag("q"),
    ))(rest)?;

    match unit {
        "px" => Ok((rest, Value::Length(number, Unit::Px))),
        "em" => Ok((rest, Value::Length(number, Unit::Em))),
        "ex" => Ok((rest, Value::Length(number, Unit::Ex))),
        "in" => Ok((rest, Value::Length(number, Unit::In))),
        "cm" => Ok((rest, Value::Length(number, Unit::Cm))),
        "mm" => Ok((rest, Value::Length(number, Unit::Mm))),
        "pt" => Ok((rest, Value::Length(number, Unit::Pt))),
        "pc" => Ok((rest, Value::Length(number, Unit::Pc))),
        "ch" => Ok((rest, Value::Length(number, Unit::Ch))),
        "rem" => Ok((rest, Value::Length(number, Unit::Rem))),
        "vw" => Ok((rest, Value::Length(number, Unit::Vw))),
        "vh" => Ok((rest, Value::Length(number, Unit::Vh))),
        "vmin" => Ok((rest, Value::Length(number, Unit::Vmin))),
        "vmax" => Ok((rest, Value::Length(number, Unit::Vmax))),
        "q" => Ok((rest, Value::Length(number, Unit::Q))),
        _ => unreachable!(),
    }
}

fn parse_color(input: &str) -> nom::IResult<&str, Value> {
    let (rest, tag) = nom::branch::alt((
        super::util::css_sanitize(nom::bytes::complete::tag("#")),
        super::util::css_sanitize(nom::bytes::complete::tag("rgba")),
        super::util::css_sanitize(nom::bytes::complete::tag("rgb")),
        super::util::css_sanitize(nom::bytes::complete::tag("hsla")),
        super::util::css_sanitize(nom::bytes::complete::tag("hsl")),
    ))(input)?;

    match tag {
        "#" => {
            let (rest, r) =
                nom::combinator::map_res(nom::bytes::complete::take(2usize), |s: &str| {
                    u8::from_str_radix(s, 16)
                })(rest)?;
            let (rest, g) =
                nom::combinator::map_res(nom::bytes::complete::take(2usize), |s: &str| {
                    u8::from_str_radix(s, 16)
                })(rest)?;
            let (rest, b) =
                nom::combinator::map_res(nom::bytes::complete::take(2usize), |s: &str| {
                    u8::from_str_radix(s, 16)
                })(rest)?;
            let (rest, a) = nom::combinator::opt(nom::combinator::map_res(
                nom::bytes::complete::take(2usize),
                |s: &str| u8::from_str_radix(s, 16),
            ))(rest)?;

            if let Some(a) = a {
                Ok((rest, Value::Color(Color::Rgba(r, g, b, a as f32 / 255.0))))
            } else {
                Ok((rest, Value::Color(Color::Rgb(r, g, b))))
            }
        }
        "rgba" => {
            let (rest, _) = super::util::ws(nom::character::complete::char('('))(rest)?;
            let (rest, r) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char(','))(rest)?;
            let (rest, g) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char(','))(rest)?;
            let (rest, b) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char(','))(rest)?;
            let (rest, a) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit() || c == '.'
                })),
                |s: &str| f32::from_str(s),
            )(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char(')'))(rest)?;

            Ok((rest, Value::Color(Color::Rgba(r, g, b, a))))
        }
        "rgb" => {
            let (rest, _) = super::util::ws(nom::character::complete::char('('))(rest)?;
            let (rest, r) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;

            let (rest, _) =
                nom::combinator::opt(super::util::ws(nom::character::complete::char(',')))(rest)?;

            let (rest, g) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (mut rest, is_space) =
                nom::combinator::opt(nom::character::complete::char(' '))(rest)?;
            if is_space.is_none() {
                (rest, _) = nom::combinator::opt(super::util::ws(nom::character::complete::char(
                    ',',
                )))(rest)?;
            }

            let (rest, b) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;

            let (rest, is_alpha) = nom::combinator::opt(super::util::ws(
                nom::bytes::complete::take_until1(")"),
            ))(rest)?;

            if let Some(mut alpha_string) = is_alpha {
                if alpha_string.contains('/') {
                    println!("alpha_string: {:?}", alpha_string);
                    (alpha_string, _) =
                        super::util::ws(nom::character::complete::char('/'))(alpha_string)?;
                } else if alpha_string.contains(',') {
                    (alpha_string, _) =
                        super::util::ws(nom::character::complete::char(','))(alpha_string)?;
                }

                let (_, a) = nom::combinator::map_res(
                    super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                        c.is_ascii_digit() || c == '.'
                    })),
                    |s: &str| f32::from_str(s),
                )(alpha_string)?;
                let (rest, _) = super::util::ws(nom::character::complete::char(')'))(rest)?;
                Ok((rest, Value::Color(Color::Rgba(r, g, b, a))))
            } else {
                let (rest, _) = super::util::ws(nom::character::complete::char(')'))(rest)?;
                Ok((rest, Value::Color(Color::Rgb(r, g, b))))
            }
        }
        "hsla" => {
            let (rest, _) = super::util::ws(nom::character::complete::char('('))(rest)?;
            let (rest, h) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char(','))(rest)?;
            let (rest, s) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char('%'))(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char(','))(rest)?;
            let (rest, l) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char('%'))(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char(','))(rest)?;
            let (rest, a) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit() || c == '.'
                })),
                |s: &str| f32::from_str(s),
            )(rest)?;
            let (rest, _) = super::util::ws(nom::character::complete::char(')'))(rest)?;

            Ok((rest, Value::Color(Color::Hsla(h, s, l, a))))
        }
        "hsl" => {
            let (rest, _) = super::util::ws(nom::character::complete::char('('))(rest)?;
            let (rest, h) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (rest, _) =
                nom::combinator::opt(super::util::ws(nom::character::complete::char(',')))(rest)?;
            let (rest, s) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (rest, _) =
                nom::combinator::opt(super::util::ws(nom::character::complete::char('%')))(rest)?;
            let (rest, _) =
                nom::combinator::opt(super::util::ws(nom::character::complete::char(',')))(rest)?;
            let (rest, l) = nom::combinator::map_res(
                super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(rest)?;
            let (rest, _) =
                nom::combinator::opt(super::util::ws(nom::character::complete::char('%')))(rest)?;

            let (rest, is_alpha) = nom::combinator::opt(super::util::ws(
                nom::bytes::complete::take_until1(")"),
            ))(rest)?;

            if let Some(mut alpha_string) = is_alpha {
                if alpha_string.contains('/') {
                    println!("alpha_string: {:?}", alpha_string);
                    (alpha_string, _) =
                        super::util::ws(nom::character::complete::char('/'))(alpha_string)?;
                } else if alpha_string.contains(',') {
                    (alpha_string, _) =
                        super::util::ws(nom::character::complete::char(','))(alpha_string)?;
                }

                let (_, a) = nom::combinator::map_res(
                    super::util::ws(nom::bytes::complete::take_while1(|c: char| {
                        c.is_ascii_digit() || c == '.'
                    })),
                    |s: &str| f32::from_str(s),
                )(alpha_string)?;
                let (rest, _) = super::util::ws(nom::character::complete::char(')'))(rest)?;
                Ok((rest, Value::Color(Color::Hsla(h, s, l, a))))
            } else {
                let (rest, _) = super::util::ws(nom::character::complete::char(')'))(rest)?;
                Ok((rest, Value::Color(Color::Hsl(h, s, l))))
            }
        }
        _ => unreachable!(),
    }
}

fn parse_at_rule(input: &str) -> nom::IResult<&str, AtRule> {
    let (rest, _) = super::util::css_sanitize(nom::bytes::complete::tag("@media"))(input)?;
    let (rest, media) = super::util::ws(nom::bytes::streaming::take_until("{"))(rest)?;
    let (rest, _) = super::util::ws(nom::character::complete::char('{'))(rest)?;
    let (rest, rules) = nom::multi::many0(super::util::css_sanitize(parse_rule))(rest)?;
    let (rest, _) = super::util::css_sanitize(nom::character::complete::char('}'))(rest)?;

    Ok((rest, AtRule::Media(media.trim().to_string(), rules)))
}

fn value(input: &str) -> nom::IResult<&str, Value> {
    nom::branch::alt((
        parse_length,
        parse_color,
        nom::combinator::map(
            super::util::ws(nom::character::complete::alphanumeric1),
            |s: &str| Value::Keyword(s.to_string()),
        ),
    ))(input)
}

fn parse_rule(input: &str) -> nom::IResult<&str, Rule> {
    let (rest, selector) = simple_selector(input)?;
    let (rest, _) = super::util::css_sanitize(nom::character::complete::char('{'))(rest)?;
    let (rest, declarations) = nom::multi::separated_list0(
        super::util::ws(nom::character::complete::char(';')),
        declaration,
    )(rest)?;
    let (rest, _) =
        nom::combinator::opt(super::util::ws(nom::character::complete::char(';')))(rest)?;
    let (rest, _) = super::util::css_sanitize(nom::character::complete::char('}'))(rest)?;

    Ok((
        rest,
        Rule {
            selectors: vec![selector],
            declarations,
        },
    ))
}

fn rule_wrapper(input: &str) -> nom::IResult<&str, Item> {
    let (rest, r) = parse_rule(input)?;
    Ok((rest, Item::Rule(r)))
}

fn parse_at_rule_wrapper(input: &str) -> nom::IResult<&str, Item> {
    let (rest, ar) = parse_at_rule(input)?;
    Ok((rest, Item::AtRule(ar)))
}

fn stylesheet(input: &str) -> nom::IResult<&str, Stylesheet> {
    let (rest, items) =
        nom::multi::many0(nom::branch::alt((rule_wrapper, parse_at_rule_wrapper)))(input)?;

    Ok((rest, Stylesheet { items }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_selector() {
        let css = r"
        h1 { color: blue; }
        p { margin: 5px; }
        ";

        let (_, stylesheet) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet,
            Stylesheet {
                items: vec![
                    Item::Rule(Rule {
                        selectors: vec![Selector::Simple(SimpleSelector {
                            tag_name: Vec::from([Some("h1".to_string())]),
                        }),],
                        declarations: vec![Declaration {
                            name: "color".to_string(),
                            value: Value::Keyword("blue".to_string()),
                        },],
                    }),
                    Item::Rule(Rule {
                        selectors: vec![Selector::Simple(SimpleSelector {
                            tag_name: Vec::from([Some("p".to_string())]),
                        }),],
                        declarations: vec![Declaration {
                            name: "margin".to_string(),
                            value: Value::Length(5.0, Unit::Px),
                        },],
                    }),
                ],
            }
        );
    }

    #[test]
    fn test_hex_color() {
        let css = r"h1 { color: #ff0000; }";

        let (_, stylesheet1) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet1,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgb(255, 0, 0)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: #ff0000ff; }";

        let (_, stylesheet2) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet2,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgba(255, 0, 0, 1.0)),
                    },],
                }),],
            }
        );
    }

    #[test]
    fn test_rgb() {
        let css = r"h1 { color: rgb(255, 0, 0); }";
        let (_, stylesheet1) = stylesheet(css).unwrap();
        assert_eq!(
            stylesheet1,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgb(255, 0, 0)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: rgb(133,2,   55); }";
        let (_, stylesheet2) = stylesheet(css).unwrap();
        assert_eq!(
            stylesheet2,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgb(133, 2, 55)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: rgb(133, 2, 55, 0.5); }";
        let (_, stylesheet3) = stylesheet(css).unwrap();
        assert_eq!(
            stylesheet3,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgba(133, 2, 55, 0.5)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: rgb(133 2 55); }";
        let (_, stylesheet4) = stylesheet(css).unwrap();
        assert_eq!(
            stylesheet4,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgb(133, 2, 55)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: rgb(133 2 55 / 0.5); }";
        let (_, stylesheet5) = stylesheet(css).unwrap();
        assert_eq!(
            stylesheet5,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgba(133, 2, 55, 0.5)),
                    },],
                }),],
            }
        );
    }

    #[test]
    fn test_rgba() {
        let css = r"h1 { color: rgba(255, 0, 0, 0.5); }";

        let (_, stylesheet1) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet1,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgba(255, 0, 0, 0.5)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: rgba(133,2,   55, 0.5); }";

        let (_, stylesheet2) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet2,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgba(133, 2, 55, 0.5)),
                    },],
                }),],
            }
        );
    }

    #[test]
    fn test_hsl() {
        let css = r"h1 { color: hsl(0, 100%, 50%); }";

        let (_, stylesheet1) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet1,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Hsl(0, 100, 50)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: hsl(133, 2%,   55%); }";

        let (_, stylesheet2) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet2,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Hsl(133, 2, 55)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: hsl(133, 2%, 55%, 20); }";
        let (_, stylesheet3) = stylesheet(css).unwrap();
        assert_eq!(
            stylesheet3,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Hsla(133, 2, 55, 20.0)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: hsl(133 2% 55); }";
        let (_, stylesheet4) = stylesheet(css).unwrap();
        assert_eq!(
            stylesheet4,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Hsl(133, 2, 55)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: hsl(133 2% 55 / 0.5); }";
        let (_, stylesheet5) = stylesheet(css).unwrap();
        assert_eq!(
            stylesheet5,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Hsla(133, 2, 55, 0.5)),
                    },],
                }),],
            }
        );
    }

    #[test]
    fn test_hsla() {
        let css = r"h1 { color: hsla(0, 100%, 50%, 0.5); }";

        let (_, stylesheet1) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet1,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Hsla(0, 100, 50, 0.5)),
                    },],
                }),],
            }
        );

        let css = r"h1 { color: hsla(133, 2%,   55%, 0.5); }";

        let (_, stylesheet2) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet2,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Hsla(133, 2, 55, 0.5)),
                    },],
                }),],
            }
        );
    }

    #[test]
    fn test_length() {
        let css = r"h1 { margin: 1px; }";

        let (_, stylesheet1) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet1,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "margin".to_string(),
                        value: Value::Length(1.0, Unit::Px),
                    },],
                }),],
            }
        );

        let css = r"
        h1 { margin: 1em; }
        ";

        let (_, stylesheet2) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet2,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    }),],
                    declarations: vec![Declaration {
                        name: "margin".to_string(),
                        value: Value::Length(1.0, Unit::Em),
                    },],
                }),],
            }
        );
    }

    #[test]
    fn test_parse_media_at_rule() {
        let css = r"
        @media screen {
            h1 { color: red; }
            p { font-size: 14px; }
        }
        ";

        let expected = Stylesheet {
            items: vec![Item::AtRule(AtRule::Media(
                "screen".to_string(),
                vec![
                    Rule {
                        selectors: vec![Selector::Simple(SimpleSelector {
                            tag_name: Vec::from([Some("h1".to_string())]),
                        })],
                        declarations: vec![Declaration {
                            name: "color".to_string(),
                            value: Value::Keyword("red".to_string()),
                        }],
                    },
                    Rule {
                        selectors: vec![Selector::Simple(SimpleSelector {
                            tag_name: Vec::from([Some("p".to_string())]),
                        })],
                        declarations: vec![Declaration {
                            name: "font-size".to_string(),
                            value: Value::Length(14.0, Unit::Px),
                        }],
                    },
                ],
            ))],
        };

        assert_eq!(stylesheet(css), Ok(("", expected)));
    }

    #[test]
    fn test_css_comment() {
        let css = r#"
        /* this is a comment */
        h1 { color: red; }
        p { font-size: 14px; }
        "#;

        let expected = Stylesheet {
            items: vec![
                Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    })],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Keyword("red".to_string()),
                    }],
                }),
                Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("p".to_string())]),
                    })],
                    declarations: vec![Declaration {
                        name: "font-size".to_string(),
                        value: Value::Length(14.0, Unit::Px),
                    }],
                }),
            ],
        };

        assert_eq!(stylesheet(css), Ok(("", expected)));

        let css = r#"
        h1 { color: red; }
        /* this is a comment */
        p { font-size: 14px; }
        "#;

        let expected = Stylesheet {
            items: vec![
                Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("h1".to_string())]),
                    })],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Keyword("red".to_string()),
                    }],
                }),
                Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Vec::from([Some("p".to_string())]),
                    })],
                    declarations: vec![Declaration {
                        name: "font-size".to_string(),
                        value: Value::Length(14.0, Unit::Px),
                    }],
                }),
            ],
        };

        assert_eq!(stylesheet(css), Ok(("", expected)));
    }

    #[test]
    fn text_multiple_class() {
        let css = r#"
        h1, h2 { color: red; }
        "#;

        let expected = Stylesheet {
            items: vec![Item::Rule(Rule {
                selectors: vec![Selector::Simple(SimpleSelector {
                    tag_name: Vec::from([Some("h1".to_string()), Some("h2".to_string())]),
                })],
                declarations: vec![Declaration {
                    name: "color".to_string(),
                    value: Value::Keyword("red".to_string()),
                }],
            })],
        };
        assert_eq!(stylesheet(css), Ok(("", expected)));

        let css = r#"
        h1, h2, h3, .class { color: red; }
        "#;

        let expected = Stylesheet {
            items: vec![Item::Rule(Rule {
                selectors: vec![Selector::Simple(SimpleSelector {
                    tag_name: Vec::from([
                        Some("h1".to_string()),
                        Some("h2".to_string()),
                        Some("h3".to_string()),
                        Some(".class".to_string()),
                    ]),
                })],
                declarations: vec![Declaration {
                    name: "color".to_string(),
                    value: Value::Keyword("red".to_string()),
                }],
            })],
        };
        assert_eq!(stylesheet(css), Ok(("", expected)));
    }

    #[test]
    fn test_universal_selector() {
        let e = &crate::parser::dom::Element::new(
            "p".to_string(),
            [
                ("id".to_string(), "test".to_string()),
                ("class".to_string(), "testclass".to_string()),
            ]
            .iter()
            .cloned()
            .collect(),
            vec![],
        );

        assert!(crate::parser::css::Selector::Universal.matches(e).unwrap());
    }

    #[test]
    fn test_type_selector() {
        let e = &crate::parser::dom::Element::new(
            "p".to_string(),
            [
                ("id".to_string(), "test".to_string()),
                ("class".to_string(), "testclass".to_string()),
            ]
            .iter()
            .cloned()
            .collect(),
            vec![],
        );

        assert!((crate::parser::css::Selector::Type {
            tag_name: "p".into(),
        })
        .matches(e)
        .unwrap());
    }

    #[test]
    fn test_attribute_selector() {
        let e = &crate::parser::dom::Element::new(
            "p".to_string(),
            [
                ("id".to_string(), "test".to_string()),
                ("class".to_string(), "testclass".to_string()),
            ]
            .iter()
            .cloned()
            .collect(),
            vec![],
        );

        assert!((crate::parser::css::Selector::Attribute {
            tag_name: "p".into(),
            attribute: "id".into(),
            value: "test".into(),
            op: AttributeSelectorOp::Eq,
        })
        .matches(e)
        .unwrap());

        assert!(!(crate::parser::css::Selector::Attribute {
            tag_name: "p".into(),
            attribute: "id".into(),
            value: "invalid".into(),
            op: AttributeSelectorOp::Eq,
        })
        .matches(e)
        .unwrap());

        assert!(!(crate::parser::css::Selector::Attribute {
            tag_name: "p".into(),
            attribute: "invalid".into(),
            value: "test".into(),
            op: AttributeSelectorOp::Eq,
        })
        .matches(e)
        .unwrap());

        assert!(!(crate::parser::css::Selector::Attribute {
            tag_name: "invalid".into(),
            attribute: "id".into(),
            value: "test".into(),
            op: AttributeSelectorOp::Eq,
        })
        .matches(e)
        .unwrap());
    }

    #[test]
    fn test_class_selector() {
        let e = &crate::parser::dom::Element::new(
            "p".to_string(),
            [
                ("id".to_string(), "test".to_string()),
                ("class".to_string(), "testclass".to_string()),
            ]
            .iter()
            .cloned()
            .collect(),
            vec![],
        );

        assert!((crate::parser::css::Selector::Class {
            class_name: "testclass".into(),
        })
        .matches(e)
        .unwrap());
    }
}
