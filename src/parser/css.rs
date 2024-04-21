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

#[derive(Debug, PartialEq)]
pub enum Selector {
    Simple(SimpleSelector),
}

#[derive(Debug, PartialEq)]
pub struct SimpleSelector {
    pub tag_name: Option<String>,
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
}

#[derive(Debug, PartialEq)]
pub enum AtRule {
    Media(String, Vec<Rule>),
}

fn simple_selector(input: &str) -> nom::IResult<&str, Selector> {
    let (input, tag_name) = nom::combinator::opt(crate::parser::util::ws(
        nom::character::complete::alphanumeric1,
    ))(input)?;

    Ok((
        input,
        Selector::Simple(SimpleSelector {
            tag_name: tag_name.map(String::from),
        }),
    ))
}

fn declaration(input: &str) -> nom::IResult<&str, Declaration> {
    let (input, name) = crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
        c.is_alphanumeric() || c == '-'
    }))(input)?;
    let (input, _) = crate::parser::util::ws(nom::bytes::complete::tag(":"))(input)?;
    let (input, value) = value(input)?;
    let (input, _) = crate::parser::util::ws(nom::bytes::complete::tag(";"))(input)?;

    Ok((
        input,
        Declaration {
            name: name.to_string(),
            value,
        },
    ))
}

fn parse_length(input: &str) -> nom::IResult<&str, Value> {
    let (input, number) = nom::combinator::map_res(
        nom::combinator::recognize(nom::sequence::pair(
            nom::character::complete::digit1,
            nom::combinator::opt(nom::sequence::pair(
                nom::character::complete::char('.'),
                nom::character::complete::digit1,
            )),
        )),
        |s: &str| s.parse::<f32>(),
    )(input)?;

    let (input, unit) = nom::branch::alt((
        nom::bytes::complete::tag("px"),
        nom::bytes::complete::tag("em"),
    ))(input)?;

    match unit {
        "px" => Ok((input, Value::Length(number, Unit::Px))),
        "em" => Ok((input, Value::Length(number, Unit::Em))),
        _ => unreachable!(),
    }
}

fn parse_color(input: &str) -> nom::IResult<&str, Value> {
    let (input, tag) = nom::branch::alt((
        crate::parser::util::ws(nom::bytes::complete::tag("#")),
        crate::parser::util::ws(nom::bytes::complete::tag("rgba")),
        crate::parser::util::ws(nom::bytes::complete::tag("rgb")),
        crate::parser::util::ws(nom::bytes::complete::tag("hsla")),
        crate::parser::util::ws(nom::bytes::complete::tag("hsl")),
    ))(input)?;

    match tag {
        "#" => {
            let (input, r) =
                nom::combinator::map_res(nom::bytes::complete::take(2usize), |s: &str| {
                    u8::from_str_radix(s, 16)
                })(input)?;
            let (input, g) =
                nom::combinator::map_res(nom::bytes::complete::take(2usize), |s: &str| {
                    u8::from_str_radix(s, 16)
                })(input)?;
            let (input, b) =
                nom::combinator::map_res(nom::bytes::complete::take(2usize), |s: &str| {
                    u8::from_str_radix(s, 16)
                })(input)?;

            Ok((input, Value::Color(Color::Rgb(r, g, b))))
        }
        "rgba" => {
            let (input, _) = crate::parser::util::ws(nom::character::complete::char('('))(input)?;
            let (input, r) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, g) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, b) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, a) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit() || c == '.'
                })),
                |s: &str| f32::from_str(s),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(')'))(input)?;

            Ok((input, Value::Color(Color::Rgba(r, g, b, a))))
        }
        "rgb" => {
            let (input, _) = crate::parser::util::ws(nom::character::complete::char('('))(input)?;
            let (input, r) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, g) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, b) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(')'))(input)?;

            Ok((input, Value::Color(Color::Rgb(r, g, b))))
        }
        "hsla" => {
            let (input, _) = crate::parser::util::ws(nom::character::complete::char('('))(input)?;
            let (input, h) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, s) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char('%'))(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, l) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char('%'))(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, a) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit() || c == '.'
                })),
                |s: &str| f32::from_str(s),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(')'))(input)?;

            Ok((input, Value::Color(Color::Hsla(h, s, l, a))))
        }
        "hsl" => {
            let (input, _) = crate::parser::util::ws(nom::character::complete::char('('))(input)?;
            let (input, h) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, s) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char('%'))(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(','))(input)?;
            let (input, l) = nom::combinator::map_res(
                crate::parser::util::ws(nom::bytes::complete::take_while1(|c: char| {
                    c.is_ascii_digit()
                })),
                |s: &str| s.parse::<u8>(),
            )(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char('%'))(input)?;
            let (input, _) = crate::parser::util::ws(nom::character::complete::char(')'))(input)?;

            Ok((input, Value::Color(Color::Hsl(h, s, l))))
        }
        _ => unreachable!(),
    }
}

fn parse_at_rule(input: &str) -> nom::IResult<&str, AtRule> {
    let (input, _) = crate::parser::util::ws(nom::bytes::complete::tag("@media"))(input)?;
    let (input, media) = crate::parser::util::ws(nom::bytes::streaming::take_until("{"))(input)?;
    let (input, _) = crate::parser::util::ws(nom::character::complete::char('{'))(input)?;
    let (input, rules) = nom::multi::many0(crate::parser::util::ws(parse_rule))(input)?;
    let (input, _) = crate::parser::util::ws(nom::character::complete::char('}'))(input)?;

    Ok((input, AtRule::Media(media.trim().to_string(), rules)))
}

fn value(input: &str) -> nom::IResult<&str, Value> {
    nom::branch::alt((
        parse_length,
        parse_color,
        nom::combinator::map(
            crate::parser::util::ws(nom::character::complete::alphanumeric1),
            |s: &str| Value::Keyword(s.to_string()),
        ),
    ))(input)
}

fn parse_rule(input: &str) -> nom::IResult<&str, Rule> {
    let (input, selector) = simple_selector(input)?;
    let (input, _) = crate::parser::util::ws(nom::character::complete::char('{'))(input)?;
    let (input, declarations) = nom::multi::separated_list0(
        crate::parser::util::ws(nom::character::complete::char(';')),
        declaration,
    )(input)?;
    let (input, _) =
        nom::combinator::opt(crate::parser::util::ws(nom::character::complete::char(';')))(input)?;
    let (input, _) = crate::parser::util::ws(nom::character::complete::char('}'))(input)?;

    Ok((
        input,
        Rule {
            selectors: vec![selector],
            declarations,
        },
    ))
}

fn rule_wrapper(input: &str) -> nom::IResult<&str, Item> {
    let (input, r) = parse_rule(input)?;
    Ok((input, Item::Rule(r)))
}

fn parse_at_rule_wrapper(input: &str) -> nom::IResult<&str, Item> {
    let (input, ar) = parse_at_rule(input)?;
    Ok((input, Item::AtRule(ar)))
}

fn stylesheet(input: &str) -> nom::IResult<&str, Stylesheet> {
    let (input, items) =
        nom::multi::many0(nom::branch::alt((rule_wrapper, parse_at_rule_wrapper)))(input)?;

    Ok((input, Stylesheet { items }))
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
                            tag_name: Some("h1".to_string()),
                        }),],
                        declarations: vec![Declaration {
                            name: "color".to_string(),
                            value: Value::Keyword("blue".to_string()),
                        },],
                    }),
                    Item::Rule(Rule {
                        selectors: vec![Selector::Simple(SimpleSelector {
                            tag_name: Some("p".to_string()),
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
    fn test_rgb() {
        let css = r"h1 { color: rgb(255, 0, 0); }";

        let (_, stylesheet1) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet1,
            Stylesheet {
                items: vec![crate::parser::css::Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Some("h1".to_string()),
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
                        tag_name: Some("h1".to_string()),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Rgb(133, 2, 55)),
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
                        tag_name: Some("h1".to_string()),
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
                        tag_name: Some("h1".to_string()),
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
                        tag_name: Some("h1".to_string()),
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
                        tag_name: Some("h1".to_string()),
                    }),],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Color(Color::Hsl(133, 2, 55)),
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
                        tag_name: Some("h1".to_string()),
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
                        tag_name: Some("h1".to_string()),
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
                        tag_name: Some("h1".to_string()),
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
                        tag_name: Some("h1".to_string()),
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
                            tag_name: Some("h1".to_string()),
                        })],
                        declarations: vec![Declaration {
                            name: "color".to_string(),
                            value: Value::Keyword("red".to_string()),
                        }],
                    },
                    Rule {
                        selectors: vec![Selector::Simple(SimpleSelector {
                            tag_name: Some("p".to_string()),
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
}
