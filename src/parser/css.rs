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
    let (rest, tag_name) = nom::combinator::opt(super::util::css_sanitize(
        nom::character::complete::alphanumeric1,
    ))(input)?;

    Ok((
        rest,
        Selector::Simple(SimpleSelector {
            tag_name: tag_name.map(String::from),
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
    ))(rest)?;

    match unit {
        "px" => Ok((rest, Value::Length(number, Unit::Px))),
        "em" => Ok((rest, Value::Length(number, Unit::Em))),
        _ => unreachable!(),
    }
}

fn parse_color(input: &str) -> nom::IResult<&str, Value> {
    let (rest, tag) = nom::branch::alt((
        super::util::ws(nom::bytes::complete::tag("#")),
        super::util::ws(nom::bytes::complete::tag("rgba")),
        super::util::ws(nom::bytes::complete::tag("rgb")),
        super::util::ws(nom::bytes::complete::tag("hsla")),
        super::util::ws(nom::bytes::complete::tag("hsl")),
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

            Ok((rest, Value::Color(Color::Rgb(r, g, b))))
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
            let (rest, _) = super::util::ws(nom::character::complete::char(')'))(rest)?;

            Ok((rest, Value::Color(Color::Rgb(r, g, b))))
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
            let (rest, _) = super::util::ws(nom::character::complete::char(')'))(rest)?;

            Ok((rest, Value::Color(Color::Hsl(h, s, l))))
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
                        tag_name: Some("h1".to_string()),
                    })],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Keyword("red".to_string()),
                    }],
                }),
                Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Some("p".to_string()),
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
                        tag_name: Some("h1".to_string()),
                    })],
                    declarations: vec![Declaration {
                        name: "color".to_string(),
                        value: Value::Keyword("red".to_string()),
                    }],
                }),
                Item::Rule(Rule {
                    selectors: vec![Selector::Simple(SimpleSelector {
                        tag_name: Some("p".to_string()),
                    })],
                    declarations: vec![Declaration {
                        name: "font-size".to_string(),
                        value: Value::Length(14.0, Unit::Px),
                    }],
                }),
            ],
        };
    }
}
