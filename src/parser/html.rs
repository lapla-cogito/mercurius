#![allow(unused)]

fn attribute(input: &str) -> nom::IResult<&str, (String, String)> {
    nom::sequence::tuple((
        nom::combinator::recognize(nom::multi::many1(nom::character::complete::alphanumeric1)),
        super::util::ws(nom::character::complete::char('=')),
        nom::sequence::delimited(
            nom::character::complete::char('"'),
            nom::combinator::recognize(nom::multi::many1(nom::character::complete::alphanumeric1)),
            nom::character::complete::char('"'),
        ),
    ))(input)
    .map(|(rest, (key, _, value))| (rest, (key.to_string(), value.to_string())))
}

fn attributes(input: &str) -> nom::IResult<&str, super::dom::AttrMap> {
    nom::multi::many0(nom::sequence::tuple((
        nom::character::complete::space0,
        attribute,
    )))(input)
    .map(|(rest, pairs)| {
        let mut map = super::dom::AttrMap::new();
        for (_, (key, value)) in pairs {
            map.insert(key, value);
        }
        (rest, map)
    })
}

fn tag_open(input: &str) -> nom::IResult<&str, (String, super::dom::AttrMap, bool)> {
    let (rest, _) = super::util::html_sanitize(nom::character::complete::char('<'))(input)?;
    let (rest, tag_name) = nom::combinator::recognize(nom::multi::many1(
        nom::character::complete::alphanumeric1,
    ))(rest)?;
    let (rest, attributes) = attributes(rest)?;
    let (rest, self_closing) = nom::combinator::opt(super::util::html_sanitize(
        nom::character::complete::char('/'),
    ))(rest)?;
    let (rest, _) = super::util::html_sanitize(nom::character::complete::char('>'))(rest)?;

    Ok((
        rest,
        (tag_name.to_string(), attributes, self_closing.is_some()),
    ))
}

fn tag_close(input: &str) -> nom::IResult<&str, &str> {
    nom::sequence::tuple((
        super::util::html_sanitize(nom::character::complete::char('<')),
        nom::character::complete::char('/'),
        nom::combinator::recognize(nom::multi::many1(nom::character::complete::alphanumeric1)),
        super::util::html_sanitize(nom::character::complete::char('>')),
    ))(input)
    .map(|(rest, (_, _, tag_name, _))| (rest, tag_name))
}

fn text(input: &str) -> nom::IResult<&str, super::dom::Node> {
    nom::combinator::map(
        nom::combinator::recognize(nom::multi::many1(nom::character::complete::alphanumeric1)),
        |text: &str| -> super::dom::Node { *super::dom::Text::new(text.to_string()) },
    )(input)
}

fn nodes(input: &str) -> nom::IResult<&str, Vec<super::dom::Node>> {
    nom::multi::many0(combine_parser)(input)
}

fn combine_parser(input: &str) -> nom::IResult<&str, super::dom::Node> {
    nom::branch::alt((element_parser, text_parser))(input)
}

fn element_parser(input: &str) -> nom::IResult<&str, super::dom::Node> {
    element(input)
}

fn text_parser(input: &str) -> nom::IResult<&str, super::dom::Node> {
    text(input)
}

fn element(input: &str) -> nom::IResult<&str, super::dom::Node> {
    let (rest, (open_tag_name, attributes, self_closing)) = tag_open(input)?;
    if self_closing {
        return Ok((
            rest,
            super::dom::Element::new(open_tag_name, attributes, vec![]),
        ));
    }

    let (rest, children) = nodes(rest)?;
    let (rest, close_tag_name) = tag_close(rest)?;

    if open_tag_name != close_tag_name {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    } else {
        Ok((
            rest,
            super::dom::Element::new(open_tag_name, attributes, children),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text() {
        assert_eq!(
            text("test"),
            Ok(("", *crate::parser::dom::Text::new("test".to_string())))
        );
    }

    #[test]
    fn test_attribute() {
        assert_eq!(
            attribute("id=\"test\""),
            Ok(("", ("id".to_string(), "test".to_string())))
        );
        assert_eq!(
            attribute("id = \"test\""),
            Ok(("", ("id".to_string(), "test".to_string())))
        );
        assert_eq!(
            attribute("id1 = \"test\""),
            Ok(("", ("id1".to_string(), "test".to_string())))
        );
    }

    #[test]
    fn test_attributes() {
        let mut expected_map = crate::parser::dom::AttrMap::new();
        expected_map.insert("test".to_string(), "foobar".to_string());
        expected_map.insert("abc".to_string(), "def".to_string());
        assert_eq!(
            attributes("test=\"foobar\" abc=\"def\""),
            Ok(("", expected_map))
        );
    }

    #[test]
    fn test_tag_open() {
        let mut expected_map = crate::parser::dom::AttrMap::new();
        expected_map.insert("test".to_string(), "foobar".to_string());
        expected_map.insert("abc".to_string(), "def".to_string());
        assert_eq!(
            tag_open("<div test=\"foobar\" abc=\"def\">"),
            Ok(("", ("div".to_string(), expected_map, false)))
        );
    }

    #[test]
    fn test_tag_close() {
        assert_eq!(tag_close("</div>"), Ok(("", "div")));
    }

    #[test]
    fn test_element() {
        assert_eq!(
            element("<div>test</div>"),
            Ok((
                "",
                crate::parser::dom::Element::new(
                    "div".to_string(),
                    crate::parser::dom::AttrMap::new(),
                    vec![*crate::parser::dom::Text::new("test".to_string())],
                )
            ))
        );

        assert_eq!(
            element(
                r#"<div id="test">
                    test
                </div>
            "#
            ),
            Ok((
                "",
                crate::parser::dom::Element::new(
                    "div".to_string(),
                    {
                        let mut map = crate::parser::dom::AttrMap::new();
                        map.insert("id".to_string(), "test".to_string());
                        map
                    },
                    vec![*crate::parser::dom::Text::new("test".to_string())],
                )
            ))
        );
    }

    #[test]
    fn test_self_closing() {
        let html = r#"<div id="test" />"#;

        let expected = crate::parser::dom::Element::new(
            "div".to_string(),
            {
                let mut map = crate::parser::dom::AttrMap::new();
                map.insert("id".to_string(), "test".to_string());
                map
            },
            vec![],
        );

        assert_eq!(element(html), Ok(("", expected)));
    }

    #[test]
    fn test_html() {
        let html = r#"
        <div id="test">
            test
        </div>
        "#;

        let expected = crate::parser::dom::Element::new(
            "div".to_string(),
            {
                let mut map = crate::parser::dom::AttrMap::new();
                map.insert("id".to_string(), "test".to_string());
                map
            },
            vec![*crate::parser::dom::Text::new("test".to_string())],
        );

        assert_eq!(element(html), Ok(("", expected)));
    }

    #[test]
    fn test_incompatible_tag() {
        let html = r#"
        <div id="test">
            test
        </span>
        "#;

        assert_eq!(
            element(html),
            Err(nom::Err::Error(nom::error::Error::new(
                html,
                nom::error::ErrorKind::Tag
            )))
        );
    }

    #[test]
    fn test_html_comment() {
        let html = r#"
        <!-- this is a comment -->
        <div id="test">
            test
        </div>
        "#;

        let expected = crate::parser::dom::Element::new(
            "div".to_string(),
            {
                let mut map = crate::parser::dom::AttrMap::new();
                map.insert("id".to_string(), "test".to_string());
                map
            },
            vec![*crate::parser::dom::Text::new("test".to_string())],
        );

        assert_eq!(element(html), Ok(("", expected)));

        let html = r#"
        <!-- this is a comment -->
        <div id="test">
            test
        </div>
        <!-- this is another comment -->
        "#;

        let expected = crate::parser::dom::Element::new(
            "div".to_string(),
            {
                let mut map = crate::parser::dom::AttrMap::new();
                map.insert("id".to_string(), "test".to_string());
                map
            },
            vec![*crate::parser::dom::Text::new("test".to_string())],
        );

        assert_eq!(element(html), Ok(("", expected)));
    }

    #[test]
    fn test_html_comment_in_text() {
        let html = r#"
        <div id="test">
            <!-- this is a comment -->
            test
        </div>
        "#;

        let expected = crate::parser::dom::Element::new(
            "div".to_string(),
            {
                let mut map = crate::parser::dom::AttrMap::new();
                map.insert("id".to_string(), "test".to_string());
                map
            },
            vec![*crate::parser::dom::Text::new("test".to_string())],
        );

        assert_eq!(element(html), Ok(("", expected)));
    }
}
