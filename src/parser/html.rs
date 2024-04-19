fn attribute(input: &str) -> nom::IResult<&str, (String, String)> {
    nom::sequence::tuple((
        nom::combinator::recognize(nom::multi::many1(nom::character::complete::alphanumeric1)),
        nom::character::complete::space0,
        nom::character::complete::char('='),
        nom::character::complete::space0,
        nom::sequence::delimited(
            nom::character::complete::char('"'),
            nom::combinator::recognize(nom::multi::many1(nom::character::complete::alphanumeric1)),
            nom::character::complete::char('"'),
        ),
    ))(input)
    .map(|(rest, (key, _, _, _, value))| (rest, (key.to_string(), value.to_string())))
}

fn attributes(input: &str) -> nom::IResult<&str, crate::parser::dom::AttrMap> {
    nom::multi::many0(nom::sequence::tuple((
        nom::character::complete::space0,
        attribute,
    )))(input)
    .map(|(rest, pairs)| {
        let mut map = crate::parser::dom::AttrMap::new();
        for (_, (key, value)) in pairs {
            map.insert(key, value);
        }
        (rest, map)
    })
}

fn tag_open(input: &str) -> nom::IResult<&str, (String, crate::parser::dom::AttrMap)> {
    nom::sequence::tuple((
        nom::character::complete::char('<'),
        nom::combinator::recognize(nom::multi::many1(nom::character::complete::alphanumeric1)),
        attributes,
        nom::character::complete::char('>'),
    ))(input)
    .map(|(rest, (_, tag_name, attributes, _))| (rest, (tag_name.to_string(), attributes)))
}

fn tag_close(input: &str) -> nom::IResult<&str, &str> {
    nom::sequence::tuple((
        nom::character::complete::char('<'),
        nom::character::complete::char('/'),
        nom::combinator::recognize(nom::multi::many1(nom::character::complete::alphanumeric1)),
        nom::character::complete::char('>'),
    ))(input)
    .map(|(rest, (_, _, tag_name, _))| (rest, tag_name))
}

fn text(input: &str) -> nom::IResult<&str, Box<crate::parser::dom::Node>> {
    nom::combinator::map(
        nom::combinator::recognize(nom::multi::many1(nom::character::complete::alphanumeric1)),
        |text: &str| -> Box<crate::parser::dom::Node> {
            Box::new(*crate::parser::dom::Text::new(text.to_string()))
        },
    )(input)
}

fn nodes(input: &str) -> nom::IResult<&str, Vec<Box<crate::parser::dom::Node>>> {
    nom::multi::many0(combine_parser)(input)
}

fn combine_parser(input: &str) -> nom::IResult<&str, Box<crate::parser::dom::Node>> {
    nom::branch::alt((element_parser, text_parser))(input)
}

fn element_parser(input: &str) -> nom::IResult<&str, Box<crate::parser::dom::Node>> {
    element(input)
}

fn text_parser(input: &str) -> nom::IResult<&str, Box<crate::parser::dom::Node>> {
    text(input)
}

fn element(input: &str) -> nom::IResult<&str, Box<crate::parser::dom::Node>> {
    let (rest, (open_tag_name, attributes)) = tag_open(input)?;
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
            Box::new(*crate::parser::dom::Element::new(
                open_tag_name,
                attributes,
                children,
            )),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            Ok(("", ("div".to_string(), expected_map)))
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
                Box::new(*crate::parser::dom::Element::new(
                    "div".to_string(),
                    crate::parser::dom::AttrMap::new(),
                    vec![Box::new(*crate::parser::dom::Text::new("test".to_string()))],
                ))
            ))
        );

        assert_eq!(
            element("<div id=\"test\">test</div>"),
            Ok((
                "",
                Box::new(*crate::parser::dom::Element::new(
                    "div".to_string(),
                    {
                        let mut map = crate::parser::dom::AttrMap::new();
                        map.insert("id".to_string(), "test".to_string());
                        map
                    },
                    vec![Box::new(*crate::parser::dom::Text::new("test".to_string()))],
                ))
            ))
        );
    }
}
