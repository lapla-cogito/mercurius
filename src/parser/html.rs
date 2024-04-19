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
}
