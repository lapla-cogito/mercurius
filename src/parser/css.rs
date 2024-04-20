#[derive(Debug, PartialEq)]
pub struct Stylesheet {
    pub rules: Vec<Rule>,
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
}

// Utility function to parse whitespace
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O>
where
    F: Fn(&'a str) -> nom::IResult<&'a str, O>,
{
    nom::sequence::delimited(
        nom::character::complete::space0,
        inner,
        nom::character::complete::space0,
    )
}

fn simple_selector(input: &str) -> nom::IResult<&str, Selector> {
    let (input, tag_name) =
        nom::combinator::opt(ws(nom::character::complete::alphanumeric1))(input)?;
    Ok((
        input,
        Selector::Simple(SimpleSelector {
            tag_name: tag_name.map(String::from),
        }),
    ))
}

fn declaration(input: &str) -> nom::IResult<&str, Declaration> {
    let (input, name) = ws(nom::bytes::complete::take_while1(|c: char| {
        c.is_alphanumeric() || c == '-'
    }))(input)?;
    let (input, _) = ws(nom::bytes::complete::tag(":"))(input)?;
    let (input, value) = ws(nom::bytes::complete::take_while1(|c: char| {
        c.is_alphanumeric()
    }))(input)?;
    let (input, _) = ws(nom::bytes::complete::tag(";"))(input)?;
    Ok((
        input,
        Declaration {
            name: name.to_string(),
            value: Value::Keyword(value.to_string()),
        },
    ))
}

fn rule(input: &str) -> nom::IResult<&str, Rule> {
    let (input, selector) = simple_selector(input)?;
    let (input, _) = ws(nom::character::complete::char('{'))(input)?;
    let (input, declarations) =
        nom::multi::separated_list0(ws(nom::character::complete::char(';')), declaration)(input)?;
    let (input, _) = ws(nom::character::complete::char('}'))(input)?;
    Ok((
        input,
        Rule {
            selectors: vec![selector],
            declarations,
        },
    ))
}

fn stylesheet(input: &str) -> nom::IResult<&str, Stylesheet> {
    let (input, rules) =
        nom::multi::separated_list1(ws(nom::character::complete::char('\n')), rule)(input)?;
    Ok((input, Stylesheet { rules }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_selector() {
        let css = r"h1 { color: blue; }
        p { margin: 5px; }";

        let (_, stylesheet) = stylesheet(css).unwrap();

        assert_eq!(
            stylesheet,
            Stylesheet {
                rules: vec![
                    Rule {
                        selectors: vec![Selector::Simple(SimpleSelector {
                            tag_name: Some("h1".to_string()),
                        }),],
                        declarations: vec![Declaration {
                            name: "color".to_string(),
                            value: Value::Keyword("blue".to_string()),
                        },],
                    },
                    Rule {
                        selectors: vec![Selector::Simple(SimpleSelector {
                            tag_name: Some("p".to_string()),
                        }),],
                        declarations: vec![Declaration {
                            name: "margin".to_string(),
                            value: Value::Keyword("5px".to_string()),
                        },],
                    },
                ],
            }
        );
    }
}
