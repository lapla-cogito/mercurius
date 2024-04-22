pub fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O>
where
    F: Fn(&'a str) -> nom::IResult<&'a str, O>,
{
    nom::sequence::delimited(
        nom::character::complete::multispace0,
        inner,
        nom::character::complete::multispace0,
    )
}

pub fn html_sanitize<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O>
where
    F: Fn(&'a str) -> nom::IResult<&'a str, O>,
{
    nom::sequence::delimited(
        nom::combinator::opt(nom::branch::alt((
            ws(html_comment),
            nom::character::complete::multispace1,
        ))),
        inner,
        nom::combinator::opt(nom::branch::alt((
            ws(html_comment),
            nom::character::complete::multispace1,
        ))),
    )
}

pub fn css_sanitize<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O>
where
    F: Fn(&'a str) -> nom::IResult<&'a str, O>,
{
    nom::sequence::delimited(
        nom::combinator::opt(nom::branch::alt((
            ws(css_comment),
            nom::character::complete::multispace1,
        ))),
        inner,
        nom::combinator::opt(nom::branch::alt((
            ws(css_comment),
            nom::character::complete::multispace1,
        ))),
    )
}

fn html_comment(input: &str) -> nom::IResult<&str, &str> {
    nom::sequence::tuple((
        nom::bytes::complete::tag("<!--"),
        nom::multi::many0(nom::character::complete::none_of("-")),
        nom::bytes::complete::tag("-->"),
    ))(input)
    .map(|(rest, (_, _, _))| (rest, ""))
}

fn css_comment(input: &str) -> nom::IResult<&str, &str> {
    nom::sequence::tuple((
        nom::bytes::complete::tag("/*"),
        nom::multi::many0(nom::character::complete::none_of("*")),
        nom::bytes::complete::tag("*/"),
    ))(input)
    .map(|(rest, (_, _, _))| (rest, ""))
}
