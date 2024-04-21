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
