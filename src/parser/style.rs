#![allow(dead_code)]

pub type PropMap = std::collections::HashMap<String, super::css::Value>;

#[derive(Debug, PartialEq)]
pub struct StyledNode<'a> {
    pub node: &'a super::dom::Node,
    pub property: PropMap,
    pub children: Vec<StyledNode<'a>>,
}

pub enum Display {
    Inline,
    Block,
    None,
}

impl<'a> StyledNode<'a> {
    pub fn new(
        node: &'a super::dom::Node,
        property: PropMap,
        children: Vec<StyledNode<'a>>,
    ) -> Self {
        Self {
            node,
            property,
            children,
        }
    }

    pub fn display(&self) -> Display {
        self.property
            .get("display")
            .map(|v| match v {
                super::css::Value::Keyword(s) => match s.as_str() {
                    "block" => Display::Block,
                    "none" => Display::None,
                    _ => Display::Inline,
                },
                _ => Display::Inline,
            })
            .unwrap_or(Display::Inline)
    }
}

pub fn style_tree(root: &super::dom::Node) -> StyledNode<'_> {
    StyledNode::new(root, Default::default(), vec![])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_universal_selector_behaviour() {
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
}
