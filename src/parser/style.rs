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

pub fn to_styled_node<'a>(
    node: &'a super::dom::Node,
    stylesheet: &super::css::Stylesheet,
) -> Option<StyledNode<'a>> {
    let mut property = PropMap::new();

    for item in stylesheet.items.iter() {
        match item {
            super::css::Item::Rule(rule) => {
                for selector in rule.selectors.iter() {
                    match selector {
                        super::css::Selector::Universal => {
                            for declaration in rule.declarations.iter() {
                                property
                                    .insert(declaration.name.clone(), declaration.value.clone());
                            }
                        }
                        super::css::Selector::Type { tag_name } => match &node.node_type {
                            super::dom::NodeType::Element(element) => {
                                if tag_name == &element.tag_name {
                                    for declaration in rule.declarations.iter() {
                                        property.insert(
                                            declaration.name.clone(),
                                            declaration.value.clone(),
                                        );
                                    }
                                }
                            }
                            super::dom::NodeType::Text(_) => todo!(),
                        },
                        super::css::Selector::Attribute {
                            tag_name,
                            op,
                            attribute,
                            value,
                        } => match &node.node_type {
                            super::dom::NodeType::Element(element) => {
                                if tag_name == &element.tag_name {
                                    match op {
                                        super::css::AttributeSelectorOp::Eq => {
                                            if element.attributes.get(attribute) == Some(value) {
                                                for declaration in rule.declarations.iter() {
                                                    property.insert(
                                                        declaration.name.clone(),
                                                        declaration.value.clone(),
                                                    );
                                                }
                                            }
                                        }
                                        super::css::AttributeSelectorOp::Contain => {
                                            if element
                                                .attributes
                                                .get(attribute)
                                                .map_or(false, |v| v.contains(value))
                                            {
                                                for declaration in rule.declarations.iter() {
                                                    property.insert(
                                                        declaration.name.clone(),
                                                        declaration.value.clone(),
                                                    );
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            super::dom::NodeType::Text(_) => todo!(),
                        },
                        super::css::Selector::Class { class_name } => match &node.node_type {
                            super::dom::NodeType::Element(element) => {
                                if element.attributes.get("class") == Some(class_name) {
                                    for declaration in rule.declarations.iter() {
                                        property.insert(
                                            declaration.name.clone(),
                                            declaration.value.clone(),
                                        );
                                    }
                                }
                            }
                            super::dom::NodeType::Text(_) => {
                                todo!()
                            }
                        },
                        _ => unreachable!(),
                    }
                }
            }
            super::css::Item::AtRule(_) => todo!(),
        }
    }

    if property.get("display") == Some(&super::css::Value::Keyword("none".to_string())) {
        return None;
    }

    let children = node
        .children
        .iter()
        .filter_map(|c| to_styled_node(c, stylesheet))
        .collect::<Vec<_>>();

    Some(StyledNode {
        node,
        property,
        children,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_styled_node() {
        let e = &crate::parser::dom::Element::new(
            "p".to_string(),
            [("id".to_string(), "test".to_string())]
                .iter()
                .cloned()
                .collect(),
            vec![],
        );

        let testcases = vec![
            (
                // * { display: block; }
                crate::parser::css::Stylesheet::new(vec![crate::parser::css::Item::Rule(
                    crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Universal],
                        declarations: vec![crate::parser::css::Declaration {
                            name: "display".to_string(),
                            value: crate::parser::css::Value::Keyword("block".to_string()),
                        }],
                    },
                )]),
                vec![(
                    "display".to_string(),
                    crate::parser::css::Value::Keyword("block".to_string()),
                )],
            ),
            (
                // div { display: block; }
                crate::parser::css::Stylesheet::new(vec![crate::parser::css::Item::Rule(
                    crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Type {
                            tag_name: "div".into(),
                        }],
                        declarations: vec![crate::parser::css::Declaration {
                            name: "display".into(),
                            value: crate::parser::css::Value::Keyword("block".to_string()),
                        }],
                    },
                )]),
                vec![],
            ),
            (
                // * { display: block; }
                // div { display: inline; }
                crate::parser::css::Stylesheet::new(vec![
                    crate::parser::css::Item::Rule(crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Universal],
                        declarations: vec![crate::parser::css::Declaration {
                            name: "display".to_string(),
                            value: crate::parser::css::Value::Keyword("block".into()),
                        }],
                    }),
                    crate::parser::css::Item::Rule(crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Type {
                            tag_name: "div".into(),
                        }],
                        declarations: vec![crate::parser::css::Declaration {
                            name: "display".into(),
                            value: crate::parser::css::Value::Keyword("inline".into()),
                        }],
                    }),
                ]),
                vec![(
                    "display".to_string(),
                    crate::parser::css::Value::Keyword("block".to_string()),
                )],
            ),
            (
                // * { display: block; }
                // p { display: inline; testname: testvalue; }
                crate::parser::css::Stylesheet::new(vec![
                    crate::parser::css::Item::Rule(crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Universal],
                        declarations: vec![crate::parser::css::Declaration {
                            name: "display".to_string(),
                            value: crate::parser::css::Value::Keyword("block".into()),
                        }],
                    }),
                    crate::parser::css::Item::Rule(crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Type {
                            tag_name: "p".into(),
                        }],
                        declarations: vec![
                            crate::parser::css::Declaration {
                                name: "display".into(),
                                value: crate::parser::css::Value::Keyword("inline".into()),
                            },
                            crate::parser::css::Declaration {
                                name: "testname".into(),
                                value: crate::parser::css::Value::Keyword("testvalue".into()),
                            },
                        ],
                    }),
                ]),
                vec![
                    (
                        "display".into(),
                        crate::parser::css::Value::Keyword("inline".into()),
                    ),
                    (
                        "testname".into(),
                        crate::parser::css::Value::Keyword("testvalue".into()),
                    ),
                ],
            ),
            (
                // * { display: block; }
                // p[id=hello] { testname: testvalue; }
                crate::parser::css::Stylesheet::new(vec![
                    crate::parser::css::Item::Rule(crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Universal],
                        declarations: vec![crate::parser::css::Declaration {
                            name: "display".to_string(),
                            value: crate::parser::css::Value::Keyword("block".into()),
                        }],
                    }),
                    crate::parser::css::Item::Rule(crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Attribute {
                            tag_name: "p".into(),
                            op: crate::parser::css::AttributeSelectorOp::Eq,
                            attribute: "id".into(),
                            value: "hello".into(),
                        }],
                        declarations: vec![crate::parser::css::Declaration {
                            name: "testname".into(),
                            value: crate::parser::css::Value::Keyword("testvalue".into()),
                        }],
                    }),
                ]),
                vec![(
                    "display".into(),
                    crate::parser::css::Value::Keyword("block".into()),
                )],
            ),
            (
                // * { display: block; }
                // p[id=hello] { testname: testvalue; }
                crate::parser::css::Stylesheet::new(vec![
                    crate::parser::css::Item::Rule(crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Universal],
                        declarations: vec![crate::parser::css::Declaration {
                            name: "display".to_string(),
                            value: crate::parser::css::Value::Keyword("block".into()),
                        }],
                    }),
                    crate::parser::css::Item::Rule(crate::parser::css::Rule {
                        selectors: vec![crate::parser::css::Selector::Attribute {
                            tag_name: "p".into(),
                            op: crate::parser::css::AttributeSelectorOp::Eq,
                            attribute: "id".into(),
                            value: "test".into(),
                        }],
                        declarations: vec![crate::parser::css::Declaration {
                            name: "testname".into(),
                            value: crate::parser::css::Value::Keyword("testvalue".into()),
                        }],
                    }),
                ]),
                vec![
                    (
                        "display".into(),
                        crate::parser::css::Value::Keyword("block".into()),
                    ),
                    (
                        "testname".into(),
                        crate::parser::css::Value::Keyword("testvalue".into()),
                    ),
                ],
            ),
        ];

        for (stylesheet, properties) in testcases {
            assert_eq!(
                to_styled_node(e, &stylesheet),
                Some(StyledNode {
                    node: e,
                    property: properties.iter().cloned().collect(),
                    children: vec![],
                })
            );
        }
    }
}
