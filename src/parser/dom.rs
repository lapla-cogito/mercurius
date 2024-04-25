#![allow(unused)]

#[derive(Debug, PartialEq)]
pub struct Node {
    pub children: Vec<Node>,
    pub node_type: NodeType,
}

#[derive(Debug, PartialEq)]
pub enum NodeType {
    Element(Element),
    Text(Text),
}

#[derive(Debug, PartialEq)]
pub struct Element {
    pub tag_name: String,
    pub attributes: AttrMap,
}

pub type AttrMap = std::collections::HashMap<String, String>;

impl Element {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(name: String, attributes: AttrMap, children: Vec<Node>) -> Node {
        Node {
            node_type: NodeType::Element(Element {
                tag_name: name,
                attributes,
            }),
            children,
        }
    }

    pub fn get_id(&self) -> Option<&String> {
        self.attributes.get("id")
    }
}

#[derive(Debug, PartialEq)]
pub struct Text {
    pub data: String,
}

impl Text {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(text: String) -> Box<Node> {
        Box::new(Node {
            node_type: NodeType::Text(Text { data: text }),
            children: vec![],
        })
    }
}
