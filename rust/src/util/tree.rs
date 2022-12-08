pub struct Tree<T> {
    pub nodes: Vec<Node<T>>
}

pub struct Node<T> {
    pub index: usize,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
    pub data: T
}

impl<T> Tree<T> {
    pub fn create_node(&mut self, data: T, parent: Option<usize>) -> usize {
        let node_index = self.nodes.len();

        if parent.is_some() {
            let parent_ix = parent.unwrap();
            self.nodes[parent_ix].children.push(node_index);
        }

        self.nodes.push(Node {
            data,
            parent,
            index: node_index,
            children: Vec::new()
        });

        node_index
    }
}
