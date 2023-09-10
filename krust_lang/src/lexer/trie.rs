//! The module for tries, the structure used for detecting keywords.

use std::cmp::PartialEq;

/// The nodes used to build tries
pub struct Node<TKey, TValue>
{
    /// The key stored in the node. The root node has no key.
    key: Option<TKey>,

    /// A list of all the children of this node.
    children: Vec<Node<TKey, TValue>>,

    /// If this node is a terminal node, this is the value represented by the node. Otherwise, this is None.
    /// If children is empty, this must have a Some value.
    value: Option<TValue>
}

impl<TKey: PartialEq + Clone, TValue: Clone> Node<TKey, TValue>
{
    /// Searches for a value the corresponding key if found.
    pub fn search(&self, key: &Vec<TKey>) -> Option<TValue>
    {
        if key.len() == 0
        {
            return self.value.clone();
        }
        for child in &self.children
        {
            if child.key == Some(key[0].clone())
            {
                let mut key: Vec<TKey> = key.clone();
                key.remove(0);
                return child.search(&key);
            }
        }
        None
    }

    /// Creates a trie based on multiple lists of values and keys.
    pub fn new(values: Vec<(Vec<TKey>, TValue)>) -> Node<TKey, TValue>
    {
        let mut trie: Node<TKey,TValue> = Node{value: None, children: Vec::new(), key: None};
        for value in values { trie.insert(&value.0, &value.1); }
        trie
    }

    // Inserts a key-value pair into the trie, assuming no value is already associated with that key.
    // Returns whether or not the insertion was successful.
    fn insert(&mut self, key: &Vec<TKey>, value: &TValue) -> bool
    {
        if key.len() == 0
        {
            if let None = self.value
            {
                self.value = Some(value.clone());
                return true;
            }
            return false;
        }
        for child in &mut self.children
        {
            if child.key == Some(key[0].clone())
            {
                let mut key: Vec<TKey> = key.clone();
                key.remove(0);
                return child.insert(&key, &value);
            }
        }
        let mut new_child: Node<TKey, TValue> = Node{key: Some(key[0].clone()), children: Vec::new(), value: None};
        let mut key: Vec<TKey> = key.clone();
        key.remove(0);
        let value_to_return: bool =  new_child.insert(&key, value);
        self.children.push(new_child);
        value_to_return
    }
}