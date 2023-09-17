//! The module for tries, the structure used for detecting keywords.

use std::cmp::PartialEq;

/// The nodes used to build tries
pub struct Node<TKey, TValue> {
    /// The key stored in the node. The root node has no key.
    key: Option<TKey>,

    /// A list of all the children of this node.
    children: Vec<Node<TKey, TValue>>,

    /// If this node is a terminal node, this is the value represented by the node. Otherwise, this is None.
    /// If children is empty, this must have a Some value.
    value: Option<TValue>,
}

impl<TKey: PartialEq + Clone, TValue: Clone> Node<TKey, TValue> {
    /// Returns the value corresponding with the key if found.
    pub fn search(&self, key: &Vec<TKey>) -> Option<TValue> {
        if key.is_empty() {
            return self.value.clone();
        }
        for child in &self.children {
            if child.key == Some(key[0].clone()) {
                let mut key: Vec<TKey> = key.clone();
                key.remove(0);
                return child.search(&key);
            }
        }
        None
    }

    /// Creates a trie based on multiple lists of values and keys.
    pub fn new(values: Vec<(Vec<TKey>, TValue)>) -> Node<TKey, TValue> {
        let mut trie: Node<TKey, TValue> = Node {
            value: None,
            children: Vec::new(),
            key: None,
        };
        for value in values {
            trie.insert(&value.0, &value.1);
        }
        trie
    }

    // Inserts a key-value pair into the trie, assuming no value is already associated with that key.
    // Returns whether or not the insertion was successful.
    fn insert(&mut self, key: &Vec<TKey>, value: &TValue) -> bool {
        if key.is_empty() {
            if self.value.is_none() {
                self.value = Some(value.clone());
                return true;
            }
            return false;
        }
        for child in &mut self.children {
            if child.key == Some(key[0].clone()) {
                let mut key: Vec<TKey> = key.clone();
                key.remove(0);
                return child.insert(&key, value);
            }
        }
        let mut new_child: Node<TKey, TValue> = Node {
            key: Some(key[0].clone()),
            children: Vec::new(),
            value: None,
        };
        let mut key: Vec<TKey> = key.clone();
        key.remove(0);
        let value_to_return: bool = new_child.insert(&key, value);
        self.children.push(new_child);
        value_to_return
    }
}

impl<TValue: Clone> Node<char, TValue> {
    /// Returns the value corresponding with the key if found. Uses a String as the key.
    pub fn search_with_string(&self, key: &String) -> Option<TValue> {
        self.search(&string_to_vec(key))
    }

    /// Creates a trie based on multiple lists of values and keys. Uses Strings as the keys.
    pub fn new_with_string(values: Vec<(String, TValue)>) -> Node<char, TValue> {
        let mut new_vec: Vec<(Vec<char>, TValue)> = Vec::new();
        for pair in values {
            new_vec.push((string_to_vec(&pair.0), pair.1));
        }
        Node::new(new_vec)
    }
}

// Converts a string to a list of characters..
fn string_to_vec(value: &String) -> Vec<char> {
    let mut out: Vec<char> = Vec::new();
    for c in value.chars().by_ref() {
        out.push(c);
    }
    out
}
