use std::fmt::Display;

use crate::util::util::all_unique;

fn find_marker(input: Vec<char>, m_size: usize) -> usize {
    m_size + input
        .windows(m_size)
        .enumerate()
        .filter(|(_, cs)| all_unique(cs.iter().collect()))
        .next()
        .unwrap()
        .0
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let chars = input.chars().collect::<Vec<char>>();
    (Box::new(find_marker(chars.clone(), 4)), Box::new(find_marker(chars, 14)))
}
