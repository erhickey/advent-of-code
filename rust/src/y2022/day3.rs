use std::{fmt::Display, collections::HashSet, hash::Hash};

fn intersections<T: Eq + Hash>(vs: Vec<Vec<T>>) -> Vec<T> {
    vs.into_iter()
        .map(|v| v.into_iter().collect::<HashSet<T>>())
        .fold(HashSet::new(), |mut acc, hs| {
            match acc.len() {
                0 => hs,
                _ => { acc.retain(|e| hs.contains(e)); acc }
            }
        })
        .drain()
        .collect()
}

fn first<T>(v: Vec<T>) -> Option<T> {
    v.into_iter().nth(0)
}

fn chunks<T: Clone>(v: Vec<T>, size: usize) -> Vec<Vec<T>> {
    v.chunks(size).map(|e| e.into()).collect()
}

fn bisect(s: &str) -> Vec<Vec<char>> {
    let (s1, s2) = s.split_at(s.len() / 2);
    vec![s1.chars().collect(), s2.chars().collect()]
}

fn item_priority(c: char) -> u32 {
    match c.is_lowercase() {
        true => c as u32 - 96,
        false => c as u32 - 38,
    }
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let sacks = input.lines();
    (
        Box::new(sacks.clone().map(bisect).map(intersections).map(first).flatten().map(item_priority).sum::<u32>()),
        Box::new(
            chunks(sacks.map(|s| s.chars().collect()).collect(), 3)
                .into_iter().map(intersections).map(first).flatten().map(item_priority).sum::<u32>()
        )
    )
}
