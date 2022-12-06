use std::{collections::HashSet, hash::Hash};

pub fn intersections<T: Eq + Hash>(vs: Vec<Vec<T>>) -> Vec<T> {
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

pub fn first<T>(v: Vec<T>) -> Option<T> {
    v.into_iter().nth(0)
}

pub fn chunks<T: Clone>(v: Vec<T>, size: usize) -> Vec<Vec<T>> {
    v
        .chunks(size)
        .map(|e| e.into())
        .collect()
}

pub fn bisect(s: &str) -> Vec<Vec<char>> {
    let (s1, s2) = s.split_at(s.len() / 2);
    vec![s1.chars().collect(), s2.chars().collect()]
}

pub fn transpose<T: Clone>(vs: Vec<Vec<T>>) -> Vec<Vec<T>>
{
    (0..vs[0].len())
        .map(|i| vs.iter().map(|v| v[i].clone()).collect::<Vec<T>>())
        .collect()
}
