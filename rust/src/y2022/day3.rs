use std::{fmt::Display, collections::HashSet};

fn split_sack(s: &str) -> Vec<&str> {
    let (s1, s2) = s.split_at(s.len() / 2);
    vec![s1, s2]
}

fn common_item(ss: Vec<&str>) -> char {
    ss.iter().fold(HashSet::new(), |mut acc, s| {
        if acc.len() > 0 {
            let s_set: HashSet<char> = s.chars().collect();
            acc.retain(|c| s_set.contains(c));
            acc
        } else {
            s.chars().collect()
        }
    }).drain().collect::<Vec<char>>()[0]
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
        Box::new(sacks.clone().map(split_sack).map(common_item).map(item_priority).sum::<u32>()),
        Box::new(
            sacks.collect::<Vec<&str>>().chunks(3).collect::<Vec<&[&str]>>().iter().map(|a| a.to_vec())
                .collect::<Vec<Vec<&str>>>().into_iter().map(common_item).map(item_priority).sum::<u32>())
    )
}
