use std::fmt::Display;

use crate::util::{string::bisect, vector::{intersections, chunks, first}};

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
            chunks(&sacks.map(|s| s.chars().collect()).collect(), 3)
                .into_iter().map(intersections).map(first).flatten().map(item_priority).sum::<u32>()
        )
    )
}
