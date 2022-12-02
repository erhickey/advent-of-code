use std::fmt::Display;

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let elves = input.split("\n\n").map(|s| s.lines().map(|l| l.parse::<u64>().unwrap()));
    let mut totals: Vec<u64> = elves.map(|a| a.sum::<u64>()).collect();
    totals.sort();

    (Box::new(*totals.iter().max().unwrap()), Box::new(totals.iter().rev().take(3).sum::<u64>()))
}
