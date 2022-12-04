use std::{fmt::Display, collections::HashSet};

fn parse_pair(s: &str) -> (HashSet<u32>, HashSet<u32>) {
    let ns: [u32; 4] = s.split(|c: char| !c.is_numeric()).map(|s| s.parse::<u32>())
        .flatten().collect::<Vec<u32>>().try_into().unwrap();
    ((ns[0]..ns[1] + 1).collect(), (ns[2]..ns[3] + 1).collect())
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let pairs = input.lines().map(parse_pair);

    (
        Box::new(pairs.clone().filter(|(s1, s2)| s1.is_subset(&s2) || s2.is_subset(&s1)).count()),
        Box::new(pairs.filter(|(s1, s2)| s1.intersection(s2).count() > 0).count())
    )
}
