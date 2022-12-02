use std::fmt::Display;

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let result = input.chars().scan(0, |state, x| {
        *state = *state + match x {
            '(' => 1,
            ')' => -1,
            _ => 0,
        };
        Some(*state)
    });

    let p1: i64 = result.clone().last().unwrap();
    let p2 = result.clone().position(|i| i == -1).unwrap() + 1;
    (Box::new(p1), Box::new(p2))
}
