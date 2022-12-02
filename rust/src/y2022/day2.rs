use std::fmt::Display;

fn score_round((c1, c2): (char, char)) -> u32 {
    result_value(c1, c2) + play_value(c2)
}

fn result_value(c1: char, c2: char) -> u32 {
    match c1 {
        'A' if c2 == 'X' => 3,
        'A' if c2 == 'Y' => 6,
        'A' if c2 == 'Z' => 0,
        'B' if c2 == 'X' => 0,
        'B' if c2 == 'Y' => 3,
        'B' if c2 == 'Z' => 6,
        'C' if c2 == 'X' => 6,
        'C' if c2 == 'Y' => 0,
        'C' if c2 == 'Z' => 3,
        _ => 0
    }
}

fn play_value(c: char) -> u32 {
    match c {
        'A' | 'X' => 1,
        'B' | 'Y' => 2,
        'C' | 'Z' => 3,
        _ => 0
    }
}

fn determine_play(c1: char, c2: char) -> char {
    match c1 {
        'A' if c2 == 'X' => 'Z',
        'A' if c2 == 'Y' => 'X',
        'A' if c2 == 'Z' => 'Y',
        'B' if c2 == 'X' => 'X',
        'B' if c2 == 'Y' => 'Y',
        'B' if c2 == 'Z' => 'Z',
        'C' if c2 == 'X' => 'Y',
        'C' if c2 == 'Y' => 'Z',
        'C' if c2 == 'Z' => 'X',
        _ => 'D'
    }
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let rounds = input
        .lines()
        .map(|s| {
            let bytes = s.as_bytes();
            (bytes[0] as char, bytes[2] as char)
        });

    (
        Box::new(rounds.clone().map(score_round).sum::<u32>()),
        Box::new(rounds.map(|(c1, c2)| score_round((c1, determine_play(c1, c2)))).sum::<u32>())
    )
}
