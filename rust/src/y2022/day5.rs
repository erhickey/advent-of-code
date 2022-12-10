use std::{fmt::Display, collections::HashMap};

use crate::util::vector::transpose;

type Stacks = HashMap<u8, Vec<char>>;
type Move = [u8; 3];

fn parse_stacks(s: &str) -> Stacks {
    let lines: Vec<Vec<char>> = s.clone()
        .lines()
        .rev()
        .map(|s| s.chars().collect())
        .collect();

    let transposed_lines: Vec<Vec<char>> = transpose(lines)
        .into_iter()
        .filter(|v| v[0] != ' ')
        .map(|v| v.into_iter().filter(|c| *c != ' ').collect())
        .collect();

    transposed_lines
        .into_iter()
        .map(|line| (
            line[0].to_digit(10).unwrap() as u8,
            line.clone().drain(1..).collect()
        ))
        .collect()
}

fn parse_move(s: &str) -> Move {
    s
        .split(' ')
        .filter(|s| s.parse::<u8>().is_ok())
        .map(|s| s.parse::<u8>().unwrap())
        .collect::<Vec<u8>>()
        .try_into()
        .unwrap()
}

fn do_move(sm: Stacks, m: Move, keep_order: bool) -> Stacks {
    let [n, f, t] = m;
    let mut from = sm.get(&f).unwrap().clone();
    let mut to = sm.get(&t).unwrap().clone();
    let mut holding: Vec<char> = Vec::new();
    (0..n).for_each(|_| holding.push(from.pop().unwrap()));
    holding = if keep_order { holding } else { holding.into_iter().rev().collect() };
    (0..n).for_each(|_| to.push(holding.pop().unwrap()));
    let mut new_stacks = sm.clone();
    new_stacks.insert(f, from);
    new_stacks.insert(t, to);
    new_stacks
}

fn get_message(sm: Stacks) -> String {
    let mut sorted_set: Vec<(u8, Vec<char>)> = sm.into_iter().collect();
    sorted_set.sort();
    sorted_set.iter().map(|(_, v)| v.last().unwrap()).collect()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let [stack_input, moves_input]: [&str; 2] = input
        .split("\n\n")
        .collect::<Vec<&str>>()
        .try_into()
        .unwrap();

    let crate_mover_9000 = |sm, m| do_move(sm, m, false);
    let crate_mover_9001 = |sm, m| do_move(sm, m, true);
    let stacks: Stacks = parse_stacks(stack_input);
    let moves: Vec<Move> = moves_input.lines().map(parse_move).collect();
    let p1_moved_stacks: Stacks = moves.clone().into_iter().fold(stacks.clone(), crate_mover_9000);
    let p2_moved_stacks: Stacks = moves.into_iter().fold(stacks, crate_mover_9001);

    (Box::new(get_message(p1_moved_stacks)), Box::new(get_message(p2_moved_stacks)))
}
