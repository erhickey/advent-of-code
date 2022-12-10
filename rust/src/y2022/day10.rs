use std::fmt::Display;

use crate::util::vector::chunks;

#[derive(Clone)]
struct Cycle {
    during: i32,
    after: i32
}

enum Instruction {
    Noop,
    Addx(i32)
}

fn parse_instruction(s: &str) -> Instruction {
    match s.starts_with("addx") {
        true => Instruction::Addx(s[5..].parse().unwrap()),
        false => Instruction::Noop
    }
}

fn run_instruction(o_cycle: Option<&Cycle>, instruction: &Instruction) -> Vec<Cycle> {
    let cycle = o_cycle.unwrap_or(&Cycle { during: 1, after: 1 });
    match instruction {
        Instruction::Noop => vec![Cycle { during: cycle.after, after: cycle.after }],
        Instruction::Addx(x) => vec![
            Cycle { during: cycle.after, after: cycle.after },
            Cycle { during: cycle.after, after: cycle.after + x }
        ]
    }
}

fn run_instructions(instructions: &Vec<Instruction>) -> Vec<Cycle> {
    let acc = Vec::new();
    instructions.iter().fold(acc, |mut acc, instruction| {
        acc.extend(run_instruction(acc.clone().iter().last(), instruction));
        acc
    })
}

fn get_frequency(cycles: &Vec<Cycle>, ix: usize) -> i32 {
    cycles[ix - 1].during * ix as i32
}

fn draw(cycles: &Vec<Cycle>) -> String {
    chunks(cycles, 40)
        .iter()
        .map(|cs| cs
            .iter()
            .enumerate()
            .map(|(ix, cycle)|
                if [cycle.during - 1, cycle.during, cycle.during + 1].contains(&(ix as i32)) { '#' } else { '.' }
            )
            .collect::<Vec<char>>()
        )
        .flat_map(|mut cs| { cs.push('\n'); cs })
        .collect()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let instructions: Vec<Instruction> = input.lines().map(parse_instruction).collect();
    let cycles = run_instructions(&instructions);

    let part1 = get_frequency(&cycles, 20)
        + get_frequency(&cycles, 60)
        + get_frequency(&cycles, 100)
        + get_frequency(&cycles, 140)
        + get_frequency(&cycles, 180)
        + get_frequency(&cycles, 220);

    (Box::new(part1), Box::new(format!("\n\n{}\n", draw(&cycles))))
}
