mod util;
mod y2015;
mod y2022;

use std::time::Instant;
use std::{env, fs, fmt::Display};

fn main() {
    let args: Vec<String> = env::args().collect();
    let day: u8 = args[1].parse().unwrap();
    let year: u16 = args[2].parse().unwrap();
    let input_file = &args[3];

    let input = fs::read_to_string(input_file).expect(&*format!("cannot find input file: {}", input_file));
    let solver = get_solver(year, day);
    let now = Instant::now();
    let (part1, part2) = solver(&input);
    let elapsed = now.elapsed();

    println!("\n{} day {}", year, day);
    println!("---------------------------");
    println!("run time: {:.2?}", elapsed);
    println!("---------------------------");
    println!("part 1: {}", part1);
    println!("part 2: {}", part2);
}

fn get_solver(year: u16, day: u8) -> fn(&str) -> (Box<dyn Display>, Box<dyn Display>) {
    match year {
        2015 => match day {
            1 => y2015::day1::solve,
            2 => y2015::day2::solve,
            _ => unimplemented!(),
        },
        2022 => match day {
            1 => y2022::day1::solve,
            2 => y2022::day2::solve,
            3 => y2022::day3::solve,
            4 => y2022::day4::solve,
            5 => y2022::day5::solve,
            6 => y2022::day6::solve,
            7 => y2022::day7::solve,
            8 => y2022::day8::solve,
            9 => y2022::day9::solve,
            10 => y2022::day10::solve,
            11 => y2022::day11::solve,
            12 => y2022::day12::solve,
            13 => y2022::day13::solve,
            14 => y2022::day14::solve,
            15 => y2022::day15::solve,
            16 => y2022::day16::solve,
            17 => y2022::day17::solve,
            18 => y2022::day18::solve,
            19 => y2022::day19::solve,
            20 => y2022::day20::solve,
            21 => y2022::day21::solve,
            22 => y2022::day22::solve,
            23 => y2022::day23::solve,
            24 => y2022::day24::solve,
            25 => y2022::day25::solve,
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}
