mod y2015;
mod y2022;

use std::io::Write;
use std::{env, fs, fmt::Display};
use std::path::Path;

use curl::easy::{Easy, List};

fn main() {
    let args: Vec<String> = env::args().collect();

    let day: u8;
    let mut year: u16 = get_latest_year();
    let mut input_file: Option<&str> = None;

    if args.len() == 4 {
        day = args[1].parse().unwrap();
        year = args[2].parse().unwrap();
        input_file = Some(&args[3]);
    } else if args.len() == 3 {
        day = args[1].parse().unwrap();
        year = args[2].parse().unwrap();
    } else {
        day = args[1].parse().unwrap();
    }

    let input = match input_file {
        Some(s) => fs::read_to_string(s).expect(&*format!("cannot find input file: {}", s)),
        _ => get_input(year, day),
    };

    let solver = get_solver(year, day);
    let (part1, part2) = solver(&input);

    println!("\nDay: {} Year: {}", day, year);
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

fn get_latest_year() -> u16 {
    // TODO: return last year, or current year if month is December
    2022
}

fn get_input(year: u16, day: u8) -> String {
    let input_filename = format!("day{}.input", day);

    if !Path::new(&input_filename).exists() {
        println!("{} does not exist, attempting retrieval", &input_filename);

        let cookie = fs::read_to_string("cookie")
            .expect("cookie not found. Create a file named cookie, containing the value of the session cookie");
        let url = format!("https://adventofcode.com/{}/day/{}/input", year, day);
        let mut list = List::new();
        list.append("content-type: text/plain").unwrap();

        let mut easy = Easy::new();
        easy.url(&url).unwrap();
        easy.http_version(curl::easy::HttpVersion::V11).unwrap();
        easy.http_headers(list).unwrap();
        easy.cookie(&*format!("session={}", &cookie)).unwrap();

        fs::File::create(&input_filename).expect(&*format!("error creating file: {}", &input_filename));
        let mut file = fs::OpenOptions::new()
            .write(true)
            .append(true)
            .open(&input_filename)
            .expect(&*format!("error opening file: {}", &input_filename));

        let mut transfer = easy.transfer();
        transfer.write_function(|data| {
            file.write_all(data).expect(&*format!("error writing data to input file: {}", &input_filename));
            Ok(data.len())
        }).unwrap();

        transfer.perform().unwrap();
    }

    fs::read_to_string(&input_filename).expect(&format!("error reading {}", &input_filename))
}
