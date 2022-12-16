use std::fmt::Display;

use nom::{bytes::complete::tag, sequence::preceded};

use crate::util::{point::Point, nom::i32};

struct Sensor {
    sensor: Point,
    beacon: Point,
    min_x: i32,
    max_x: i32,
    distance: u32
}

fn create_sensor(sx: i32, sy: i32, bx: i32, by: i32) -> Sensor {
    let sensor = Point { x: sx, y: sy };
    let beacon = Point { x: bx, y: by };
    let distance = sensor.manhattan_distance(&beacon);
    let min_x = sx - distance as i32;
    let max_x = sx + distance as i32;
    Sensor { sensor, beacon, min_x, max_x, distance }
}

fn parse_sensor(s: &str) -> Sensor {
    let (s, sx) = preceded(tag("Sensor at x="), i32)(s).unwrap();
    let (s, sy) = preceded(tag(", y="), i32)(s).unwrap();
    let (s, bx) = preceded(tag(": closest beacon is at x="), i32)(s).unwrap();
    let (_, by) = preceded(tag(", y="), i32)(s).unwrap();
    create_sensor(sx, sy, bx, by)
}

fn cant_contain_sensor(sensors: &Vec<Sensor>, point: Point) -> bool {
    for sensor in sensors {
        if point.manhattan_distance(&sensor.sensor) <= sensor.distance && point != sensor.beacon {
            return true;
        }
    }
    false
}

fn part1(sensors: &Vec<Sensor>, y: i32) -> usize {
    let min_x = sensors.iter().map(|s| s.min_x).min().unwrap();
    let max_x = sensors.iter().map(|s| s.max_x).max().unwrap();
    (min_x..max_x + 1)
        .filter(|x| cant_contain_sensor(sensors, Point { x: *x, y }))
        .count()
}

fn part2(_sensors: &Vec<Sensor>, _max: i32) -> i128 {
    0
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let sensors: Vec<Sensor> = input.lines().map(parse_sensor).collect();

    (Box::new(part1(&sensors, 2000000)), Box::new(part2(&sensors, 4000000)))
    // (Box::new(part1(&sensors, 10)), Box::new(part2(&sensors, 20)))
}
