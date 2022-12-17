use std::fmt::Display;

use nom::{bytes::complete::tag, sequence::preceded};
use rustc_hash::FxHashSet;

use crate::util::{point::Point, nom::i32};

#[derive(Clone, Eq, Hash, PartialEq)]
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

fn can_contain_sensor(sensors: &Vec<Sensor>, point: &Point) -> bool {
    for sensor in sensors {
        if point.manhattan_distance(&sensor.sensor) <= sensor.distance {
            return false;
        }
    }
    true
}

fn part2(sensors: &Vec<Sensor>, max: i32) -> i128 {
    let mut s_candidates: FxHashSet<(Sensor, Sensor)> = FxHashSet::default();

    // find pairs of sensors that may have an open point between diagonals
    for s1 in sensors {
        for s2 in sensors {
            if s1.sensor != s2.sensor {
                let distance = s1.sensor.manhattan_distance(&s2.sensor);
                if distance == s1.distance + s2.distance + 2 {
                    if !s_candidates.contains(&(s2.clone(), s1.clone())) {
                        s_candidates.insert((s1.clone(), s2.clone()));
                    }
                }
            }
        }
    }

    let mut candidates: FxHashSet<Point> = FxHashSet::default();

    // gather points between the identified diagonals
    // don't think this will work if the point we're
    // looking for is on the same x or y axis as either sensor
    for (s1, s2) in s_candidates {
        let x_cmp =  s2.sensor.x.cmp(&s1.sensor.x);
        let y_cmp =  s2.sensor.y.cmp(&s1.sensor.y);
        for point in s1.sensor.circumference(s1.distance + 1) {
            if point.x.cmp(&s1.sensor.x) == x_cmp && point.y.cmp(&s1.sensor.y) == y_cmp {
                candidates.insert(point);
            }
        }
    }

    // remove ineligible points
    candidates.retain(|p| p.x >= 0 && p.x <= max && p.y >= 0 && p.y <= max);

    // find solution
    let f = || {
        for p in candidates {
            if can_contain_sensor(sensors, &p) {
                return p;
            }
        }
        Point { x: 0, y: 0 }
    };
    let point = f();
    (point.x as i128 * 4000000) + point.y as i128
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let sensors: Vec<Sensor> = input.lines().map(parse_sensor).collect();

    (Box::new(part1(&sensors, 2000000)), Box::new(part2(&sensors, 4000000)))
}
