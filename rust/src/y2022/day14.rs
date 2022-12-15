use std::fmt::Display;

use rustc_hash::FxHashMap;

use crate::util::point::Point;

#[derive(Clone, Eq, PartialEq)]
enum Object {
    Rock,
    Sand
}

fn extrapolate_rocks(points: Vec<Point>) -> Vec<Point> {
    let mut iter = points.into_iter();
    let mut current_rock = iter.next().unwrap();

    iter.flat_map(|rock| {
        let line = current_rock.line_with(&rock);
        current_rock = rock;
        line
    }).collect()
}

fn parse_points(s: &str) -> Vec<Point> {
    s.split(" -> ").map(|s| s.parse().unwrap()).collect()
}

fn move_sand(objects: &FxHashMap<Point, Object>, position: &Point, floor: Option<i32>) -> Point {
    if floor.is_some() && floor.unwrap() == position.y + 1 {
        return position.clone();
    }

    let straight_down = Point { x: position.x, y: position.y + 1 };
    let diagonally_left = Point { x: position.x - 1, y: position.y + 1 };
    let diagonally_right = Point { x: position.x + 1, y: position.y + 1 };

    if objects.contains_key(&straight_down) {
        if objects.contains_key(&diagonally_left) {
            if objects.contains_key(&diagonally_right) {
                return position.clone();
            } else {
                return diagonally_right;
            }
        } else {
            return diagonally_left;
        }
    } else {
        return straight_down;
    }
}

fn spawn_sand(objects: &FxHashMap<Point, Object>, void: i32, has_floor: bool) -> Option<Point> {
    let origin = Point { x: 500, y: 0 };

    if objects.contains_key(&origin) {
        return None;
    }

    let floor = if has_floor { Some(void + 2) } else { None };
    let mut position = move_sand(objects, &origin, floor);
    let mut last_position = origin;

    while last_position != position && (has_floor || position.y != void) {
        last_position = position;
        position = move_sand(objects, &last_position, floor);
    }

    if !has_floor && position.y == void {
        None
    } else {
        Some(position)
    }
}

fn fill(rocks: &FxHashMap<Point, Object>, has_floor: bool) -> usize {
    let void = rocks.keys().map(|p| p.y).max().unwrap();
    let mut objects = rocks.clone();

    while let Some(new_sand) = spawn_sand(&objects, void, has_floor) {
        objects.insert(new_sand, Object::Sand);
    }

    objects.values().filter(|v| **v == Object::Sand).count()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let rocks: FxHashMap<Point, Object> = input
        .lines()
        .flat_map(|s|
            extrapolate_rocks(parse_points(s))
                .into_iter()
                .map(|p| (p, Object::Rock))
                .collect::<Vec<(Point, Object)>>()
        )
        .collect();

    (Box::new(fill(&rocks, false)), Box::new(fill(&rocks, true)))
}
