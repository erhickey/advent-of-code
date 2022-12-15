use std::{collections::HashMap, hash::Hash};

use super::point::Point;

pub type Grid<T> = HashMap<Point, T>;

pub fn parse_grid<T: From<char>>(s: &str) -> Grid<T> {
    let mut y = -1;
    s
        .lines()
        .flat_map(|line| {
            y += 1;
            let mut x = -1;
            line.chars().map(|c| {
                x += 1;
                (Point { x, y }, T::from(c))
            }).collect::<Vec<(Point, T)>>()
        })
        .collect()
}

pub fn neighbors<'a, T: Clone + Eq + Hash>(
    point: &Point,
    grid: &'a Grid<T>,
    include_diagonal: bool
) -> impl Iterator<Item=Point> + 'a {
    if include_diagonal { point.adjacent_points() } else { point.neighboring_points() }
        .into_iter()
        .filter(|p| grid.contains_key(p))
}
