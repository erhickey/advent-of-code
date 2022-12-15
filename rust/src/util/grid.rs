use std::{hash::Hash, fmt::{Formatter, Display, Error}, str::FromStr};

use rustc_hash::FxHashMap;

use super::point::Point;

pub struct Grid<T: Clone + Eq + Hash> {
    pub points: FxHashMap<Point, T>,
    max_x: i32,
    max_y: i32,
    min_x: i32,
    min_y: i32
}

impl<T: Clone + Eq + Hash> Grid<T> {
    pub fn neighbors(&self, point: &Point, include_diagonal: bool) -> impl Iterator<Item=Point> + '_ {
        if include_diagonal { point.adjacent_points() } else { point.neighboring_points() }
            .into_iter()
            .filter(|p| self.is_valid_point(p))
    }

    pub fn is_valid_point(&self, p: &Point) -> bool {
        p.x >= self.min_x && p.x <= self.max_x && p.y >= self.min_y && p.y <= self.max_y
    }
}

impl<T: Clone + Eq + From<char> + Hash> FromStr for Grid<T> {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut y = -1;
        let grid: FxHashMap<Point, T> = s
            .lines()
            .flat_map(|line| {
                y += 1;
                let mut x = -1;
                line.chars().map(|c| {
                    x += 1;
                    (Point { x, y }, T::from(c))
                }).collect::<Vec<(Point, T)>>()
            })
            .collect();

        let max_x = grid.keys().map(|p| p.x).max().unwrap();
        let max_y = grid.keys().map(|p| p.y).max().unwrap();

        Ok(Grid {
            points: grid,
            max_x,
            max_y,
            min_x: 0,
            min_y: 0
        })
    }
}

impl<T: Clone + Eq + Hash + Into<char>> Display for Grid<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut lines: Vec<char> = (self.min_y..self.max_y + 1)
            .flat_map(|y| {
                let mut line: Vec<char> = Vec::new();
                line.push('\n');
                line.extend((self.min_x..self.max_x + 1)
                    .map(|x| {
                        let p = Point { x, y };
                        if self.points.contains_key(&p) {
                            self.points.get(&p).unwrap().clone().into()
                        } else { ' ' }
                    })
                    .collect::<Vec<char>>());
                line.push(' ');
                line.extend(y.to_string().chars());
                line
            })
            .collect();

        lines.push('\n');
        let min_x: Vec<char> = self.min_x.to_string().chars().collect();
        lines.extend(min_x.clone());
        lines.extend(" ".repeat((self.max_x - self.min_x).abs() as usize - min_x.len()).chars());
        lines.extend(self.max_x.to_string().chars());

        write!(f, "{}", lines.iter().collect::<String>())
    }
}
