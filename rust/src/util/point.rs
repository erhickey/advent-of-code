use std::{str::FromStr, fmt::Error};

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Point {
    pub x: i32,
    pub y: i32
}

impl Point {
    pub fn move_up(&self, n: i32) -> Point {
        Point { x: self.x, y: self.y + n }
    }

    pub fn move_down(&self, n: i32) -> Point {
        Point { x: self.x, y: self.y - n }
    }

    pub fn move_left(&self, n: i32) -> Point {
        Point { x: self.x - n, y: self.y }
    }

    pub fn move_right(&self, n: i32) -> Point {
        Point { x: self.x + n, y: self.y }
    }

    pub fn neighboring_points(&self) -> Vec<Point> {
        vec![
            Point { x: self.x - 1, y: self.y },
            Point { x: self.x, y: self.y - 1 },
            Point { x: self.x, y: self.y + 1 },
            Point { x: self.x + 1, y: self.y },
        ]
    }

    pub fn adjacent_points(&self) -> Vec<Point> {
        vec![
            Point { x: self.x - 1, y: self.y + 1 },
            Point { x: self.x - 1, y: self.y },
            Point { x: self.x - 1, y: self.y - 1 },
            Point { x: self.x, y: self.y - 1 },
            Point { x: self.x, y: self.y + 1 },
            Point { x: self.x + 1, y: self.y + 1 },
            Point { x: self.x + 1, y: self.y },
            Point { x: self.x + 1, y: self.y - 1 }
        ]
    }

    pub fn x_distance(&self, point: &Point) -> i32 {
        (self.x - point.x).abs()
    }

    pub fn y_distance(&self, point: &Point) -> i32 {
        (self.y - point.y).abs()
    }

    pub fn is_left_of(&self, point: &Point) -> bool {
        self.x < point.x
    }

    pub fn is_under(&self, point: &Point) -> bool {
        self.y < point.y
    }

    pub fn line_with(&self, point: &Point) -> Vec<Point> {
        let min_x = if self.x < point.x { self.x } else { point.x };
        let max_x = if self.x > point.x { self.x } else { point.x };
        let min_y = if self.y < point.y { self.y } else { point.y };
        let max_y = if self.y > point.y { self.y } else { point.y };

        let mut points: Vec<Point> = Vec::new();
        for y in min_y..max_y + 1 {
            for x in min_x..max_x + 1 {
                points.push(Point { x, y });
            }
        }
        points
    }

    pub fn manhattan_distance(&self, point: &Point) -> u32 {
        ((self.x - point.x).abs() + (self.y - point.y).abs()) as u32
    }

    #[allow(dead_code)]
    pub fn circle(&self, manhattan_distance: u32) -> Vec<Point> {
        let mut points: Vec<Point> = Vec::new();
        for y in self.y - manhattan_distance as i32..self.y + manhattan_distance as i32 + 1 {
            let vert_dist = (self.y - y).abs();
            let to_expand = manhattan_distance as i32 - vert_dist;
            for x in self.x - to_expand..self.x + to_expand + 1 {
                points.push(Point { x, y });
            }
        }
        points
    }

    #[allow(dead_code)]
    pub fn circumference(&self, manhattan_distance: u32) -> Vec<Point> {
        let mut points: Vec<Point> = Vec::new();
        for y in self.y - manhattan_distance as i32..self.y + manhattan_distance as i32 + 1 {
            let vert_dist = (self.y - y).abs();
            let to_expand = manhattan_distance as i32 - vert_dist;
            points.push(Point { x: self.x - to_expand, y });
            points.push(Point { x: self.x + to_expand, y });
        }
        points
    }
}

impl FromStr for Point {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let [x_str, y_str]: [&str; 2] = s.split(",").collect::<Vec<&str>>().try_into().unwrap();

        Ok(Point {
            x: x_str.parse().unwrap(),
            y: y_str.parse().unwrap()
        })
    }
}
