use std::collections::HashSet;

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

    #[allow(dead_code)]
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

    #[allow(dead_code)]
    pub fn is_adjacent(&self, point: &Point) -> bool {
        self.adjacent_points().contains(point)
    }

    #[allow(dead_code)]
    pub fn is_adjacent_or_equal(&self, point: &Point) -> bool {
        self.is_adjacent(point) || self == point
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
}

#[allow(dead_code)]
pub fn pretty_print_points(ps: &HashSet<Point>) -> String {
    let xs: Vec<i32> = ps.clone().drain().into_iter().map(|p| p.x).collect();
    let ys: Vec<i32> = ps.clone().drain().into_iter().map(|p| p.y).collect();
    let min_x = xs.clone().into_iter().min().unwrap();
    let max_x = xs.into_iter().max().unwrap();
    let min_y = ys.clone().into_iter().min().unwrap();
    let max_y = ys.into_iter().max().unwrap();

    let mut ret: Vec<char> = (min_y..max_y + 1)
        .flat_map(|y| {
            let mut vs: Vec<char> = Vec::new();
            vs.push('\n');
            vs.extend(y.to_string().chars().rev());
            vs.push(' ');
            vs.extend((min_x..max_x + 1)
                .map(|x| if ps.contains(&Point { x, y }) { '#' } else { ' ' })
                .rev()
                .collect::<Vec<char>>());
            vs
        })
        .rev()
        .collect();
    ret.extend(min_x.to_string().chars());
    ret.extend(" ".repeat((max_x - min_x).abs() as usize).chars());
    ret.extend(max_x.to_string().chars());
    ret.iter().collect()
}
