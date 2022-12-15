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
}
