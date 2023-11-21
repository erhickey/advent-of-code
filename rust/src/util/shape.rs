use super::point::Point;

#[derive(Clone, Debug)]
pub struct Shape {
    pub points: Vec<Point>,
    pub min_x: i32,
    pub max_x: i32,
    pub min_y: i32,
    pub max_y: i32
}

impl Shape {
    #[allow(dead_code)]
    pub fn move_up(&mut self) {
        self.min_y += 1;
        self.max_y += 1;
        for point in &mut self.points {
            point.y += 1;
        }
    }

    pub fn move_down(&mut self) {
        self.min_y -= 1;
        self.max_y -= 1;
        for point in &mut self.points {
            point.y -= 1;
        }
    }

    pub fn move_left(&mut self) {
        self.min_x -= 1;
        self.max_x -= 1;
        for point in &mut self.points {
            point.x -= 1;
        }
    }

    pub fn move_right(&mut self) {
        self.min_x += 1;
        self.max_x += 1;
        for point in &mut self.points {
            point.x += 1;
        }
    }

    #[allow(dead_code)]
    pub fn points_sorted_from_bottom(&self) -> Vec<Point> {
        let mut ps: Vec<Point> = self.points.clone().into_iter().collect();
        ps.sort_by_key(|p| p.y);
        ps
    }
}

impl From<Vec<Point>> for Shape {
    fn from(points: Vec<Point>) -> Self {
        let xs = points.iter().map(|p| p.x);
        let ys = points.iter().map(|p| p.y);
        let min_x = xs.clone().min().unwrap();
        let max_x = xs.max().unwrap();
        let min_y = ys.clone().min().unwrap();
        let max_y = ys.max().unwrap();
        Shape { points, min_x, max_x, min_y, max_y }
    }
}
