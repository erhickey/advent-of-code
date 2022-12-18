use std::{fmt::Display, cmp::max};

use rustc_hash::FxHashSet;

use crate::util::{shape::Shape, point::Point};

#[derive(Debug, Clone)]
enum Jet {
    Left,
    Right
}

#[derive(Clone)]
struct State {
    jets: Vec<Jet>,
    jet_ix: usize,
    current_shape: Shape,
    shape_ix: u8,
    num_rocks: u128,
    fallen_rocks: FxHashSet<Point>,
    highest_rock: i32
}

impl State {
    fn blow(&mut self, jet: &Jet) {
        match jet {
            Jet::Right => self.move_right(),
            Jet::Left => self.move_left()
        }
    }

    fn move_right(&mut self) {
        if self.current_shape.max_x == 6 {
            return;
        }

        let mut shape = self.current_shape.clone();
        shape.move_right();

        if self.can_contain_shape(shape) {
            self.current_shape.move_right();
        }
    }

    fn move_left(&mut self) {
        if self.current_shape.min_x == 0 {
            return;
        }

        let mut shape = self.current_shape.clone();
        shape.move_left();

        if self.can_contain_shape(shape) {
            self.current_shape.move_left();
        }
    }

    fn can_contain_shape(&self, shape: Shape) -> bool {
        if shape.min_y == 0 {
            return false;
        }

        let f = || {
            for p in &shape.points {
                if self.fallen_rocks.contains(p) {
                    return false;
                }
            }
            true
        };
        f()
    }

    fn move_down(&mut self) {
        let mut shape = self.current_shape.clone();
        shape.move_down();

        if self.can_contain_shape(shape) {
            self.current_shape.move_down();
        } else {
            self.highest_rock = max(self.highest_rock, self.current_shape.max_y);
            self.fallen_rocks.extend(self.current_shape.points.clone());
            self.num_rocks += 1;
            self.spawn_new_shape();
        }
    }

    fn spawn_new_shape(&mut self) {
        let y = self.highest_rock + 4;
        self.shape_ix = if self.shape_ix == 4 { 0 } else { self.shape_ix + 1 };

        self.current_shape = match self.shape_ix {
            0 => shape0(y),
            1 => shape1(y),
            2 => shape2(y),
            3 => shape3(y),
            4 => shape4(y),
            _ => panic!()
        }
    }

    fn tick(&mut self) {
        if self.jet_ix == self.jets.len() {
            self.jet_ix = 0;
        }


        let jet = self.jets[self.jet_ix].clone();
        self.blow(&jet);
        self.move_down();

        self.jet_ix += 1;
    }
}

impl From<Vec<Jet>> for State {
    fn from(jets: Vec<Jet>) -> Self {
        State {
            jets,
            jet_ix: 0,
            current_shape: shape0(4),
            shape_ix: 0,
            num_rocks: 0,
            highest_rock: 0,
            fallen_rocks: FxHashSet::default()
        }
    }
}

fn shape0(y: i32) -> Shape {
    Shape::from(vec![
        Point { x: 2, y },
        Point { x: 3, y },
        Point { x: 4, y },
        Point { x: 5, y }
    ])
}

fn shape1(y: i32) -> Shape {
    Shape::from(vec![
        Point { x: 2, y: y + 1 },
        Point { x: 3, y },
        Point { x: 3, y: y + 1 },
        Point { x: 3, y: y + 2 },
        Point { x: 4, y: y + 1 }
    ])
}

fn shape2(y: i32) -> Shape {
    Shape::from(vec![
        Point { x: 2, y },
        Point { x: 3, y },
        Point { x: 4, y },
        Point { x: 4, y: y + 1 },
        Point { x: 4, y: y + 2 }
    ])
}

fn shape3(y: i32) -> Shape {
    Shape::from(vec![
        Point { x: 2, y },
        Point { x: 2, y: y + 1 },
        Point { x: 2, y: y + 2 },
        Point { x: 2, y: y + 3 }
    ])
}

fn shape4(y: i32) -> Shape {
    Shape::from(vec![
        Point { x: 2, y },
        Point { x: 3, y },
        Point { x: 2, y: y + 1 },
        Point { x: 3, y: y + 1 }
    ])
}

fn parse_jet(c: char) -> Jet {
    match c {
        '>' => Jet::Right,
        '<' => Jet::Left,
        _ => panic!()
    }
}

fn part1(mut state: State) -> i32 {
    while state.num_rocks < 2022 {
        state.tick();
    }
    state.highest_rock
}

fn part2(mut state: State) -> i32 {
    state.tick();
    state.highest_rock
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let jets: Vec<Jet> = input.chars().filter(|c| *c == '>' || *c == '<').map(parse_jet).collect();
    let state = State::from(jets);

    (Box::new(part1(state.clone())), Box::new(part2(state)))
}
