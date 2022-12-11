use std::{fmt::Display, collections::HashSet};

use crate::util::point::Point;

#[derive(Clone)]
enum Direction {
    U, D, L, R
}

impl Direction {
    fn from(c: char) -> Direction {
        match c {
            'U' => Direction::U,
            'D' => Direction::D,
            'L' => Direction::L,
            'R' => Direction::R,
            _ => panic!()
        }
    }
}

#[derive(Clone)]
struct State {
    head: Point,
    tail: Option<Box<State>>
}

fn parse_move(s: &str) -> Vec<Direction> {
    let [d, c]: [&str; 2] = s.split(' ').collect::<Vec<&str>>().try_into().unwrap();
    let dir = d.chars().next().unwrap();
    (0..c.parse().unwrap()).into_iter().map(|_| Direction::from(dir)).collect()
}

fn update_tail(state: Option<Box<State>>, leader: Point) -> Option<Box<State>> {
    if state.is_none() {
        return None
    }

    let old_tail = state.unwrap();
    let current_pos = &old_tail.head;
    let x_separation = leader.x_distance(current_pos) > 1;
    let y_separation = leader.y_distance(current_pos) > 1;

    let head = Point {
        x: if x_separation || y_separation && current_pos.x != leader.x {
            if leader.is_left_of(current_pos) { current_pos.x - 1 } else { current_pos.x + 1 }
        } else { current_pos.x },
        y: if y_separation || x_separation && current_pos.y != leader.y {
            if leader.is_under(current_pos) { current_pos.y - 1 } else { current_pos.y + 1 }
        } else { current_pos.y }
    };

    Some(Box::new(State { head: head.clone(), tail: update_tail(old_tail.tail, head) }))
}

fn do_move(state: &State, dir: &Direction) -> State {
    let head = match dir {
        Direction::U => state.head.move_up(1),
        Direction::D => state.head.move_down(1),
        Direction::L => state.head.move_left(1),
        Direction::R => state.head.move_right(1)
    };

    State { head: head.clone(), tail: update_tail(state.tail.clone(), head) }
}

fn do_moves(moves: Vec<Direction>, tail_size: u8) -> Vec<State> {
    let mut init: Vec<State> = Vec::new();
    init.push(create_state(tail_size));

    moves
        .iter()
        .fold(init, |mut acc, mv| {
            acc.push(do_move(acc.last().unwrap(), mv));
            acc
        })
}

fn create_state(tail_size: u8) -> State {
    State {
        head: Point { x: 0, y: 0 },
        tail: if tail_size == 0 { None } else { Some(Box::new(create_state(tail_size - 1 ))) }
    }
}

fn get_end_position(state: State) -> Point {
    if state.tail.is_none() { state.head } else { get_end_position(*state.tail.unwrap()) }
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let moves: Vec<Direction> = input.lines().flat_map(parse_move).collect();
    let part1: HashSet<Point> = do_moves(moves.clone(), 1).into_iter().map(get_end_position).collect();
    let part2: HashSet<Point> = do_moves(moves, 9).into_iter().map(get_end_position).collect();

    (Box::new(part1.len()), Box::new(part2.len()))
}
