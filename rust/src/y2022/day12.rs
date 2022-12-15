use std::fmt::Display;

use rustc_hash::FxHashMap;

use crate::util::{grid::Grid, point::Point, dijkstra::dijkstra_shortest};

fn build_neighbor_map(grid: &Grid<char>) -> FxHashMap<Point, Vec<Point>> {
    grid
        .points
        .iter()
        .map(|(p, c)|
            (p.clone(), grid.neighbors(p, false)
                .filter(|p2| *c as u32 + 1 >= *grid.points.get(p2).unwrap() as u32)
                .collect()
            )
        )
        .collect()
}

fn optimal_path(end: &Point, valid_neighbors: &FxHashMap<Point, Vec<Point>>, grid: Grid<char>) -> u128 {
    grid.points
        .into_iter()
        .filter(|(_, c)| *c == 'a')
        .map(|(p, _)| dijkstra_shortest(&p, end, valid_neighbors))
        .min()
        .unwrap()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let mut grid: Grid<char> = input.parse().unwrap();

    let start = grid.points.clone().into_iter().find(|(_, v)| *v == 'S').unwrap().0;
    let end = grid.points.clone().into_iter().find(|(_, v)| *v == 'E').unwrap().0;
    *grid.points.get_mut(&start).unwrap() = 'a';
    *grid.points.get_mut(&end).unwrap() = 'z';
    let neighbor_map = build_neighbor_map(&grid);

    (Box::new(dijkstra_shortest(&start, &end, &neighbor_map)), Box::new(optimal_path(&end, &neighbor_map, grid)))
}
