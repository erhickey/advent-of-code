use std::{fmt::Display, collections::{HashMap, HashSet}};

use crate::util::{grid::{parse_grid, Grid, neighbors}, point::Point, dijkstra::dijkstra_shortest};

fn build_neighbor_map(grid: &Grid<char>) -> HashMap<Point, HashSet<Point>> {
    grid
        .iter()
        .map(|(p, c)|
            (p.clone(), neighbors(p, grid, false)
                .filter(|p2| *c as u32 + 1 >= *grid.get(p2).unwrap() as u32)
                .collect()
            )
        )
        .collect()
}

fn optimal_path(end: &Point, valid_neighbors: &HashMap<Point, HashSet<Point>>, grid: Grid<char>) -> u128 {
    grid.into_iter().filter(|(_, c)| *c == 'a').map(|(p, _)| dijkstra_shortest(&p, end, valid_neighbors)).min().unwrap()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let mut grid: Grid<char> = parse_grid(input);

    let start = grid.clone().into_iter().find(|(_, v)| *v == 'S').unwrap().0;
    let end = grid.clone().into_iter().find(|(_, v)| *v == 'E').unwrap().0;
    *grid.get_mut(&start).unwrap() = 'a';
    *grid.get_mut(&end).unwrap() = 'z';
    let neighbor_map = build_neighbor_map(&grid);

    (Box::new(dijkstra_shortest(&start, &end, &neighbor_map)), Box::new(optimal_path(&end, &neighbor_map, grid)))
}
