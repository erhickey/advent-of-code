use std::fmt::Display;

type Trees = Vec<Vec<u32>>;

fn parse_trees(input: &str) -> Trees {
    input
        .lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

fn is_visible(trees: &Trees, (x, y): (usize, usize)) -> bool {
    if x == 0 || y == 0 || x == trees.len() - 1 ||  y == trees.len() -1 {
        return true;
    }

    let left_max_height = trees[y][0..x].iter().max().unwrap();
    let right_max_height = trees[y][x + 1..].iter().max().unwrap();
    let top_max_height = trees[0..y].iter().map(|v| v[x]).max().unwrap();
    let bot_max_height = trees[y + 1..].iter().map(|v| v[x]).max().unwrap();
    let min_max = [left_max_height, right_max_height, &top_max_height, &bot_max_height].into_iter().min().unwrap();

    trees[y][x] > *min_max
}

fn count_visible(trees: &Trees) -> usize {
    (0..trees.len())
        .into_iter()
        .flat_map(|y| {
            let f = move |x| is_visible(trees, (x, y));
            (0..trees[0].len()).into_iter().map(f)
        })
        .filter(|b| *b)
        .count()
}

fn viewing_distance(trees: Vec<u32>, height: u32) -> usize {
    let view = trees.clone().into_iter().take_while(|t| t < &height).count();
    view + if view == trees.len() { 0 } else { 1 }
}

fn scenic_score(trees: &Trees, (x, y): (usize, usize)) -> usize {
    let height = trees[y][x];
    let edge = trees.len() - 1;

    let dist_left = if y == 0 { 0 } else {
        viewing_distance(trees[y][0..x].iter().rev().cloned().collect(), height)
    };

    let dist_right = if y == edge { 0 } else {
        viewing_distance(trees[y][x + 1..].iter().cloned().collect(), height)
    };

    let dist_top = if x == 0 { 0 } else {
        viewing_distance(trees[0..y].iter().map(|v| v[x]).rev().collect(), height)
    };

    let dist_bot = if x == edge { 0 } else {
        viewing_distance(trees[y + 1..].iter().map(|v| v[x]).collect(), height)
    };

    dist_left * dist_right * dist_top * dist_bot
}

fn best_tree_score(trees: &Trees) -> usize {
    (0..trees.len())
        .into_iter()
        .flat_map(|y| {
            let f = move |x| scenic_score(trees, (x, y));
            (0..trees[0].len()).into_iter().map(f)
        })
        .max()
        .unwrap()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let trees = parse_trees(input);

    (Box::new(count_visible(&trees)), Box::new(best_tree_score(&trees)))
}
