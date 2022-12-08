use std::fmt::Display;

use crate::util::tree::Tree;

type FileSystem = Tree<File>;

struct File {
    name: String,
    size: u32
}

fn mkdir(name: &str) -> File {
    File { name: String::from(name), size: 0 }
}

fn cd(filesystem: &FileSystem, dir_index: usize, dir_name: &str) -> usize {
    *filesystem
        .nodes[dir_index]
        .children
        .iter()
        .find(|ix| {
            filesystem.nodes[**ix].data.name == dir_name
        })
        .unwrap()
}

fn create_filesystem(lines: Vec<&str>) -> FileSystem {
    let root_dir = mkdir(lines[0].split(' ').nth(2).unwrap());
    let mut filesystem = FileSystem { nodes: Vec::new() };
    let mut current_index = filesystem.create_node(root_dir, None);

    lines[1..].iter().for_each(|line| {
        let words: Vec<&str> = line.split(' ').collect();
        match words[0] {
            "$" if words[1] == "cd" && words[2] == ".." =>
                current_index = filesystem.nodes[current_index].parent.unwrap(),
            "$" if words[1] == "cd" => current_index = cd(&filesystem, current_index, words[2]),
            "$" => (),
            "dir" => _ = filesystem.create_node(mkdir(words[1]), Some(current_index)),
            _ => {
                let file = File { name: String::from(words[1]), size: words[0].parse().unwrap() };
                filesystem.create_node(file, Some(current_index));
            }
        }
    });

    filesystem
}

fn dir_size(filesystem: &FileSystem, dir_index: usize) -> u32 {
    filesystem
        .nodes[dir_index]
        .children
        .iter()
        .map(|ix| match filesystem.nodes[*ix].data.size {
            0 => dir_size(filesystem, *ix),
            _ => filesystem.nodes[*ix].data.size
        })
        .sum()
}

fn dir_sizes(filesystem: &FileSystem) -> Vec<u32> {
    filesystem
        .nodes
        .iter()
        .map(|n| match n.data.size {
            0 => dir_size(filesystem, n.index),
            _ => 0
        })
        .collect()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let filesystem = create_filesystem(input.lines().collect());
    let directory_sizes = dir_sizes(&filesystem);
    let root_size = directory_sizes.clone().into_iter().max().unwrap();
    let used_space = 70000000 - root_size;
    let needed_space = 30000000 - used_space;

    (
        Box::new(directory_sizes.iter().filter(|x| **x <= 100000).sum::<u32>()),
        Box::new(directory_sizes.into_iter().filter(|x| *x >= needed_space).min().unwrap())
    )
}
