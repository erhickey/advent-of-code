use std::{collections::{HashSet, HashMap, BinaryHeap}, hash::Hash, cmp::Reverse};

pub fn dijkstra_shortest<T: Clone + Eq + Hash + Ord>(
    start: &T,
    end: &T,
    valid_neighbors: &HashMap<T, HashSet<T>>
) -> u128 {
    let distance_map = dijkstra_distance(start, end, valid_neighbors);
    distance_map.get(&end).unwrap_or(&u128::MAX).clone()
}

pub fn dijkstra_distance<T: Clone + Eq + Hash + Ord>(
    start: &T,
    end: &T,
    valid_neighbors: &HashMap<T, HashSet<T>>
) -> HashMap<T, u128> {
    let mut visited_nodes: HashSet<T> = HashSet::new();
    let mut distances: HashMap::<T, u128> = HashMap::from([(start.clone(), 0); 1]);
    let mut queue: BinaryHeap<Reverse<(u128, &T)>> = BinaryHeap::new();

    queue.push(Reverse((0, start)));

    while let Some(Reverse((distance, current_node))) = queue.pop() {
        if !visited_nodes.insert(current_node.clone()) {
            continue;
        }

        if current_node == end {
            break;
        }

        let new_distance = distance + 1;

        for neighbor in valid_neighbors.get(&current_node).unwrap() {
            // is it faster to check if visited nodes contains neighbor to possibly skip the
            // following match block or not? Only tested on 2022 day 12 so far, it was a little
            // faster to skip the check and always execute the following match block
            match distances.get_mut(neighbor) {
                Some(d) => {
                    if *d > new_distance {
                        *d = new_distance;
                        queue.push(Reverse((new_distance, neighbor)));
                    }
                },
                None => {
                    distances.insert(neighbor.clone(), new_distance);
                    queue.push(Reverse((new_distance, neighbor)));
                }
            }
        }
    }

    distances
}
