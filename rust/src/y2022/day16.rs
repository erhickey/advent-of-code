use std::fmt::Display;

use itertools::{Itertools, enumerate};
use nohash_hasher::{IntMap, IntSet};
use nom::{bytes::complete::{tag, take}, sequence::preceded, multi::separated_list1, IResult, branch::alt};
use rustc_hash::FxHashMap;

use crate::util::{nom::u_8, dijkstra::dijkstra_shortest};

type Valves = IntMap<u16, Valve>;

#[derive(Clone, Eq, PartialEq)]
struct Valve {
    id: u16,
    flow_rate: u8,
    leads_to: Vec<u16>,
    distances: IntMap<u16, u8>
}

#[derive(Clone)]
struct State {
    closed_valves: IntSet<u16>,
    pressure_per_turn: u8,
    pressure_released: u16,
    destinations: Vec<(u16, u8)>
}

impl State {
    fn open_valve(&mut self, valve: u16, valves: &Valves) {
        if self.closed_valves.remove(&valve) {
            self.pressure_per_turn += valves.get(&valve).unwrap().flow_rate;
        }
    }

    fn valid_destinations(&self, current_positions: Vec<u16>, valves: &Valves) -> Vec<Vec<(u16, u8)>> {
        let mut destinations: Vec<Vec<(u16, u8)>> = Vec::new();

        for dest_combo in self.closed_valves.iter().combinations(current_positions.len()) {
            for pos_perm in current_positions.iter().permutations(current_positions.len()) {
                let mut ds: Vec<(u16, u8)> = Vec::new();
                for (ix, pos) in enumerate(&pos_perm) {
                    let v = valves.get(pos).unwrap();
                    let dest = dest_combo[ix];
                    ds.push((*dest, *v.distances.get(dest).unwrap()));
                }
                destinations.push(ds);
            }
        }

        destinations.into_iter().unique().collect()
    }

    fn states_from(&self, destinations: Vec<Vec<(u16, u8)>>) -> Vec<State> {
        let mut states: Vec<State> = Vec::new();
        for ds in &destinations {
            let mut s = self.clone();
            s.destinations.extend(ds);
            states.push(s);
        }
        states
    }

    fn spawn_possible_states(&self, current_positions: Vec<u16>, valves: &Valves) -> Vec<State> {
        self.states_from(self.valid_destinations(current_positions, valves))
    }

    fn tick(&mut self, valves: &Valves) -> Vec<u16> {
        self.release_pressure();

        let mut destinations: Vec<(u16, u8)> = Vec::new();
        let mut valves_opened: Vec<u16> = Vec::new();

        for (valve, distance) in &self.destinations {
            if *distance == 0 {
                valves_opened.push(*valve);
            } else {
                destinations.push((*valve, distance - 1));
            }
        }

        for valve in &valves_opened {
            self.open_valve(*valve, valves);
        }

        self.destinations = destinations;
        valves_opened
    }

    fn release_pressure(&mut self) {
        self.pressure_released += self.pressure_per_turn as u16;
    }
}

impl From<Valves> for State {
    fn from(valves: Valves) -> Self {
        State {
            closed_valves: valves.values().filter(|v| v.flow_rate > 0).map(|v| v.id).collect(),
            pressure_per_turn: 0,
            pressure_released: 0,
            destinations: Vec::new()
        }
    }
}

fn parse_valve_name(s: &str) -> IResult<&str, &str> {
    take(2usize)(s)
}

fn parse_valve(s: &str) -> Valve {
    let (s, name) = preceded(tag("Valve "), parse_valve_name)(s).unwrap();
    let (s, flow_rate) = preceded(tag(" has flow rate="), u_8)(s).unwrap();
    let (_, leads_to) = preceded(
        alt((tag("; tunnel leads to valve "), tag("; tunnels lead to valves "))),
        separated_list1(tag(", "), parse_valve_name)
    )(s).unwrap();
    Valve {
        id: valve_name_to_id(name),
        flow_rate,
        leads_to: leads_to.into_iter().map(|s| valve_name_to_id(s)).collect(),
        distances: IntMap::default()
    }
}

fn valve_name_to_id(s: &str) -> u16 {
    let [c1, c2]: [char; 2] = s.chars().collect::<Vec<char>>().try_into().unwrap();
    ((c1 as u16) << 8) + c2 as u16
}

fn build_distance_maps(valves: Valves) -> Valves {
    let ends: Vec<u16> = valves.values().filter(|v| v.flow_rate > 0).map(|v| v.id).collect();
    let mut starts = ends.clone();
    starts.push(valve_name_to_id("AA"));

    let valid_neighbors: FxHashMap<u16, Vec<u16>> = valves.values().map(|v| (v.id, v.leads_to.clone())).collect();

    let mut calced: Vec<Valve> = Vec::new();

    for start in starts {
        let mut distance_map: IntMap<u16, u8> = IntMap::default();
        for end in ends.clone() {
            distance_map.insert(end, dijkstra_shortest(&start, &end, &valid_neighbors) as u8);
        }
        let mut valve = valves.get(&start).unwrap().clone();
        valve.distances = distance_map;
        calced.push(valve);
    }

    calced.into_iter().map(|v| (v.id, v)).collect()
}

fn do_tick(mut state: State, valves: &Valves) -> Vec<State> {
    let valves_opened = state.tick(valves);

    if state.closed_valves.len() == 0 || valves_opened.len() == 0 {
        return vec![state];
    }

    state.spawn_possible_states(valves_opened, valves)
}

fn do_ticks(mut states: Vec<State>, turns: u8, valves: &Valves) -> u16 {
    for _ in 0..turns {
        let mut new_states: Vec<State> = Vec::new();

        for state in states {
            new_states.extend(do_tick(state, valves));
        }

        let leader: i16 = new_states.iter().map(|s| s.pressure_released).max().unwrap() as i16;
        // arbitrarily reduce the number of paths to consider
        // this works for all tests/inputs I tried, but there must be a better way
        states = new_states.into_iter().filter(|s| s.pressure_released as i16 > leader - 100).collect();
    }

    states.into_iter().map(|s| s.pressure_released).max().unwrap()
}

fn add_elephant(states: &Vec<State>) -> Vec<State> {
    let mut ret: Vec<State> = Vec::new();
    let destinations: Vec<(u16, u8)> = states.iter().map(|s| s.destinations[0].clone()).collect();

    for state in states {
        for destination in destinations.clone() {
            if state.destinations[0] != destination {
                let mut s = state.clone();
                s.destinations.push(destination);
                ret.push(s);
            }
        }
    }

    ret
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let mut valves: IntMap<u16, Valve> = input
        .lines()
        .map(|s| {
            let valve = parse_valve(s);
            (valve.id, valve)
        })
        .collect();
    valves = build_distance_maps(valves);

    let state = State::from(valves.clone());
    let states = state.spawn_possible_states(vec![valve_name_to_id("AA")], &valves);
    let states_w_elephant = add_elephant(&states);

    (Box::new(do_ticks(states, 30, &valves)), Box::new(do_ticks(states_w_elephant, 26, &valves)))
}
