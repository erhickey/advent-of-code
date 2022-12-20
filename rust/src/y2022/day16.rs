use std::{fmt::Display, cmp::max};

use nohash_hasher::IntMap;
use nom::{bytes::complete::{tag, take}, sequence::preceded, multi::separated_list1, IResult, branch::alt};
use rustc_hash::FxHashMap;

use crate::util::{bits::u16::{is_bit_unset, set_bit, unset_bits, clear_bit}, nom::u_8, dijkstra::dijkstra_shortest};

type Valves = [Valve; 16];

#[derive(Debug, Clone, Eq, PartialEq)]
struct Valve {
    id: u8,
    name: String,
    flow_rate: u8,
    is_origin: bool,
    leads_to_strings: Vec<String>,
    leads_to: Vec<u8>,
    distances_by_name: FxHashMap<String, u8>,
    distances: [u8; 16]
}

#[derive(Clone)]
struct State {
    valve_states: u16,
    pressure_per_turn: u8,
    pressure_released: u16,
    destination: u8,
    distance: u8
}

impl State {
    fn open_valve(&mut self, valve: u8, valves: &Valves) {
        if is_bit_unset(self.valve_states, valve) {
            self.valve_states = set_bit(self.valve_states, valve);
            self.pressure_per_turn += valves[valve as usize].flow_rate;
        }
    }

    fn spawn_possible_states(&self, valves: &Valves) -> Vec<State> {
        let mut states: Vec<State> = Vec::with_capacity(unset_bits(&self.valve_states).count());

        for dest in unset_bits(&self.valve_states) {
            let mut s = self.clone();
            s.destination = dest;
            s.distance = valves[self.destination as usize].distances[dest as usize];
            states.push(s);
        }

        states
    }

    fn tick(&mut self, valves: &Valves) -> bool {
        self.release_pressure();

        if self.distance == 0 {
            self.open_valve(self.destination, valves);
            self.valve_states == u16::MAX
        } else {
            self.distance -= 1;
            true
        }
    }

    fn release_pressure(&mut self) {
        self.pressure_released += self.pressure_per_turn as u16;
    }

    fn new() -> Self {
        State {
            valve_states: 32768,
            pressure_per_turn: 0,
            pressure_released: 0,
            destination: 15,
            distance: 0
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
        name: String::from(name),
        id: 0,
        flow_rate,
        leads_to_strings: leads_to.into_iter().map(String::from).collect(),
        leads_to: Vec::new(),
        is_origin: name == "AA",
        distances_by_name: FxHashMap::default(),
        distances: [0; 16]
    }
}

fn do_ticks(mut states: Vec<State>, turns: u8, valves: &Valves) -> Vec<State> {
    for _ in 0..turns {
        let mut new_states: Vec<State> = Vec::new();

        for mut state in states {
            if state.tick(valves) {
                new_states.push(state);
            } else {
                new_states.extend(state.spawn_possible_states(valves));
            }
        }

        states = new_states;
    }

    states
}

fn pressure_released(states: Vec<State>) -> u16 {
    states.into_iter().map(|s| s.pressure_released).max().unwrap()
}

fn build_distance_maps(valves: Vec<Valve>) -> Vec<Valve> {
    let ends: Vec<String> = valves.iter().filter(|v| v.flow_rate > 0).map(|v| v.name.clone()).collect();
    let mut starts = ends.clone();
    starts.push(valves.iter().filter(|v| v.is_origin).next().unwrap().name.clone());

    let valid_neighbors: FxHashMap<String, Vec<String>> = valves
        .iter()
        .map(|v| (v.name.clone(), v.leads_to_strings.clone()))
        .collect();

    let mut ret: Vec<Valve> = Vec::new();

    for valve in &valves {
        for start in &starts {
            if start == &valve.name {
                let mut distance_map: FxHashMap<String, u8> = FxHashMap::default();
                for end in ends.clone() {
                    distance_map.insert(end.clone(), dijkstra_shortest(start, &end, &valid_neighbors) as u8);
                }
                let mut v = valve.clone();
                v.distances_by_name = distance_map;
                ret.push(v);
            }
        }
    }

    ret
}

fn assign_valve_ids(valves: Vec<Valve>) -> Valves {
    let mut id = 0;
    let mut name_to_id: FxHashMap<String, u8> = FxHashMap::default();

    let valves_w_ids: Vec<Valve> = valves.into_iter().map(|mut valve| {
        if valve.flow_rate > 0 {
            valve.id = id;
            name_to_id.insert(valve.name.clone(), valve.id);
            id += 1;
        } else if valve.is_origin {
            valve.id = 15;
            name_to_id.insert(valve.name.clone(), valve.id);
        }
        valve
    }).collect();

    let mut valves_w_dist_maps: Vec<Valve> = valves_w_ids.into_iter().map(|mut valve| {
        valve.distances_by_name
             .iter()
             .for_each(|(k, v)| valve.distances[*name_to_id.get(k).unwrap() as usize] = *v);
        valve
    }).collect();

    valves_w_dist_maps.sort_by_key(|v| v.id);
    valves_w_dist_maps.try_into().unwrap()
}

fn part2(states: &Vec<State>) -> u16 {
    let mut best_results: IntMap<u16, u16> = IntMap::default();

    for state in states {
        best_results
            .entry(clear_bit(state.valve_states, 15))
            .and_modify(|pr| *pr = max(*pr, state.pressure_released))
            .or_insert(state.pressure_released);
    }

    let scores: Vec<(u16, u16)> = best_results.into_iter().collect();
    let mut highest_score = 0;

    for (ok, ov) in &scores {
        for (ik, iv) in &scores {
            if ok & ik == 0 {
                if ov + iv > highest_score {
                    highest_score = ov + iv;
                }
            }
        }
    }

    highest_score
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let mut prebuilt_valves: Vec<Valve> = input.lines().map(parse_valve).collect();
    prebuilt_valves = build_distance_maps(prebuilt_valves);
    let valves = assign_valve_ids(prebuilt_valves);

    let state = State::new();
    let states = state.spawn_possible_states(&valves);
    let tick_26 = do_ticks(states, 26, &valves);
    let part2 = part2(&tick_26);

    (Box::new(pressure_released(do_ticks(tick_26, 4, &valves))), Box::new(part2))
}
