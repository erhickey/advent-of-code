use std::fmt::Display;

use nom::{bytes::complete::{tag, take}, sequence::preceded, multi::separated_list1, IResult, branch::alt};
use rustc_hash::FxHashMap;

use crate::util::{bits::u16::{is_bit_set, set_bit, unset_bits}, nom::u_8, dijkstra::dijkstra_shortest};

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
    distance: u8,
    el_destination: u8,
    el_distance: u8,
    has_elephant: bool
}

impl State {
    fn open_valve(&mut self, valve: u8, valves: &Valves) {
        if !is_bit_set(self.valve_states, valve) {
            self.valve_states = set_bit(self.valve_states, valve);
            self.pressure_per_turn += valves[valve as usize].flow_rate;
        }
    }

    fn spawn_possible_states(&self, valves: &Valves) -> Vec<State> {
        let mut states: Vec<State> = Vec::new();

        if self.has_elephant && self.el_distance == 0 && self.distance == 0 {
            let closed_valves: Vec<u8> = unset_bits(&self.valve_states).collect();
            for closed1 in &closed_valves {
                for closed2 in &closed_valves {
                    if closed2 > closed1 {
                        let mut s1 = self.clone();
                        s1.destination = *closed1;
                        s1.el_destination = *closed2;
                        s1.distance = valves[self.destination as usize].distances[*closed1 as usize];
                        s1.el_distance = valves[self.el_destination as usize].distances[*closed2 as usize];
                        states.push(s1);

                        let mut s2 = self.clone();
                        s2.destination = *closed2;
                        s2.el_destination = *closed1;
                        s2.distance = valves[self.destination as usize].distances[*closed2 as usize];
                        s2.el_distance = valves[self.el_destination as usize].distances[*closed1 as usize];
                        states.push(s2);

                    }
                }
            }
        } else {
            for dest in unset_bits(&self.valve_states) {
                let mut s = self.clone();
                if self.distance == 0 {
                    s.destination = dest;
                    s.distance = valves[self.destination as usize].distances[dest as usize];
                } else {
                    s.el_destination = dest;
                    s.el_distance = valves[self.el_destination as usize].distances[dest as usize];
                }
                states.push(s);
            }
        }

        states
    }

    fn tick(&mut self, valves: &Valves) -> bool {
        self.release_pressure();

        let mut valve_opened = false;

        if self.distance == 0 {
            self.open_valve(self.destination, valves);
            valve_opened = true;
        } else {
            self.distance -= 1;
        }

        if self.has_elephant {
            if self.el_distance == 0 {
                self.open_valve(self.el_destination, valves);
                valve_opened = true;
            } else {
                self.el_distance -= 1;
            }
        }

        !valve_opened || self.valve_states == u16::MAX
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
            distance: 0,
            el_destination: 15,
            el_distance: u8::MAX,
            has_elephant: false
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

fn do_ticks(mut states: Vec<State>, turns: u8, valves: &Valves) -> u16 {
    for n in 0..turns {
        println!("{} {}", n, states.len());
        let mut new_states: Vec<State> = Vec::new();

        for mut state in states {
            if state.tick(valves) {
                new_states.push(state);
            } else {
                new_states.extend(state.spawn_possible_states(valves));
            }
        }

        let leader: i16 = new_states.iter().map(|s| s.pressure_released).max().unwrap() as i16;
        // arbitrarily reduce the number of paths to consider
        // this works, but there must be a better way
        states = new_states.into_iter().filter(|s| s.pressure_released as i16 > leader - 150).collect();
    }

    states.into_iter().map(|s| s.pressure_released).max().unwrap()
}

fn add_elephant(states: &Vec<State>) -> Vec<State> {
    let mut ret: Vec<State> = Vec::new();
    let destinations: Vec<(u8, u8)> = states.iter().map(|s| (s.destination, s.distance)).collect();

    for state in states {
        for destination in &destinations {
            if state.destination != destination.0 {
                let mut s = state.clone();
                s.el_destination = destination.0;
                s.el_distance = destination.1;
                s.has_elephant = true;
                ret.push(s);
            }
        }
    }

    ret
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

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let mut prebuilt_valves: Vec<Valve> = input.lines().map(parse_valve).collect();
    prebuilt_valves = build_distance_maps(prebuilt_valves);
    let valves = assign_valve_ids(prebuilt_valves);

    let state = State::new();
    let states = state.spawn_possible_states(&valves);
    let states_w_elephant = add_elephant(&states);

    (Box::new(do_ticks(states, 30, &valves)), Box::new(do_ticks(states_w_elephant, 26, &valves)))
}
