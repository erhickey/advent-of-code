use std::fmt::Display;

use nom::{bytes::complete::{tag, take}, sequence::preceded, multi::separated_list1, IResult, branch::alt};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::util::nom::u_32;

#[derive(Clone, Eq, Hash, PartialEq)]
struct Valve {
    name: String,
    flow_rate: u32,
    leads_to: Vec<String>
}

#[derive(Clone)]
struct State {
    valves: FxHashMap<String, Valve>,
    closed_valves: FxHashSet<String>,
    open_valves: FxHashSet<Valve>,
    current_valve: String,
    last_valve: String,
    pressure_released: u32,
    all_valves_open: bool,
    elapsed_time: u8
}

impl State {
    fn open_valve(&mut self) {
        self.release_pressure();
        self.closed_valves.remove(&self.current_valve);
        self.open_valves.insert(self.valves.get(&self.current_valve).unwrap().clone());
        self.last_valve = String::from("");
        if self.closed_valves.len() == 0 {
            self.all_valves_open = true;
        }
        self.elapsed_time += 1;
    }

    fn move_to(&mut self, valve: String) {
        self.release_pressure();
        self.last_valve = self.current_valve.clone();
        self.current_valve = valve;
        self.elapsed_time += 1;
    }

    fn wait(&mut self) {
        self.release_pressure();
        self.elapsed_time += 1;
    }

    fn release_pressure(&mut self) {
        self.pressure_released += self.open_valves.iter().map(|v| v.flow_rate).sum::<u32>();
    }

    fn can_open_valve(&self) -> bool {
        self.closed_valves.contains(&self.current_valve)
    }

    fn valve(&self) -> &Valve {
        self.valves.get(&self.current_valve).unwrap()
    }
}

impl From<FxHashMap<String, Valve>> for State {
    fn from(valves: FxHashMap<String, Valve>) -> Self {
        let closed_valves = valves.clone().into_values().filter(|v| v.flow_rate > 0).map(|v| v.name).collect();
        State {
            valves,
            closed_valves,
            open_valves: FxHashSet::default(),
            current_valve: String::from("AA"),
            last_valve: String::from(""),
            pressure_released: 0,
            all_valves_open: false,
            elapsed_time: 0
        }
    }
}

fn parse_valve_name(s: &str) -> IResult<&str, &str> {
    take(2usize)(s)
}

fn parse_valve(s: &str) -> Valve {
    let (s, name) = preceded(tag("Valve "), parse_valve_name)(s).unwrap();
    let (s, flow_rate) = preceded(tag(" has flow rate="), u_32)(s).unwrap();
    let (_, leads_to) = preceded(
        alt((tag("; tunnel leads to valve "), tag("; tunnels lead to valves "))),
        separated_list1(tag(", "), parse_valve_name)
    )(s).unwrap();
    Valve { name: String::from(name), flow_rate, leads_to: leads_to.into_iter().map(|s| String::from(s)).collect() }
}

fn take_turn(state: State) -> Vec<State> {
    if state.all_valves_open {
        let mut s = state.clone();
        s.wait();
        return vec![s];
    }

    let mut paths: Vec<State> = Vec::new();

    if state.can_open_valve() {
        let mut s = state.clone();
        s.open_valve();
        let all_valves_open = s.all_valves_open;
        paths.push(s);

        if all_valves_open {
            return paths;
        }
    }

    for tunnel in state.valve().leads_to.clone() {
        if tunnel != state.last_valve {
            let mut s = state.clone();
            s.move_to(tunnel);
            paths.push(s);
        }
    }

    paths
}

fn take_turns(state: State) -> Vec<State> {
    let mut states = vec![state];

    for n in 0..30 {
        states = states
            .into_iter()
            .flat_map(|s| take_turn(s))
            .filter(|s| s.open_valves.len() >= n / 4 || s.closed_valves.is_empty())
            .collect();
    }

    states
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let valves: FxHashMap<String, Valve> = input
        .lines()
        .map(|s| {
            let valve = parse_valve(s);
            (valve.name.clone(), valve)
        })
        .collect();

    let part1 = take_turns(State::from(valves)).iter().map(|s| s.pressure_released).max().unwrap();

    (Box::new(part1), Box::new(0))
}
