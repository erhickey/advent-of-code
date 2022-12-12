use std::{fmt::Display, collections::HashMap};

use nom::{
    sequence::{terminated, delimited, preceded},
    character::complete::{newline, u8},
    bytes::complete::tag, multi::separated_list1,
    combinator::map,
    IResult,
    branch::alt
};
use num::BigUint;

#[derive(Clone, Eq, Hash, PartialEq)]
enum Operation {
    Add(u8),
    Multiply(u8),
    Square
}

impl Operation {
    fn execute(&self, n: &BigUint) -> BigUint {
        match self {
            Operation::Add(x) => x + n,
            Operation::Multiply(x) => x * n,
            Operation::Square => n * n
        }
    }
}

struct Turn {
    active_monkey: u8,
    before: Monkeys,
    after: Monkeys
}

impl Turn {
    fn num_items_inspected(&self) -> u128 {
        self.before.get(&self.active_monkey).unwrap().items.len() as u128
    }
}

type Monkeys = HashMap<u8, Monkey>;

#[derive(Clone, Eq, Hash, PartialEq)]
struct Monkey {
    id: u8,
    items: Vec<BigUint>,
    operation: Operation,
    divisible_test: u8,
    success_target: u8,
    failure_target: u8
}

impl Monkey {
    fn inspect_items(&self) -> Vec<BigUint> {
        self.items.iter().map(|n| self.operation.execute(n)).collect()
    }

    fn test(&self, n: &BigUint) -> bool {
        n % self.divisible_test == num::zero()
    }

    fn test_target(&self, n: &BigUint) -> u8 {
        if self.test(n) { self.success_target } else { self.failure_target }
    }

    fn clone_with_no_items(&self) -> Monkey {
        let mut monkey = self.clone();
        monkey.items = Vec::new();
        monkey
    }

    fn clone_with_items(&self, items: Vec<(u8, BigUint)>) -> Monkey {
        let mut new_items = self.items.clone();
        for (ix, item) in items {
            if ix == self.id {
                new_items.push(item);
            }
        }
        let mut monkey = self.clone();
        monkey.items = new_items;
        monkey
    }
}

fn parse_id(s: &str) -> IResult<&str, u8> {
    delimited(tag("Monkey "), u8, tag(":"))(s)
}

fn parse_items(s: &str) -> IResult<&str, Vec<u8>> {
    preceded(tag("  Starting items: "), separated_list1(tag(", "), u8))(s)
}

fn parse_divisible(s: &str) -> IResult<&str, u8> {
    preceded(tag("  Test: divisible by "), u8)(s)
}

fn parse_success_target(s: &str) -> IResult<&str, u8> {
    preceded(tag("    If true: throw to monkey "), u8)(s)
}

fn parse_failure_target(s: &str) -> IResult<&str, u8> {
    preceded(tag("    If false: throw to monkey "), u8)(s)
}

fn parse_operation(s: &str) -> IResult<&str, Operation> {
    let add = map(preceded(tag("  Operation: new = old + "), u8), Operation::Add);
    let mult = map(preceded(tag("  Operation: new = old * "), u8), Operation::Multiply);
    let square = map(preceded(tag("  Operation: new = old * "), tag("old")), |_| Operation::Square);

    alt((add, mult, square))(s)
}

fn parse_monkey(s: &str) -> Monkey {
    let (s, id) = terminated(parse_id, newline)(s).unwrap();
    let (s, items) = terminated(parse_items, newline)(s).unwrap();
    let (s, operation) = terminated(parse_operation, newline)(s).unwrap();
    let (s, divisible_test) = terminated(parse_divisible, newline)(s).unwrap();
    let (s, success_target) = terminated(parse_success_target, newline)(s).unwrap();
    let (_, failure_target) = parse_failure_target(s).unwrap();

    Monkey {
        id,
        items: items.iter().map(|n| BigUint::new(vec![*n as u32])).collect(),
        operation,
        divisible_test,
        success_target,
        failure_target
    }
}

fn parse_monkeys(s: &str) -> Monkeys {
    s.split("\n\n").map(parse_monkey).map(|m| (m.id, m)).collect()
}

fn do_turn(monkeys: &Monkeys, monkey_id: &u8, decrease_worry: bool) -> Turn {
    let monkey = monkeys.get(monkey_id).unwrap();

    let items: Vec<(u8, BigUint)> = monkey.inspect_items().iter().map(|item| {
        let new_item = if decrease_worry { item / BigUint::new(vec![3]) } else { item.clone() };
        (monkey.test_target(&new_item), new_item)
    }).collect();

    Turn {
        active_monkey: *monkey_id,
        before: monkeys.clone(),
        after: monkeys.into_iter().map(|(ix, m)| {
            if ix == monkey_id {
                (*ix, m.clone_with_no_items())
            } else {
                (*ix, m.clone_with_items(items.clone()))
            }
        }).collect()
    }
}

fn do_round(monkeys: &Monkeys, decrease_worry: bool) -> Vec<Turn> {
    let mut keys: Vec<&u8> = monkeys.keys().collect();
    keys.sort();
    keys.into_iter().fold(Vec::new(), |mut acc, key| {
        let last_turn = if acc.len() == 0 { monkeys } else { &acc.iter().last().unwrap().after };
        acc.push(do_turn(last_turn, key, decrease_worry));
        acc
    })
}

fn do_rounds(monkeys: &Monkeys, rounds: u32, decrease_worry: bool) -> Vec<Turn> {
    (0..rounds)
        .fold(Vec::new(), |mut acc, _| {
            let last_turn = if acc.len() == 0 { monkeys } else { &acc.iter().last().unwrap().after };
            acc.extend(do_round(last_turn, decrease_worry));
            acc
        })
}

fn calc_monkey_business(turns: Vec<Turn>) -> u128 {
    let mut values: Vec<u128> = turns.iter().fold(HashMap::new(), |mut acc, t| {
        acc
            .entry(t.active_monkey)
            .and_modify(|c| *c = (*c as u128) + t.num_items_inspected()).or_insert(t.num_items_inspected());
        acc
    }).into_values().collect();
    values.sort();
    values.iter().rev().take(2).product()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let monkeys = parse_monkeys(input);

    (
        Box::new(calc_monkey_business(do_rounds(&monkeys, 20, true))),
        Box::new(calc_monkey_business(do_rounds(&monkeys, 10000, false)))
    )
}
