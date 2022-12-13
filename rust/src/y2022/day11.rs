use std::{fmt::Display, collections::HashMap};

use nom::{
    sequence::{terminated, delimited, preceded},
    character::complete::{newline, u8, u32},
    bytes::complete::tag, multi::separated_list1,
    combinator::map,
    IResult,
    branch::alt
};

enum Operation {
    Add(u32),
    Multiply(u32),
    Square
}

impl Operation {
    fn execute(&self, n: &u32) -> u32 {
        match self {
            Operation::Add(x) => x + n,
            Operation::Multiply(x) => x * n,
            Operation::Square => n * n
        }
    }
}

type Monkeys = HashMap<u8, Monkey>;

struct Monkey {
    id: u8,
    operation: Operation,
    divisor: u32,
    divisible_target: u8,
    not_divisible_target: u8
}

impl Monkey {
    fn throw_target(&self, n: &u32) -> u8 {
        if *n == 0 { self.divisible_target } else { self.not_divisible_target }
    }
}

type Items = Vec<Item>;

#[derive(Clone)]
struct Item {
    owner: u8,
    values: HashMap<u8, u32>
}

impl Item {
    fn from(value: u32, monkeys: &Monkeys, owner: u8) -> Item {
        Item {
            owner,
            values: monkeys.values().map(|m| (m.id, value)).collect()
        }
    }

    fn inspect(&self, monkeys: &Monkeys, current_monkey: &u8, decrease_worry: bool) -> Item {
        if self.owner != *current_monkey {
            return self.clone();
        }

        let owner = monkeys.get(&self.owner).unwrap();

        let new_values: HashMap<u8, u32> = self.values.clone().into_iter().map(|(k, v)| {
            let monkey = monkeys.get(&k).unwrap();
            let new_value: f32 = owner.operation.execute(&v) as f32 / (if decrease_worry { 3.0 } else { 1.0 });
            (k, new_value.floor() as u32 % monkey.divisor)
        }).collect();

        Item {
            owner: owner.throw_target(new_values.get(&owner.id).unwrap()),
            values: new_values
        }
    }
}

struct Turn {
    active_monkey: u8,
    before: Items,
    after: Items
}

impl Turn {
    fn num_items_inspected(&self) -> usize {
        self.before.iter().filter(|item| item.owner == self.active_monkey).count()
    }
}

fn parse_id(s: &str) -> IResult<&str, u8> {
    delimited(tag("Monkey "), u8, tag(":"))(s)
}

fn parse_items(s: &str) -> IResult<&str, Vec<u32>> {
    preceded(tag("  Starting items: "), separated_list1(tag(", "), u32))(s)
}

fn parse_divisible(s: &str) -> IResult<&str, u32> {
    preceded(tag("  Test: divisible by "), u32)(s)
}

fn parse_divisible_target(s: &str) -> IResult<&str, u8> {
    preceded(tag("    If true: throw to monkey "), u8)(s)
}

fn parse_not_divisible_target(s: &str) -> IResult<&str, u8> {
    preceded(tag("    If false: throw to monkey "), u8)(s)
}

fn parse_operation(s: &str) -> IResult<&str, Operation> {
    let add = map(preceded(tag("  Operation: new = old + "), u32), Operation::Add);
    let mult = map(preceded(tag("  Operation: new = old * "), u32), Operation::Multiply);
    let square = map(preceded(tag("  Operation: new = old * "), tag("old")), |_| Operation::Square);

    alt((add, mult, square))(s)
}

fn parse_monkey(s: &str) -> (Monkey, Vec<u32>) {
    let (s, id) = terminated(parse_id, newline)(s).unwrap();
    let (s, items) = terminated(parse_items, newline)(s).unwrap();
    let (s, operation) = terminated(parse_operation, newline)(s).unwrap();
    let (s, divisor) = terminated(parse_divisible, newline)(s).unwrap();
    let (s, divisible_target) = terminated(parse_divisible_target, newline)(s).unwrap();
    let (_, not_divisible_target) = parse_not_divisible_target(s).unwrap();

    (Monkey {
        id,
        operation,
        divisor,
        divisible_target,
        not_divisible_target
    }, items)
}

fn parse_monkeys(s: &str) -> (Monkeys, Items) {
    let result = s.split("\n\n").map(parse_monkey);
    let monkeys: Monkeys = result.clone().map(|(m, _)| (m.id, m)).collect();
    let items = result.fold(Vec::new(), |mut acc, (m, is)| {
        is.into_iter().for_each(|item| acc.push(Item::from(item, &monkeys, m.id)));
        acc
    });
    (monkeys, items)
}

fn do_turn(monkeys: &Monkeys, items: &Items, current_monkey: &u8, decrease_worry: bool) -> Turn {
    Turn {
        active_monkey: *current_monkey,
        before: items.clone(),
        after: items.into_iter().map(|item| item.inspect(monkeys, current_monkey, decrease_worry)).collect()
    }
}

fn do_round(monkeys: &Monkeys, items: &Items, decrease_worry: bool) -> Vec<Turn> {
    let mut keys: Vec<&u8> = monkeys.keys().collect();
    keys.sort();
    keys.into_iter().fold(Vec::new(), |mut acc, key| {
        let last_turn = if acc.len() == 0 { items } else { &acc.iter().last().unwrap().after };
        acc.push(do_turn(monkeys, last_turn, key, decrease_worry));
        acc
    })
}

fn do_rounds(monkeys: &Monkeys, items: &Items, rounds: u32, decrease_worry: bool) -> Vec<Turn> {
    (0..rounds)
        .fold(Vec::new(), |mut acc, _| {
            let last_turn = if acc.len() == 0 { items } else { &acc.iter().last().unwrap().after };
            acc.extend(do_round(monkeys, last_turn, decrease_worry));
            acc
        })
}

fn calc_monkey_business(turns: Vec<Turn>) -> usize {
    let mut values: Vec<usize> = turns.iter().fold(HashMap::new(), |mut acc, t| {
        acc
            .entry(t.active_monkey)
            .and_modify(|c| *c += t.num_items_inspected())
            .or_insert(t.num_items_inspected());
        acc
    }).into_values().collect();
    values.sort();
    values.iter().rev().take(2).product()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let (monkeys, items) = parse_monkeys(input);

    (
        Box::new(calc_monkey_business(do_rounds(&monkeys, &items, 20, true))),
        Box::new(calc_monkey_business(do_rounds(&monkeys, &items, 10000, false)))
    )
}
