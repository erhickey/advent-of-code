use std::fmt::Display;

use nom::{
    sequence::{terminated, delimited, preceded},
    character::complete::{newline, u8, u32},
    bytes::complete::tag, multi::separated_list1,
    combinator::map,
    IResult,
    branch::alt
};

#[derive(Clone)]
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

type Monkeys = Vec<Monkey>;

#[derive(Clone)]
struct Monkey {
    id: u8,
    operation: Operation,
    divisor: u32,
    divisible_target: u8,
    not_divisible_target: u8
}

impl Monkey {
    fn throw_target(&self, n: &u32, do_mod: bool) -> u8 {
        let val = if do_mod { *n % self.divisor } else { *n };
        if val == 0 { self.divisible_target } else { self.not_divisible_target }
    }
}

type Items = Vec<Item>;

#[derive(Clone)]
struct Item {
    owner: u8,
    values: Vec<(Monkey, u32)>
}

impl Item {
    fn from(value: u32, monkeys: &Monkeys, owner: u8) -> Item {
        Item {
            owner,
            values: monkeys.iter().map(|m| (m.clone(), value)).collect()
        }
    }

    fn inspect(&mut self, current_monkey: &Monkey, decrease_worry: bool) -> u128 {
        if self.owner != current_monkey.id {
            return 0;
        }

        for (monkey, v) in self.values.iter_mut() {
            if decrease_worry {
                *v = current_monkey.operation.execute(&v) / 3;
            } else {
                *v = (current_monkey.operation.execute(&v)) % monkey.divisor;
            }
            if monkey.id == current_monkey.id {
                self.owner = current_monkey.throw_target(&v, decrease_worry);
            }
        }

        1
    }
}

struct State {
    monkeys: Monkeys,
    max_monkey: usize,
    active_monkey: usize,
    inspect_count: Vec<u128>,
    items: Items,
    decrease_worry: bool
}

impl State {
    fn from(monkeys: Monkeys, items: Items, decrease_worry: bool) -> State {
        State {
            max_monkey: (&monkeys.len() - 1),
            inspect_count: vec![0; monkeys.len()],
            monkeys,
            active_monkey: 0,
            items,
            decrease_worry
        }
    }

    fn take_turn(&mut self) {
        let monkey = &self.monkeys[self.active_monkey];
        let mut inspect_count = 0;

        for item in self.items.iter_mut() {
            inspect_count += item.inspect(monkey, self.decrease_worry);
        }

        self.inspect_count[self.active_monkey] = self.inspect_count[self.active_monkey] + inspect_count;
        self.active_monkey = self.next_monkey();
    }

    fn next_monkey(&self) -> usize {
        match self.active_monkey == self.max_monkey {
            true => 0,
            false => self.active_monkey + 1
        }
    }

    fn calc_monkey_business(&self) -> u128 {
        let mut sorted_values = self.inspect_count.clone();
        sorted_values.sort();
        sorted_values.into_iter().rev().take(2).product()
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
    let monkeys: Monkeys = result.clone().map(|(m, _)| m).collect();
    let items = result.fold(Vec::new(), |mut acc, (m, is)| {
        is.into_iter().for_each(|item| acc.push(Item::from(item, &monkeys, m.id)));
        acc
    });
    (monkeys, items)
}

fn run(monkeys: Monkeys, items: Items, decrease_worry: bool, rounds: usize) -> u128 {
    let mut state = State::from(monkeys, items, decrease_worry);
    for _ in 0..rounds * state.monkeys.len() {
        state.take_turn();
    }
    state.calc_monkey_business()
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let (monkeys, items) = parse_monkeys(input);

    (
        Box::new(run(monkeys.clone(), items.clone(), true, 20)),
        Box::new(run(monkeys, items, false, 10000))
    )
}
