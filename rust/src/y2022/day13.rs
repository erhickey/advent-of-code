use std::{fmt::Display, cmp::Ordering};

use nom::{
    sequence::delimited,
    character::complete::u32,
    bytes::complete::tag, multi::separated_list0,
    IResult,
    branch::alt
};

type Packets = (PacketValue, PacketValue);

#[derive(Clone, Debug, Eq, PartialEq)]
enum PacketValue {
    Int(u32),
    List(Vec<PacketValue>)
}

fn parse_int(s: &str) -> IResult<&str, PacketValue> {
    let (s, i) = u32(s)?;
    Ok((s, PacketValue::Int(i)))
}

fn parse_list(s: &str) -> IResult<&str, PacketValue> {
    let (s, pvs) = delimited(tag("["), separated_list0(tag(","), alt((parse_list, parse_int))), tag("]"))(s)?;
    Ok((s, PacketValue::List(pvs)))
}

fn parse_packet(s: &str) -> PacketValue {
    let (_, p) = parse_list(s).unwrap();
    p
}

fn parse_packets(s: &str) -> Packets {
    let [p1, p2]: [PacketValue; 2] = s.lines().map(parse_packet).collect::<Vec<PacketValue>>().try_into().unwrap();
    (p1, p2)
}

fn compare_packet_value(pv1: &PacketValue, pv2: &PacketValue) -> Ordering {
    match pv1 {
        PacketValue::Int(i1) => match pv2 {
            PacketValue::Int(i2) if i1 < i2 => Ordering::Less,
            PacketValue::Int(i2) if i1 > i2 => Ordering::Greater,
            PacketValue::Int(_) => Ordering::Equal,
            PacketValue::List(_) => compare_packet_value(&PacketValue::List(vec![pv1.clone()]), pv2)
        },
        PacketValue::List(l1) =>
            match pv2 {
                PacketValue::Int(_) => compare_packet_value(pv1, &PacketValue::List(vec![pv2.clone()])),
                PacketValue::List(l2) if l1.is_empty() && l2.is_empty() => Ordering::Equal,
                PacketValue::List(_) if l1.is_empty() => Ordering::Less,
                PacketValue::List(l2) if l2.is_empty() => Ordering::Greater,
                PacketValue::List(l2) => {
                    let mut l1_m = l1.clone();
                    let mut l2_m = l2.clone();
                    let result = compare_packet_value(&l1_m.remove(0), &l2_m.remove(0));

                    if result == Ordering::Equal {
                        compare_packet_value(&PacketValue::List(l1_m), &PacketValue::List(l2_m))
                    } else {
                        result
                    }
                }
            }
    }
}

fn is_in_order((p1, p2): &Packets) -> bool {
    compare_packet_value(p1, p2) != Ordering::Greater
}

fn part1(packets: &Vec<Packets>) -> usize {
    packets
        .iter()
        .enumerate()
        .map(|(ix, ps)| (ix + 1, is_in_order(ps)))
        .filter(|(_, b)| *b)
        .map(|(ix, _)| ix)
        .sum()
}

fn part2(packets: Vec<Packets>) -> usize {
    let divider_packet_1 = PacketValue::List(vec![PacketValue::List(vec![PacketValue::Int(2)])]);
    let divider_packet_2 = PacketValue::List(vec![PacketValue::List(vec![PacketValue::Int(6)])]);

    let mut part2_packets: Vec<PacketValue> = packets
        .into_iter()
        .flat_map(|(p1, p2)| vec![p1, p2])
        .collect();
    part2_packets.push(divider_packet_1.clone());
    part2_packets.push(divider_packet_2.clone());
    part2_packets.sort_by(|pv1, pv2| compare_packet_value(pv1, pv2));

    let divider_packet_1_index = part2_packets.iter().position(|pv| pv == &divider_packet_1).unwrap() + 1;
    let divider_packet_2_index = part2_packets.iter().position(|pv| pv == &divider_packet_2).unwrap() + 1;

    divider_packet_1_index * divider_packet_2_index
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let packets: Vec<Packets> = input.split("\n\n").map(parse_packets).collect();

    (Box::new(part1(&packets)), Box::new(part2(packets)))
}
