use std::fmt::Display;

type GiftBox = (u64, u64, u64);

fn box_from_string(s: &str) -> GiftBox {
    let t: Vec<u64> = s.split('x').map(|e| e.parse::<u64>().unwrap()).collect();
    (t[0], t[1], t[2])
}

fn surface_area((x, y, z): &GiftBox) -> u64 {
    let side1 = x * y;
    let side2 = y * z;
    let side3 = x * z;
    let sides = [side1, side2, side3];
    [side1 * 2, side2 * 2, side3 * 2, *sides.iter().min().unwrap()].iter().sum()
}

fn ribbon_length((x, y, z): &GiftBox) -> u64 {
    let mut ss = [x, y, z];
    ss.sort();
    x * y * z + (2 * ss[0]) + (2 * ss[1])
}

pub fn solve(input: &str) -> (Box<dyn Display>, Box<dyn Display>) {
    let boxes: Vec<GiftBox> = input.lines().map(box_from_string).collect();
    (Box::new(boxes.iter().map(surface_area).sum::<u64>()), Box::new(boxes.iter().map(ribbon_length).sum::<u64>()))
}
