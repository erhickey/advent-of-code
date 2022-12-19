pub fn is_bit_set(bytes: u16, n: u8) -> bool {
    0 != bytes & (1 << n)
}

pub fn set_bit(mut bytes: u16, n: u8) -> u16 {
    bytes |= 1 << n;
    bytes
}

#[allow(dead_code)]
pub fn clear_bit(mut bytes: u16, n: u8) -> u16 {
    bytes &= !(1 << n);
    bytes
}

#[allow(dead_code)]
pub fn toggle_bit(mut bytes: u16, n: u8) -> u16 {
    bytes ^= 1 << n;
    bytes
}

#[allow(dead_code)]
pub fn is_even(n: u16) -> bool {
    n&1 == 0
}

pub fn unset_bits<'a>(bytes: &'a u16) -> impl Iterator<Item=u8> + 'a {
    (0..16).filter(|n| !is_bit_set(*bytes, *n))
}
