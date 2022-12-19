#[allow(dead_code)]
pub fn is_bit_set(bytes: i32, n: u8) -> bool {
    0 != bytes & (1 << n)
}

#[allow(dead_code)]
pub fn set_bit(mut bytes: i32, n: u8) -> i32 {
    bytes |= 1 << n;
    bytes
}

#[allow(dead_code)]
pub fn clear_bit(mut bytes: i32, n: u8) -> i32 {
    bytes &= !(1 << n);
    bytes
}

#[allow(dead_code)]
pub fn toggle_bit(mut bytes: i32, n: u8) -> i32 {
    bytes ^= 1 << n;
    bytes
}

#[allow(dead_code)]
pub fn is_even(n: i32) -> bool {
    n&1 == 0
}

#[allow(dead_code)]
pub fn is_negative(n: i32) -> bool {
    (n>>31)&1 == 1
}
