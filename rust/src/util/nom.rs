use nom::{character::complete::{char, u8, u16, u32}, IResult, combinator::opt};

pub fn i32(s: &str) -> IResult<&str, i32> {
    let (s, sign) = opt(char('-'))(s)?;
    let (s, n) = u32(s)?;
    Ok((s, if sign.is_some() { -1 * n as i32 } else { n as i32 }))
}

#[allow(dead_code)]
pub fn u_32(s: &str) -> IResult<&str, u32> {
    u32(s)
}

#[allow(dead_code)]
pub fn u_16(s: &str) -> IResult<&str, u16> {
    u16(s)
}

pub fn u_8(s: &str) -> IResult<&str, u8> {
    u8(s)
}
