use nom::{character::complete::{char, u32}, IResult, combinator::opt};

pub fn i32(s: &str) -> IResult<&str, i32> {
    let (s, sign) = opt(char('-'))(s)?;
    let (s, n) = u32(s)?;
    Ok((s, if sign.is_some() { -1 * n as i32 } else { n as i32 }))
}

pub fn u_32(s: &str) -> IResult<&str, u32> {
    u32(s)
}
