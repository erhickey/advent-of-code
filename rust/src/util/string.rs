pub fn bisect(s: &str) -> Vec<Vec<char>> {
    let (s1, s2) = s.split_at(s.len() / 2);
    vec![s1.chars().collect(), s2.chars().collect()]
}
