module Y2020.Day18 (solve) where

data Component = Operand Int | Add | Multiply | OpenParen | CloseParen

type Expression = [Component]

parseLine :: String -> Expression
parseLine = map parseComponent . filter (/=' ')

parseComponent :: Char -> Component
parseComponent '+' = Add
parseComponent '*' = Multiply
parseComponent '(' = OpenParen
parseComponent ')' = CloseParen
parseComponent c = Operand $ read [c]

evaluate :: Expression -> Int
evaluate = fst . go (0, [])
  where
    go acc [] = acc
    go (_, rem) (Operand n:xs) = go (n, rem) xs
    go (a, rem) (Add:Operand n:xs) = go (a + n, rem) xs
    go (a, rem) (Add:OpenParen:xs) = let (a', rem') = go (0, []) xs in go (a + a', rem) rem'
    go (a, rem) (Multiply:Operand n:xs) = go (a * n, rem) xs
    go (a, rem) (Multiply:OpenParen:xs) = let (a', rem') = go (0, []) xs in go (a * a', rem) rem'
    go (_, rem) (OpenParen:xs) = let (a', rem') = go (0, []) xs in go (a', rem) rem'
    go (a, _) (CloseParen:xs) = (a, xs)

prioritizeAddition :: Expression -> Expression
prioritizeAddition = go []
  where
    go acc (Operand n:Add:xs) = go (acc ++ [OpenParen,Operand n,Add]) $ insertCloseParen xs
    go acc (CloseParen:Add:xs) = go (insertOpenParen acc ++ [CloseParen,Add]) $ insertCloseParen xs
    go acc (x:xs) = go (acc ++ [x]) xs
    go acc [] = acc
    insertOpenParen = insertMatchingOpen (0, []) . reverse
    insertCloseParen (Operand n:xs) = Operand n:CloseParen:xs
    insertCloseParen (OpenParen:xs) = OpenParen:insertMatchingClose (0, []) xs
    insertMatchingClose (0, acc) [] = reverse $ CloseParen:acc
    insertMatchingClose (0, acc) (CloseParen:xs) = reverse (CloseParen:CloseParen:acc) ++ xs
    insertMatchingClose (n, acc) (CloseParen:xs) = insertMatchingClose (n - 1, CloseParen:acc) xs
    insertMatchingClose (n, acc) (OpenParen:xs) = insertMatchingClose (n + 1, OpenParen:acc) xs
    insertMatchingClose (n, acc) (x:xs) = insertMatchingClose (n, x:acc) xs
    insertMatchingOpen (0, acc) [] = OpenParen:acc
    insertMatchingOpen (0, acc) (OpenParen:xs) = reverse xs ++ OpenParen:OpenParen:acc
    insertMatchingOpen (n, acc) (OpenParen:xs) = insertMatchingOpen (n - 1, OpenParen:acc) xs
    insertMatchingOpen (n, acc) (CloseParen:xs) = insertMatchingOpen (n + 1, CloseParen:acc) xs
    insertMatchingOpen (n, acc) (x:xs) = insertMatchingOpen (n, x:acc) xs

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    es = map parseLine $ lines input
    part1 = show . sum $ map evaluate es
    part2 = show . sum $ map (evaluate . prioritizeAddition) es
