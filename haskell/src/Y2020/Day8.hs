module Y2020.Day8 (solve) where

import Data.Sequence ((><), Seq, viewl, ViewL(..))
import qualified Data.Sequence as Sq (foldlWithIndex, fromList, length, splitAt, update)
import Data.Set (Set)
import qualified Data.Set as St (empty, insert, member)

data Opcode = Acc | Nop | Jmp

type Instruction = (Opcode, Int)

type State = (Seq Instruction, Set Int, Int, Int)

parseLine :: String -> Instruction
parseLine ('n':xs) = (Nop, parseArg $ drop 3 xs)
parseLine ('j':xs) = (Jmp, parseArg $ drop 3 xs)
parseLine ('a':xs) = (Acc, parseArg $ drop 3 xs)

parseArg :: String -> Int
parseArg ('+':xs) = parseArg xs
parseArg s = read s

accumulate :: Instruction -> Int -> Int
accumulate (Acc, a) n = n + a
accumulate _ n = n

nextIndex :: Instruction -> Int
nextIndex (Jmp, n) = n
nextIndex _ = 1

move :: Int -> Seq a -> Seq a
move n s = r >< l
  where (l, r) = Sq.splitAt (n `mod` Sq.length s) s

runNext :: State -> State
runNext (sq, st, ix, acc) = (sq', st', ix', acc')
  where relativeIndex = nextIndex instruction
        (instruction :< _) = viewl sq
        acc' = accumulate instruction acc
        sq' = move relativeIndex sq
        ix' = ix + relativeIndex
        st' = St.insert ix st

accumulator :: State -> Int
accumulator (_, _, _, n) = n

runWhile :: (State -> Bool) -> Seq Instruction -> State
runWhile f sq = last $ takeWhile f run
  where run = iterate runNext (sq, St.empty, 0, 0)

newInstruction :: State -> Bool
newInstruction (_, st, ix, _) = not $ ix `St.member` st

incomplete :: State -> Bool
incomplete (sq, _, ix, _) = ix /= Sq.length sq

modded :: Seq Instruction -> [Seq Instruction]
modded sq = Sq.foldlWithIndex swap [] sq
  where swap acc _ (Acc, _) = acc
        swap acc ix (Nop, n) = Sq.update ix (Jmp, n) sq : acc
        swap acc ix (Jmp, n) = Sq.update ix (Nop, n) sq : acc

p2 :: Seq Instruction -> State
p2 = go . modded
  where
    go (s:ss)
      | not . incomplete $ run = run
      | otherwise = go ss
      where run = runNext $ runWhile (\x -> incomplete x && newInstruction x) s

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    xs = Sq.fromList . map parseLine $ lines input
    part1 = show . accumulator $ runWhile newInstruction xs
    part2 = show . accumulator $ p2 xs
