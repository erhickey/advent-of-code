module Y2019.Intcode (ProgramState(..), runIntcode, runIntcodeEasy, runIntcodeXMem) where

import Control.Monad.ST (runST, ST)
import Data.Array.Base (unsafeFreeze, unsafeRead, unsafeWrite)
import Data.Array.MArray (newListArray)
import Data.Array.ST (STArray)
import Data.Array.Unboxed (Array, elems)

data ProgramState = ProgramState
  { psIntcode :: [Int]
  , psInput   :: [Int]
  , psOutput  :: [Int]
  , psIndex   :: Int        -- instruction pointer
  , psRBase   :: Int        -- relative base
  }

type InternalState s =
  ( STArray s Int Int -- memory
  , [Int]             -- input
  , [Int]             -- output
  , Int               -- instruction pointer
  , Int               -- relative base
  )

runIntcodeEasy :: [Int] -> [Int]
runIntcodeEasy xs = psIntcode . runIntcode $ ProgramState xs [] [] 0 0

runIntcode :: ProgramState -> ProgramState
runIntcode = runIntcodeXMem 0

runIntcodeXMem :: Int -> ProgramState -> ProgramState
runIntcodeXMem memX ps =
  let intcode = psIntcode ps
      input   = psInput ps
      ix      = psIndex ps
      rb      = psRBase ps
  in runST $ do
    let xmSize = memX * length intcode
        mem = intcode ++ replicate xmSize 0
        ln = length intcode + xmSize - 1
    arr <- newListArray (0, ln) $ intcode ++ mem
    (_, _, output, rix, rrb) <- loop (arr, input, [], ix, rb)
    rArr <- unsafeFreeze arr
    return $ ProgramState (elems (rArr :: Array Int Int)) [] output rix rrb

loop :: InternalState s -> ST s (InternalState s)
loop state@(arr, input, output, ix, rb) = do
  instruction <- getValue arr ix
  let (a3Mode, a2Mode, a1Mode, opcode) = parseInstruction instruction
  case opcode of
    1   -> mathOp (+) state a1Mode a2Mode a3Mode
    2   -> mathOp (*) state a1Mode a2Mode a3Mode
    3   ->
      if not $ null input then do
        let val = head input
        writeIx <- getDest arr (ix + 1) rb a1Mode
        writeAndLoop (arr, tail input, output, ix, rb) val writeIx $ ix + 2
      else return state
    4   -> do
      out <- getArg arr (ix + 1) rb a1Mode
      loop (arr, input, out:output, ix + 2, rb)
    5   -> jump (/= 0) state a1Mode a2Mode
    6   -> jump (== 0) state a1Mode a2Mode
    7   -> condOp (<) state a1Mode a2Mode a3Mode
    8   -> condOp (==) state a1Mode a2Mode a3Mode
    9   -> do
      operand2 <- getArg arr (ix + 1) rb a1Mode
      loop (arr, input, output, ix + 2, rb + operand2)
    99  -> return (arr, input, output, -1, rb)
    _   -> loop (arr, input, output, ix + 1, rb)

mathOp :: (Int -> Int -> Int) -> InternalState s -> Int -> Int -> Int -> ST s (InternalState s)
mathOp f state@(arr, _, _, ix, rb) a1Mode a2Mode a3Mode = do
  operand1 <- getArg arr (ix + 1) rb a1Mode
  operand2 <- getArg arr (ix + 2) rb a2Mode
  writeIx  <- getDest arr (ix + 3) rb a3Mode
  writeAndLoop state (f operand1 operand2) writeIx $ ix + 4

jump :: (Int -> Bool) -> InternalState s -> Int -> Int -> ST s (InternalState s)
jump f (arr, input, output, ix, rb) a1Mode a2Mode = do
  arg1   <- getArg arr (ix + 1) rb a1Mode
  trueIx <- getArg arr (ix + 2) rb a2Mode
  if f arg1 then loop (arr, input, output, trueIx, rb) else loop (arr, input, output, ix + 3, rb)

condOp :: (Int -> Int -> Bool) -> InternalState s -> Int -> Int -> Int -> ST s (InternalState s)
condOp f state@(arr, _, _, ix, rb) a1Mode a2Mode a3Mode = do
  arg1    <- getArg arr (ix + 1) rb a1Mode
  arg2    <- getArg arr (ix + 2) rb a2Mode
  writeIx <- getDest arr (ix + 3) rb a3Mode
  writeAndLoop state (if f arg1 arg2 then 1 else 0) writeIx $ ix + 4

writeAndLoop :: InternalState s -> Int -> Int -> Int -> ST s (InternalState s)
writeAndLoop state@(arr, input, output, ix, rb) val writeIx nextIx = do
  unsafeWrite arr writeIx val
  if writeIx == ix then loop state else loop (arr, input, output, nextIx, rb)

getArg :: STArray s Int Int -> Int -> Int -> Int -> ST s Int
getArg arr ix rb mode
  | mode == 2 = getValue arr . (+) rb =<< getValue arr ix
  | mode == 1 = getValue arr ix
  | otherwise = getValue arr =<< getValue arr ix

getDest :: STArray s Int Int -> Int -> Int -> Int -> ST s Int
getDest arr ix rb mode
  | mode == 2 = (+) rb <$> getValue arr ix
  | otherwise = getValue arr ix

getValue :: STArray s Int Int -> Int -> ST s Int
getValue = unsafeRead

-- (ten thousands, thousands, hundreds, tens + ones) places of int
parseInstruction :: Int -> (Int, Int, Int, Int)
parseInstruction instruction = (head digits, digits !! 1, digits !! 2, 10 * (digits !! 3) + digits !! 4)
  where digits = map (`mod` 10) . reverse . take 5 $ iterate (`div` 10) instruction
