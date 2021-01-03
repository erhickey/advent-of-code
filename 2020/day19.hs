{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<|>))
import Data.IntMap ((!), IntMap)
import qualified Data.IntMap as M (fromList)
import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T (length, lines)
import qualified Data.Text.IO as T (readFile)

import Data.Attoparsec.Text (char, decimal, IResult(..), parse, parseOnly, Parser, sepBy, string)

data Rule = A | B | Single [Int] | Double ([Int], [Int])

ruleLineParser :: Parser (Int, Rule)
ruleLineParser = do
  n <- decimal
  string ": "
  rule <- ruleParser
  return (n, rule)

ruleParser :: Parser Rule
ruleParser = do
  string "\"a\"" >> return A
  <|> (string "\"b\"" >> return B)
  <|> do
    rs1 <- decimal `sepBy` char ' '
    (string " | " >> do
      rs2 <- decimal `sepBy` char ' '
      return $ Double (rs1, rs2))
      <|> return (Single rs1)

ruleValidator :: IntMap Rule -> Rule -> Parser Bool
ruleValidator _ A = char 'a' >> return True
ruleValidator _ B = char 'b' >> return True
ruleValidator m (Single rs) = do
  mapM_ (ruleValidator m . (!) m) rs
  return True
ruleValidator m (Double (rs1, rs2)) = ruleValidator m (Single rs1) <|> ruleValidator m (Single rs2)

isValid :: IntMap Rule -> Text -> Bool
isValid m s = case parse (ruleValidator m (m ! 0)) s of
  Fail {} -> False
  Partial _ -> False
  Done i _ -> T.length i == 0

main = do
  input <- T.lines <$> T.readFile "day19.input"
  let (rules, _:msgs) = break (=="") input
      ruleMap = M.fromList . rights $ map (parseOnly ruleLineParser) rules
      p1 = filter (==True) $ map (isValid ruleMap) msgs
  print . (++) "Part 1: " . show $ length p1
