{-# LANGUAGE OverloadedStrings #-}

module Y2021.Day16 (solve) where

import Control.Applicative ((<|>))
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M (fromList)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Attoparsec.Text (char, count, many', parseOnly, Parser, string)
import qualified Data.Attoparsec.Text as P (take)

data Packet = Packet { packetVersion :: Int, packetType :: Int, packetData :: PacketData }
data PacketData = Packets [Packet] | PacketValue Int

textToDec :: Text -> Int
textToDec = stringToDec . T.unpack

stringToDec :: String -> Int
stringToDec = binToDec . map (read . (:[]))

binToDec :: [Int] -> Int
binToDec = sum . zipWith (*) (iterate (*2) 1) . reverse

charToBin :: Map Char String
charToBin = M.fromList
  [ ('0', "0000") , ('1', "0001") , ('2', "0010") , ('3', "0011")
  , ('4', "0100") , ('5', "0101") , ('6', "0110") , ('7', "0111")
  , ('8', "1000") , ('9', "1001") , ('A', "1010") , ('B', "1011")
  , ('C', "1100") , ('D', "1101") , ('E', "1110") , ('F', "1111")
  ]

parse :: String -> [Packet]
parse = fromRight [] . parseOnly parsePackets . T.pack . concatMap (charToBin !)

parsePackets :: Parser [Packet]
parsePackets = many' parsePacket

parsePacket :: Parser Packet
parsePacket = P.take 3 >>= (\v -> parseLiteralValuePacket v <|> parseOperatorPacket v) . textToDec

parseLiteralValuePacket :: Int -> Parser Packet
parseLiteralValuePacket version = do
  _ <- string "100"
  vs <- many' (char '1' >> P.take 4)
  lv <- char '0' >> P.take 4
  pure . Packet version 4 . PacketValue . stringToDec . concatMap T.unpack $ vs ++ [lv]

parseOperatorPacket :: Int -> Parser Packet
parseOperatorPacket version = do
  typeId <- textToDec <$> P.take 3
  ps <- parseSubPacketsByLength <|> parseSubPacketsByNum
  pure . Packet version typeId $ Packets ps

parseSubPacketsByLength :: Parser [Packet]
parseSubPacketsByLength = char '0' >> P.take 15 >>= (P.take . textToDec) <&> (fromRight [] . parseOnly parsePackets)

parseSubPacketsByNum :: Parser [Packet]
parseSubPacketsByNum = char '1' >> P.take 11 >>= (`count` parsePacket) . textToDec

versionSum :: Packet -> Int
versionSum (Packet v _ (Packets ps)) = v + sum (map versionSum ps)
versionSum (Packet v _ _) = v

calculate :: Packet -> Int
calculate (Packet _ 0 (Packets ps)) = sum $ map calculate ps
calculate (Packet _ 1 (Packets ps)) = product $ map calculate ps
calculate (Packet _ 2 (Packets ps)) = minimum $ map calculate ps
calculate (Packet _ 3 (Packets ps)) = maximum $ map calculate ps
calculate (Packet _ 4 (PacketValue n)) = n
calculate (Packet _ 5 (Packets (x:y:_))) = if calculate x > calculate y then 1 else 0
calculate (Packet _ 6 (Packets (x:y:_))) = if calculate x < calculate y then 1 else 0
calculate (Packet _ 7 (Packets (x:y:_))) = if calculate x == calculate y then 1 else 0

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ps = parse $ init input
    part1 = show . sum $ map versionSum ps
    part2 = show . sum $ map calculate ps
