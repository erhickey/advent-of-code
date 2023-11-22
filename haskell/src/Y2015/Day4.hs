{-# LANGUAGE OverloadedStrings #-}

module Y2015.Day4 (solve) where

import qualified Data.ByteString as BS (ByteString, isPrefixOf)
import qualified Data.ByteString.Lazy as BL (append, ByteString, filter)
import qualified Data.ByteString.Lazy.Char8 as CL (pack)
import Data.ByteString.Lazy.Char8 (pack)

import Crypto.Hash.MD5 (hashlazy)
import Data.ByteString.Base16 (encode)
import Data.Word8 (isControl)

hash :: BL.ByteString -> BS.ByteString
hash = encode . hashlazy

-- TODO: find faster way to convert Num to ByteString
keys :: BL.ByteString -> [BL.ByteString]
keys key = map (BL.append key . pack . show) [1..]

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    hashes = map hash . keys . BL.filter (not . isControl) $ CL.pack input
    part1 = show . (+1) . length $ takeWhile (not . BS.isPrefixOf "00000") hashes
    part2 = show . (+1) . length $ takeWhile (not . BS.isPrefixOf "000000") hashes
