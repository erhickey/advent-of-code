{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as BS (ByteString, isPrefixOf)
import qualified Data.ByteString.Lazy as BL (append, ByteString, filter, readFile)
import Data.ByteString.Lazy.Char8 (pack)

import Crypto.Hash.MD5 (hashlazy)
import Data.ByteString.Base16 (encode)
import Data.Word8 (isControl)

hash :: BL.ByteString -> BS.ByteString
hash = encode . hashlazy

-- TODO: find faster way to convert Num to ByteString
keys :: BL.ByteString -> [BL.ByteString]
keys key = map (BL.append key . pack . show) [1..]

main = do
  hashes <- map hash . keys . BL.filter (not . isControl) <$> BL.readFile "day4.input"
  print . (++) "Part 1: " . show . (+1) . length $ takeWhile (not . BS.isPrefixOf "00000") hashes
  print . (++) "Part 2: " . show . (+1) . length $ takeWhile (not . BS.isPrefixOf "000000") hashes
