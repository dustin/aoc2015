module Day4 where

import           Crypto.Hash           (Digest, MD5 (..), digestToHexByteString,
                                        hash)
import           Data.Byteable         (toBytes)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC

input :: BS.ByteString
input = "bgvyzdsv"

part1 :: Int
part1 = head [n | n <- [1..], "00000" `BS.isPrefixOf` h n]
  where
    h n = digestToHexByteString (hash (input <> BC.pack (show n)) :: Digest MD5)

part2 :: Int
part2 = head [n | n <- [1..], "\0\0\0" `BS.isPrefixOf` h n]
  where
    h n = toBytes (hash (input <> BC.pack (show n)) :: Digest MD5)
