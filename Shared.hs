module Shared where

import Data.Bits
import Data.Binary
import qualified Data.ByteString.Lazy as B


-- Convert an array of bits into a byte
bitsToByte :: [Int] -> Word8
bitsToByte = foldl (\byte bit -> byte * 2 + (fromIntegral bit :: Word8)) 0


-- Convert an int value to bytes
toBytes :: (Binary a) => a -> [Word8]
toBytes a = B.unpack (B.reverse (encode a))


-- Split a list on some predicate
split :: (a -> Bool) -> [a] -> [[a]]
split p []   = []
split p list = do
    let (current, remaining) = break p list
    let recur = if null remaining then [] else split p (tail remaining)
    [current] ++ recur


-- Concatenate the members of a 2-tuple of lists.
concatTuple :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatTuple (la, ra) (lb, rb) = (la ++ lb, ra ++ rb)


stripLeft :: (a -> Bool) -> [a] -> [a]
stripLeft _ [] = []
stripLeft p (x:xs)
    | matched   = stripLeft p xs
    | otherwise = [x] ++ xs
    where matched = p x


stripRight :: (a -> Bool) -> [a] -> [a]
stripRight p x = reverse (stripLeft p (reverse x))


fromLeft :: Either a b -> a
fromLeft (Left  l) = l
fromLeft (Right _) = error("Invalid call to fromLeft")


fromRight :: Either a b -> b
fromRight (Right r) = r
fromRight (Left  _) = error("Invalid call to fromRight")
