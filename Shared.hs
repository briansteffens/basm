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
