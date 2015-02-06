module Codec.Encryption.Word64(Word64(..), bitify, unbitify, read, breverse, fromBytes, toBytes, pack, unpack) where

import Data.Foldable(foldl)
import Data.Array(map, range, take, drop, reverse, length)
import qualified Data.String as S
import Math(pow)
import Prelude.Unsafe(unsafeIndex)

data Word64 = Word64 Number Number

bitify32 :: Number -> [Boolean]
bitify32 w = map (\b -> w .&. (pow 2 b) /= 0) $ range 31 0

bitify :: Word64 -> [Boolean]
bitify (Word64 h l) = (bitify32 h) ++ (bitify32 l)

unbitify32 :: [Boolean] -> Number
unbitify32 bs = foldl (\i b -> i + i + if b then 1 else 0) 0 bs

unbitify :: [Boolean] -> Word64
unbitify bs = Word64 (unbitify32 $ take 32 bs) (unbitify32 $ drop 32 bs)

instance eqWord64 :: Eq Word64 where
  (==) (Word64 ah al) (Word64 bh bl) = ah == bh && al == bl
  (/=) a b = not $ a == b

instance showWord64 :: Show Word64 where
  show (Word64 h l) = "0x" ++ showHex h ++ showHex l

read :: String -> Word64
read s = Word64 (readHex (S.take 8 s)) (readHex (S.drop 8 s))

breverse :: Number -> Number
breverse = unsafeIndex [0,128,64,192,32,160,96,224,16,144,80,208,48,176,112,240,8,136,72,200,40,168,104,232,24,152,88,216,56,184,120,248,4,132,68,196,36,164,100,228,20,148,84,212,52,180,116,244,12,140,76,204,44,172,108,236,28,156,92,220,60,188,124,252,2,130,66,194,34,162,98,226,18,146,82,210,50,178,114,242,10,138,74,202,42,170,106,234,26,154,90,218,58,186,122,250,6,134,70,198,38,166,102,230,22,150,86,214,54,182,118,246,14,142,78,206,46,174,110,238,30,158,94,222,62,190,126,254,1,129,65,193,33,161,97,225,17,145,81,209,49,177,113,241,9,137,73,201,41,169,105,233,25,153,89,217,57,185,121,249,5,133,69,197,37,165,101,229,21,149,85,213,53,181,117,245,13,141,77,205,45,173,109,237,29,157,93,221,61,189,125,253,3,131,67,195,35,163,99,227,19,147,83,211,51,179,115,243,11,139,75,203,43,171,107,235,27,155,91,219,59,187,123,251,7,135,71,199,39,167,103,231,23,151,87,215,55,183,119,247,15,143,79,207,47,175,111,239,31,159,95,223,63,191,127,255]


pack :: [Number] -> Word64
pack bs = Word64 (p (take 4 bs)) (p (drop 4 bs))
  where p l = foldl step 0 $ pad l
          where step sofar i = sofar * 256 + i
                pad l = l ++ take (4 - length l) [0,0,0,0]

unpack :: Word64 -> [Number]
unpack (Word64 h l) = u h ++ u l
  where u x = [ (x / 0x1000000) .&. 0xff
              , (x / 0x10000) .&. 0xff
              , (x / 0x100) .&. 0xff
              , x .&. 0xff
              ]        

fromBytes :: [Number] -> Word64
fromBytes x = pack $ breverse <$> x

toBytes :: Word64 -> [Number]
toBytes x = breverse <$> unpack x

foreign import showHex
"""
function showHex(a) {
  return ('00000000'+((a>>>0).toString(16))).slice(-8);
}
""" :: Number -> String


foreign import readHex
"""
function readHex(a) {
  return parseInt(a, 16);
}
""" :: String -> Number
