module Codec.Encryption.Utils where

import Math(pow)
import Data.Array(take, drop, reverse, length, concat, concatMap)
import Data.Foldable(foldl)
import Codec.Encryption.Word64(Word64(..), read, fromBytes, pack, unpack)
import Data.String(toCharArray, fromCharArray, takeWhile)
import Data.Char(Char(..), toCharCode, fromCharCode)
import qualified Codec.Encryption.DES as DES

encrypt :: Word64 -> [Word64] -> [Word64]
encrypt k x = (DES.encrypt k) <$> x

decrypt :: Word64 -> [Word64] -> [Word64]
decrypt k x = (DES.decrypt k) <$> x

words64 :: [Number] -> [Word64]
words64 l = pack <$> chunks 8 l

unwords64 :: [Word64] -> [Number]
unwords64 l = concat $ unpack <$> l 

charsBytes :: [Char] -> [Number]
charsBytes = concatMap (\x -> let c = toCharCode x in [(c / 256) .&. 255, c .&. 255])

bytesChars :: [Number] -> [Char]
bytesChars s = (\ [h,l] -> fromCharCode $ h * 256 + l) <$> chunks 2 s

encryptText :: Word64 -> String -> [Word64]
encryptText k = encrypt k <<< words64 <<< charsBytes <<< toCharArray

decryptText :: Word64 -> [Word64] -> String
decryptText k = strip0s <<< fromCharArray <<< bytesChars <<< unwords64 <<< decrypt k

strip0s :: String -> String
strip0s = takeWhile ((/=) (fromCharCode 0))

textKey :: String -> Word64
textKey = pack <<< charsBytes <<< toCharArray

hexKey :: String -> Word64
hexKey = read

chunks :: forall a. Number -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : (chunks n $ drop n xs)
