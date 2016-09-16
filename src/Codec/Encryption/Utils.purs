module Codec.Encryption.Utils where

import Prelude
import Codec.Encryption.DES as DES
import Codec.Encryption.Word64 (Word64, read, pack, unpack)
import Data.Array (take, drop, concat, concatMap)
import Data.Char (fromCharCode, toCharCode)
import Data.Int.Bits ((.&.), shr)
import Data.List ((:))
import Data.String (toCharArray, fromCharArray, takeWhile)
import Partial.Unsafe (unsafePartial)

encrypt :: Word64 -> Array Word64 -> Array Word64
encrypt k x = (DES.encrypt k) <$> x

decrypt :: Word64 -> Array Word64 -> Array Word64
decrypt k x = (DES.decrypt k) <$> x

words64 :: Array Int -> Array Word64
words64 l = pack <$> chunks 8 l

unwords64 :: Array Word64 -> Array Int
unwords64 l = concat $ unpack <$> l 

charsBytes :: Array Char -> Array Int
charsBytes = concatMap (\x -> let c = toCharCode x in [shr c 8, c .&. 255])

bytesChars :: Array Int -> Array Char
bytesChars s = unsafePartial $ (\ [h,l] -> fromCharCode $ h * 256 + l) <$> chunks 2 s

encryptText :: Word64 -> String -> Array Word64
encryptText k = encrypt k <<< words64 <<< charsBytes <<< toCharArray

decryptText :: Word64 -> Array Word64 -> String
decryptText k = strip0s <<< fromCharArray <<< bytesChars <<< unwords64 <<< decrypt k

strip0s :: String -> String
strip0s = takeWhile ((/=) (fromCharCode 0))

textKey :: String -> Word64
textKey = pack <<< charsBytes <<< toCharArray

hexKey :: String -> Word64
hexKey = read

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = pure (take n xs) <> (chunks n $ drop n xs)
