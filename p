diff --cc bower.json
index 8854f70,3db8285..0000000
--- a/bower.json
+++ b/bower.json
diff --cc src/Codec/Encryption/DES.purs
index c1c88f6,0b4f090..0000000
--- a/src/Codec/Encryption/DES.purs
+++ b/src/Codec/Encryption/DES.purs
@@@ -1,14 -1,12 +1,13 @@@
  module Codec.Encryption.DES (encrypt, decrypt) where
  
 -import Data.Array(length, zipWith, take, drop, map, concat)
 -import Data.Foldable(sum)
 -import Prelude.Unsafe(unsafeIndex)
 -import Codec.Encryption.Word64
 +import Codec.Encryption.Word64 (Word64, unbitify, bitify)
 +import Data.Array (zipWith, take, drop, concat)
 +import Data.Array.Partial (unsafeIndex)
- import Data.Int.Bits ((.&.))
++import Data.Int.Bits (shl, (.^.), shr, (.&.))
 +import Data.List ((:), List(Nil))
 +import Partial.Unsafe (unsafePartial)
 +import Prelude ((==), (+), map, ($), (/=), (<>))
  
- type Bool = Boolean
 -type Bool = Number
 -type Int = Number
  type Word8 = Int
  type Rotation = Int
  type Key     = Word64
@@@ -16,23 -14,23 +15,23 @@@ type Message = Word6
  type Enc     = Word64
  
  
- type BitsX  = Array Bool
- type Bits4  = Array Bool
- type Bits6  = Array Bool
- type Bits28 = Array Bool
- type Bits32 = Array Bool
- type Bits48 = Array Bool
- type Bits56 = Array Bool
- type Bits64 = Array Bool
 -type BitsX  = [Bool]
 -type Bits4  = [Bool]
 -type Bits6  = [Bool]
 -type Bits28 = [Bool]
 -type Bits32 = [Bool]
 -type Bits48 = [Bool]
 -type Bits56 = [Bool]
 -type Bits64 = [Bool]
++type BitsX  = Array Boolean
++type Bits4  = Array Boolean
++type Bits6  = Array Boolean
++type Bits28 = Array Boolean
++type Bits32 = Array Boolean
++type Bits48 = Array Boolean
++type Bits56 = Array Boolean
++type Bits64 = Array Boolean
  
  rotateL :: Bits28 -> Int -> Bits28
 -rotateL bits rot = drop rot bits ++ take rot bits
 +rotateL bits rot = drop rot bits <> take rot bits
  
  xor :: BitsX -> BitsX -> BitsX
- xor = zipWith (/=)
+ xor = zipWith (.^.)
  
  initial_permutation :: Bits64 -> Bits64
 -initial_permutation mb = map (unsafeIndex mb) i
 +initial_permutation mb = unsafePartial $ map (unsafeIndex mb) i
   where i = [57, 49, 41, 33, 25, 17,  9, 1, 59, 51, 43, 35, 27, 19, 11, 3,
              61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7,
              56, 48, 40, 32, 24, 16,  8, 0, 58, 50, 42, 34, 26, 18, 10, 2,
@@@ -110,29 -103,20 +109,29 @@@ expansion_permutation mb = unsafePartia
              15, 16, 17, 18, 19, 20, 19, 20, 21, 22, 23, 24,
              23, 24, 25, 26, 27, 28, 27, 28, 29, 30, 31,  0]
  
 -s_box :: [[Word8]] -> Bits6 -> Bits4
 -s_box s (a:b:c:d:e:f:_) = bits4 $ (s `unsafeIndex` row) `unsafeIndex` col
 - where row = num1 a + num0 f
 -       col = num3 b + num2 c  + num1 d + num0 e
 -       num0 x = x
 -       num1 x = x `shl` 1
 -       num2 x = x `shl` 2
 -       num3 x = x `shl` 3
 -       bits4 i = [ (i .&. 8) `shr` 3
 -                 , (i .&. 4) `shr` 2
 -                 , (i .&. 2) `shr` 1
 -                 , (i .&. 1)
 -                 ]
 -
 +s_box' :: Partial => Array (Array Word8) -> Bits6 -> Bits4
 +s_box' s arr = bits4 $ (s `unsafeIndex` row) `unsafeIndex` col
 +  where a = arr `unsafeIndex` 0
 +        b = arr `unsafeIndex` 1
 +        c = arr `unsafeIndex` 2
 +        d = arr `unsafeIndex` 3
 +        e = arr `unsafeIndex` 4
 +        f = arr `unsafeIndex` 5
 +        row = num1 a + num0 f
 +        col = num3 b + num2 c  + num1 d + num0 e
-         num0 x = if x then 1 else 0 
-         num1 x = if x then 2 else 0
-         num2 x = if x then 4 else 0
-         num3 x = if x then 8 else 0
-         bits4 i = [ ((i .&. 8) == 8)
-                   , ((i .&. 4) == 4)
-                   , ((i .&. 2) == 2)
-                   , ((i .&. 1) == 1)
++        num0 x = x
++        num1 x = x `shl` 1
++        num2 x = x `shl` 2
++        num3 x = x `shl` 3
++        bits4 i = [ (i .&. 8) `shr` 3
++                  , (i .&. 4) `shr` 2
++                  , (i .&. 2) `shr` 1
++                  , (i .&. 1)
 +                  ]
 +
 +s_box :: Array (Array Word8) -> Bits6 -> Bits4
 +s_box = unsafePartial s_box'                  
 +                 
  s_box_1 :: Bits6 -> Bits4
  s_box_1 = s_box i
   where i = [[14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7],
diff --cc src/Codec/Encryption/Word64.purs
index 97a99f6,715b421..0000000
--- a/src/Codec/Encryption/Word64.purs
+++ b/src/Codec/Encryption/Word64.purs
