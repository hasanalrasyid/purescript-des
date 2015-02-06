module Test.Main where

import Data.String(toCharArray)
import Debug.Trace
import Codec.Encryption.DES
import qualified Codec.Encryption.Utils as U
import Codec.Encryption.Word64
import Test.QuickCheck
import Math(floor)

instance arbWord64 :: Arbitrary Word64 where
  arbitrary = do
    h <- arbitrary
    l <- arbitrary
    return $ Word64 (w32 h) (w32 l)
      where w32 = Math.floor <<< ((*) 0x100000000)

main = do
  let k = U.hexKey "5B5A57676A56676E"
  assert $ k == Word64 0x5B5A5767 0x6A56676E
  assert $ Word64 0x974AFFBF 0x86022D1F == encrypt k (Word64 0x675A6967 0x5E5A6B5A)
  
--  let k2 = U.textKey "12345678"
--  let orig = "hello world"
--  let m = U.words64 $ U.stringBytes orig
--  assert $ U.encrypt k2 m == [ Word64 0x28dba02e 0xb5f6dd47
--                             , Word64 0x6042daeb 0xfa59687a
--                             ]
  quickCheck $ \a -> a == (U.words64 $ U.unwords64 a)
  quickCheck $ \t -> t == (U.bytesChars $ U.charsBytes t) <?> show t ++ show (U.charsBytes t)
  quickCheck $ \k m -> m == (U.decrypt k $ U.encrypt k m)
  quickCheck $ \x -> let ix = (floor $ 255 * x) in ix == (breverse $ breverse ix)
  quickCheck $ \x -> x == (pack $ unpack x)
  quickCheck $ \k m -> m == (decrypt k $ encrypt k m)
  quickCheck $ \k m -> let m' = (U.decryptText k $ U.encryptText k m) in m == m' <?> show m ++ " -> " ++ show m' ++ " A " ++ show (toCharArray m) 


assert :: Boolean -> QC Unit
assert = quickCheck' 1
