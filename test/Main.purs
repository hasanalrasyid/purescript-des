module Test.Main where

import Test.QuickCheck
import Codec.Encryption.Utils as U
import Codec.Encryption.DES (encrypt, decrypt)
import Codec.Encryption.Word64 (unpack, pack, breverse, Word64(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Int.Bits ((.&.))
import Data.String (toCharArray)
import Prelude (map, show, (<>), Unit, class Eq, unit, pure, bind, eq, (==), (-), ($), negate)
import Test.QuickCheck.Arbitrary (arbitrary, class Arbitrary)

newtype TWord64 = TWord64 Word64


instance eqTWord64 :: Eq TWord64 where
  eq (TWord64 a) (TWord64 b) = eq a b

instance arbTWord64 :: Arbitrary TWord64 where
  arbitrary = do
    h <- arbitrary
    l <- arbitrary
    pure $ TWord64 (Word64 h l)

main :: forall e. Eff ( console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | e) Unit
main = do
  let assert = quickCheck' 1
  let k = U.hexKey "5B5A57676A56676E"
  assert $ k == Word64 0x5B5A5767 0x6A56676E
  let u = Word64 (-1756692545) (-2046677729)
      e = encrypt k (Word64 0x675A6967 0x5E5A6B5A)
  quickCheck' 1 $ u == e <?> "unencrypted " <> show u <> " encrypted " <> show e

  let unword64 (TWord64 w) = w
      unwordArray64 = map unword64
  
  -- let k2 = U.textKey "12345678"
  -- let orig = "hello world"
  -- let m = U.words64 $ U.stringBytes orig
  -- assert $ U.encrypt k2 m == [ Word64 0x28dba02e 0xb5f6dd47
  --                            , Word64 0x6042daeb 0xfa59687a
  --                            ]
  quickCheck $ \a -> (a :: Array TWord64) == (map TWord64 $ U.words64 $ U.unwords64 $ unwordArray64 a)
  quickCheck $ \t -> t == (U.bytesChars $ U.charsBytes t) <?> show t <> show (U.charsBytes t)
  quickCheck $ \(TWord64 k) m -> (unwordArray64 m) == (U.decrypt k $ U.encrypt k (unwordArray64 m))
  quickCheck $ \x -> let ix = (x .&. 255) in ix == (breverse $ breverse ix)
  quickCheck $ \(TWord64 x) -> x == (pack $ unpack x)
  quickCheck $ \(TWord64 k) (TWord64 m) -> m == (decrypt k $ encrypt k m)
  quickCheck $ \(TWord64 k) m -> let m' = (U.decryptText k $ U.encryptText k m) in m == m' <?> show m <> " -> " <> show m' <> " A " <> show (toCharArray m)
  pure unit


