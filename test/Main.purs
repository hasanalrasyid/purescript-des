module Test.Main where

import Prelude
import Test.Assert(ASSERT, assert)
import Test.QuickCheck (quickCheck, quickCheck', (<?>))
import Codec.Encryption.Utils as U
import Codec.Encryption.DES (encrypt, decrypt)
import Codec.Encryption.Word64 (unpack, pack, breverse, Word64(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (filter)
import Data.Int.Bits ((.&.))
import Data.String (fromCharArray, toCharArray)
import Test.QuickCheck.Arbitrary (arbitrary, class Arbitrary)

newtype ValidString = ValidString String

instance arbValidString :: Arbitrary ValidString where
  arbitrary = do
    s <- arbitrary
    pure $ ValidString $ fromCharArray $ filter (_ /= '\0') $ toCharArray s


newtype TWord64 = TWord64 Word64


instance eqTWord64 :: Eq TWord64 where
  eq (TWord64 a) (TWord64 b) = eq a b

instance arbTWord64 :: Arbitrary TWord64 where
  arbitrary = do
    h <- arbitrary
    l <- arbitrary
    pure $ TWord64 (Word64 h l)

main :: forall e. Eff ( assert :: ASSERT, console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | e) Unit
main = do
  let key = U.hexKey "5B5A57676A56676E"
  assert $ key == Word64 0x5B5A5767 0x6A56676E
  let u = Word64 (-1756692545) (-2046677729) 
      e = encrypt key (Word64 0x675A6967 0x5E5A6B5A)
  quickCheck' 1 $ u == e <?> "unencrypted " <> show u <> " encrypted " <> show e

  let unword64 (TWord64 w) = w
      unwordArray64 = map unword64
  
  quickCheck $ \a -> (a :: Array TWord64) == (map TWord64 $ U.words64 $ U.unwords64 $ unwordArray64 a)
  quickCheck $ \t -> t == (U.bytesChars $ U.charsBytes t) <?> show t <> show (U.charsBytes t)
  quickCheck $ \(TWord64 k) m -> (unwordArray64 m) == (U.decrypt k $ U.encrypt k (unwordArray64 m))
  quickCheck $ \x -> let ix = (x .&. 255) in ix == (breverse $ breverse ix)
  quickCheck $ \(TWord64 x) -> x == (pack $ unpack x)
  quickCheck $ \(TWord64 k) (TWord64 m) -> m == (decrypt k $ encrypt k m)
  quickCheck $ \(TWord64 k) (ValidString m) -> let m' = (U.decryptText k $ U.encryptText k m) in m == m' <?> show m <> " -> " <> show m' <> " A " <> show (toCharArray m)


