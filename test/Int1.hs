{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnboxedTuples #-}

module Int1
  ( tests
  ) where

import Basics.Int1
import Prelude hiding (read)

import Control.Applicative (liftA2)
import Control.Monad (forM)
import Control.Monad.ST (runST)
import Data.Int1 (Int1)
import Data.Monoid (All(..))
import Data.Primitive.ByteArray (ByteArray(..),MutableByteArray(..))
import Data.Proxy (Proxy(Proxy))
import Data.Word (Word32)
import GHC.Exts (Int(I#),MutableByteArray#)
import GHC.ST (ST(ST))
import GHC.TypeNats (KnownNat)
import Test.QuickCheck.Instances.Text ()
import Test.Tasty (TestTree,defaultMain,testGroup,adjustOption)
import Test.Tasty.HUnit (testCase,(@=?),assertFailure)
import Test.Tasty.QuickCheck ((===),(=/=),(==>))
import Test.Tasty.QuickCheck (Gen,testProperty)
import Unsafe.Coerce (unsafeCoerce)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as F
import qualified Data.Int1 as Int1
import qualified Data.List as L
import qualified Data.List as List
import qualified Data.Primitive.ByteArray as Prim
import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain
  $ adjustOption (\_ -> QC.QuickCheckTests 4000)
  $ tests

tests :: TestTree
tests = testGroup "basics"
  [ testGroup "Int1"
    [ testProperty "lift/unlift are inverse" $ \b ->
        lift (unlift b) == b
    , testCase "initialize with False" $
        let sz = 150
            actual = runST $ do
                marr <- initialized sz 0
                forM [0..sz-1] $ \i -> read marr i
            expected = replicate sz 0
         in actual @=? expected
    , testCase "initialize with True" $
        let sz = 150
            actual = runST $ do
                marr <- initialized sz 1
                forM [0..sz-1] $ \i -> read marr i
            expected = replicate sz 1
         in actual @=? expected
    , testProperty "read/write single bits" $ \xs ->
        let actual = runST $ do
              marr <- uninitialized (length xs)
              forM (zip [0..] xs) $ \(i, x) ->
                write marr i x
              forM [0..length xs - 1] $ \i -> read marr i
         in actual == xs
    , testProperty "index/write single bits" $ \xs ->
        let arr = Prim.runByteArray $ do
              marr <- uninitialized (length xs)
              forM (zip [0..] xs) $ \(i, x) ->
                write marr i x
              pure marr
            actual = index arr <$> [0..length xs - 1]
         in actual == xs
    , testProperty "shrink" $ \Shrink{xs,sz} ->
        let actual = runST $ do
              marr <- uninitialized (length xs)
              forM (zip [0..] xs) $ \(i, x) ->
                write marr i x
              marr' <- shrink marr sz
              forM [0..sz - 1] $ \i -> read marr' i
         in actual == take sz xs
    , testProperty "set#" $ \Set{xs,off,len,v} ->
        let actual = runST $ do
              marr <- uninitialized (length xs)
              forM (zip [0..] xs) $ \(i, x) ->
                write marr i x
              set marr off len v
              forM [0..length xs - 1] $ \i -> read marr i
            expected = take off xs ++ replicate len v ++ drop (off + len) xs
         in actual === expected
    , testProperty "copy" $ \Copy{xs,doff,ys,soff,len} ->
        let src = Prim.runByteArray $ do
              marr <- uninitialized (length ys)
              forM (zip [0..] ys) $ \(i, y) ->
                write marr i y
              pure marr
            actual = runST $ do
              dst <- uninitialized (length xs)
              forM (zip [0..] xs) $ \(i, x) ->
                write dst i x
              copy dst doff src soff len
              forM [0..length xs - 1] $ \i -> read dst i
            expected = take doff xs ++ take len (drop soff ys) ++ drop (doff + len) xs
         in actual === expected
    , testProperty "copyMutable" $ \Copy{xs,doff,ys,soff,len} ->
        let actual = runST $ do
              src <- uninitialized (length ys)
              forM (zip [0..] ys) $ \(i, y) ->
                write src i y
              dst <- uninitialized (length xs)
              forM (zip [0..] xs) $ \(i, x) ->
                write dst i x
              copyMutable dst doff src soff len
              forM [0..length xs - 1] $ \i -> read dst i
            expected = take doff xs ++ take len (drop soff ys) ++ drop (doff + len) xs
         in actual === expected
    ]
  ]

--- Lift the functions under test ---

index :: ByteArray -> Int -> Int1
index (ByteArray arr) (I# i) = lift (index# arr i)

read :: MutableByteArray s -> Int -> ST s Int1
read (MutableByteArray marr) (I# ix) = ST (\st ->
  let (# st', v #) = read# marr ix st
   in (# st', lift v #))

write :: MutableByteArray s -> Int -> Int1 -> ST s ()
write (MutableByteArray marr) (I# i) v = ST (\st ->
  (# write# marr i (unlift v) st, () #))

uninitialized :: Int -> ST s (MutableByteArray s)
uninitialized (I# n) = ST (\st ->
  let (# st', marr #) = uninitialized# n st
   in (# st', MutableByteArray marr #))

initialized :: Int -> Int1 -> ST s (MutableByteArray s)
initialized (I# n) v = ST (\st ->
  let (# st', marr #) = initialized# n (unlift v) st
   in (# st', MutableByteArray marr #))

shrink :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
shrink (MutableByteArray marr) (I# i) = ST (\st ->
  let (# st', marr' #) = shrink# marr i st
   in (# st', MutableByteArray marr' #))

set :: MutableByteArray s -> Int -> Int -> Int1 -> ST s ()
set (MutableByteArray marr) (I# off) (I# len) v = ST (\st ->
   (# set# marr off len (unlift v) st, () #))

copy :: MutableByteArray s -> Int -> ByteArray -> Int -> Int -> ST s ()
copy (MutableByteArray dst) (I# doff) (ByteArray src) (I# soff) (I# len) = ST (\st ->
  (# copy# dst doff src soff len st, () #))

copyMutable :: MutableByteArray s -> Int -> MutableByteArray s -> Int -> Int -> ST s ()
copyMutable (MutableByteArray dst) (I# doff) (MutableByteArray src) (I# soff) (I# len) = ST (\st ->
  (# copyMutable# dst doff src soff len st, () #))

instance QC.Arbitrary Int1 where
  arbitrary = do
    b <- QC.arbitrary
    pure $ Int1.fromInt b

data Shrink = Shrink { xs::[Int1], sz::Int} deriving (Show)
instance QC.Arbitrary Shrink where
  arbitrary = do
    x' <- QC.arbitrary
    xs' <- QC.arbitrary
    let xs = x':xs'
    sz <- QC.chooseInt (1, length xs)
    pure Shrink{xs,sz}

data Set = Set { xs::[Int1], off::Int, len::Int, v::Int1 } deriving (Show)
instance QC.Arbitrary Set where
  arbitrary = do
    xs <- QC.arbitrary
    off <- QC.chooseInt (0,max 0 (length xs - 1))
    len <- QC.chooseInt (0,length xs - off)
    v <- QC.arbitrary
    pure Set{xs,off,len,v}

data Copy = Copy { xs::[Int1],doff::Int,ys::[Int1],soff::Int,len::Int } deriving (Show)
instance QC.Arbitrary Copy where
  arbitrary = do
    xs <- QC.arbitrary
    doff <- QC.chooseInt (0, max 0 (length xs - 1))
    ys <- QC.arbitrary
    soff <- QC.chooseInt (0, max 0 (length ys - 1))
    len <- QC.chooseInt (0, min (length xs - doff) (length ys - soff))
    pure Copy{xs,doff,ys,soff,len}
