{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Int8
  ( -- Types
    T
  , T#
  , R
    -- Lifting
  , lift
  , unlift
    -- Compare
  , gt#
  , lt#
  , gte#
  , lte#
  , eq#
  , neq#
    -- Array
  , read#
  , write#
  , index#
  , set#
  , uninitialized#
  , initialized#
  , uninitialized
  , initialized
  , copy#
  , copyMutable#
  , shrink#
    -- Constants
  , zero
  , def
    -- Metadata
  , signed
  , size
    -- Encoding
  , shows
  ) where

import Prelude hiding (shows)

import Data.Primitive (MutableByteArray(..))
import GHC.Exts hiding (setByteArray#)
import GHC.Int
import GHC.ST (ST(ST))

import qualified Prelude
import qualified GHC.Exts as Exts

type T = Int8
type T# = Int#
type R = 'IntRep

def :: T
def = 0

zero :: T
zero = 0

signed :: Bool
signed = False

size :: Int
size = 1

lift :: T# -> T
lift = I8#

unlift :: T -> T#
unlift (I8# i) = i

gt# :: T# -> T# -> Int#
gt# = (>#)

lt# :: T# -> T# -> Int#
lt# = (<#)

gte# :: T# -> T# -> Int#
gte# = (>=#)

lte# :: T# -> T# -> Int#
lte# = (<=#)

eq# :: T# -> T# -> Int#
eq# = (==#)

neq# :: T# -> T# -> Int#
neq# = (/=#)

index# :: ByteArray# -> Int# -> T#
index# = indexInt8Array#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = readInt8Array#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# = writeInt8Array#

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = Exts.setByteArray# marr off len x s

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
shrink# m i s = (# Exts.shrinkMutableByteArray# m i s, m #)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
uninitialized# = Exts.newByteArray#

uninitialized :: Int -> ST s (MutableByteArray s)
uninitialized (I# sz) = ST $ \s0 -> case uninitialized# sz s0 of
  (# s1, a #) -> (# s1, MutableByteArray a #)

initialized :: Int -> T -> ST s (MutableByteArray s)
initialized (I# sz) e = ST $ \s0 -> case initialized# sz (unlift e) s0 of
  (# s1, a #) -> (# s1, MutableByteArray a #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
copy# dst doff src soff len =
  Exts.copyByteArray# src soff dst doff len

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src soff dst doff len

initialized# :: Int# -> T# -> State# s -> (# State# s, MutableByteArray# s #)
initialized# n e s0 = case Exts.newByteArray# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

shows :: T -> String -> String
shows = Prelude.shows
