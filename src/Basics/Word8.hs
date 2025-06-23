{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Word8
  ( -- Types
    T
  , T#
  , R
    -- Lifting
  , lift
  , unlift
    -- Arithmetic
  , minus#
  , quot#
  , rem#
    -- Compare
  , gt#
  , lt#
  , gte#
  , lte#
  , eq#
  , neq#
  , gt
  , lt
  , gte
  , lte
  , eq
  , neq
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
  , maxBound
  , minBound
    -- Metadata
  , signed
  , size
    -- Encode
  , shows
  ) where

import Prelude hiding (shows,minBound,maxBound)

import Data.Primitive (MutableByteArray(..))
import GHC.Exts hiding (setByteArray#)
import GHC.ST (ST(ST))
import GHC.Word

import qualified Prelude
import qualified GHC.Exts as Exts

type T = Word8
type T# = Word8#
type R = 'Word8Rep

def :: T
{-# inline def #-}
def = 0

zero :: T
{-# inline zero #-}
zero = 0

signed :: Bool
{-# inline signed #-}
signed = False

size :: Int
{-# inline size #-}
size = 1

maxBound :: T
{-# inline maxBound #-}
maxBound = 255

minBound :: T
{-# inline minBound #-}
minBound = 0

lift :: T# -> T
{-# inline lift #-}
lift i = W8# i

unlift :: T -> T#
{-# inline unlift #-}
unlift (W8# i) = i

gt# :: T# -> T# -> Int#
{-# inline gt# #-}
gt# = gtWord8#

lt# :: T# -> T# -> Int#
{-# inline lt# #-}
lt# = ltWord8#

gte# :: T# -> T# -> Int#
{-# inline gte# #-}
gte# = geWord8#

lte# :: T# -> T# -> Int#
{-# inline lte# #-}
lte# = leWord8#

eq# :: T# -> T# -> Int#
{-# inline eq# #-}
eq# = eqWord8#

neq# :: T# -> T# -> Int#
{-# inline neq# #-}
neq# = neWord8#

gt :: T -> T -> Bool
{-# inline gt #-}
gt (W8# a) (W8# b) = Exts.isTrue# (gtWord8# a b)

lt :: T -> T -> Bool
{-# inline lt #-}
lt (W8# a) (W8# b) = Exts.isTrue# (ltWord8# a b)

gte :: T -> T -> Bool
{-# inline gte #-}
gte (W8# a) (W8# b) = Exts.isTrue# (geWord8# a b)

lte :: T -> T -> Bool
{-# inline lte #-}
lte (W8# a) (W8# b) = Exts.isTrue# (leWord8# a b)

eq :: T -> T -> Bool
{-# inline eq #-}
eq (W8# a) (W8# b) = Exts.isTrue# (eqWord8# a b)

neq :: T -> T -> Bool
{-# inline neq #-}
neq (W8# a) (W8# b) = Exts.isTrue# (neWord8# a b)

minus# :: T# -> T# -> T#
{-# inline minus# #-}
minus# x y = subWord8# x y

quot# :: T# -> T# -> T#
{-# inline quot# #-}
quot# = quotWord8#

rem# :: T# -> T# -> T#
{-# inline rem# #-}
rem# = remWord8#

index# :: ByteArray# -> Int# -> T#
{-# inline index# #-}
index# arr i = indexWord8Array# arr i

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# arr i st = readWord8Array# arr i st

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# arr i v st = writeWord8Array# arr i v st

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# marr off len x s = Exts.setByteArray# marr off len (word2Int# (word8ToWord# x)) s

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline shrink# #-}
shrink# m i s = (# Exts.shrinkMutableByteArray# m i s, m #)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline uninitialized# #-}
uninitialized# = Exts.newByteArray#

initialized# :: Int# -> T# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline initialized# #-}
initialized# n e s0 = case Exts.newByteArray# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

uninitialized :: Int -> ST s (MutableByteArray s)
{-# inline uninitialized #-}
uninitialized (I# sz) = ST $ \s0 -> case uninitialized# sz s0 of
  (# s1, a #) -> (# s1, MutableByteArray a #)

initialized :: Int -> T -> ST s (MutableByteArray s)
{-# inline initialized #-}
initialized (I# sz) e = ST $ \s0 -> case initialized# sz (unlift e) s0 of
  (# s1, a #) -> (# s1, MutableByteArray a #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
{-# inline copy# #-}
copy# dst doff src soff len =
  Exts.copyByteArray# src soff dst doff len

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src soff dst doff len

shows :: T -> String -> String
{-# inline shows #-}
shows = Prelude.shows
