{-# language CPP #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}
{-# language ViewPatterns #-}

#include <MachDeps.h>

module Basics.Word128
  ( -- Types
    T
  , T#
  , R
    -- Lifting
  , lift
  , unlift
    -- Compare
  , eq#
  , neq#
  , lt#
  , lte#
  , gt#
  , gt
  , gte
  , lt
  , lte
  , eq
  , neq
    -- Arithmetic
  , minus#
  , quot#
    -- Array
  , read#
  , write#
  , index#
  , read
  , write
  , index
  , set#
  , uninitialized#
  , initialized#
  , uninitialized
  , initialized
  , copy#
  , copyMutable#
  , shrink#
  , shrink
    -- Constants
  , def
  , zero
  , minBound
  , maxBound
    -- Metadata
  , size
    -- Encoding
  , shows
  ) where

import Prelude hiding (shows,minBound,maxBound,read)

import Data.Primitive (MutableByteArray(..),ByteArray(..))
import Data.WideWord.Word128 (Word128(Word128))
import GHC.Exts hiding (setByteArray#)
import GHC.ST (ST(ST))
import GHC.Word (Word64(W64#))

import qualified Prelude
import qualified GHC.Exts as Exts

type T = Word128
type T# = (# Word#, Word# #)
type R = 'TupleRep '[ 'WordRep, 'WordRep ]

def :: T
{-# inline def #-}
def = 0

zero :: T
{-# inline zero #-}
zero = 0

maxBound :: T
{-# inline maxBound #-}
maxBound = Word128 18446744073709551615 18446744073709551615

minBound :: T
{-# inline minBound #-}
minBound = 0

size :: Int
{-# inline size #-}
size = 16

lt# :: T# -> T# -> Int#
{-# inline lt# #-}
lt# (# a1, a2 #) (# b1, b2 #) = case ltWord# a1 b1 of
  1# -> 1#
  _ -> case eqWord# a1 b1 of
    1# -> ltWord# a2 b2
    _ -> 0#

gt# :: T# -> T# -> Int#
{-# inline gt# #-}
gt# (# a1, a2 #) (# b1, b2 #) = case gtWord# a1 b1 of
  1# -> 1#
  _ -> case eqWord# a1 b1 of
    1# -> gtWord# a2 b2
    _ -> 0#

gt :: T -> T -> Bool
{-# inline gt #-}
gt = (>)

lt :: T -> T -> Bool
{-# inline lt #-}
lt = (<)

gte :: T -> T -> Bool
{-# inline gte #-}
gte = (>=)

lte :: T -> T -> Bool
{-# inline lte #-}
lte = (<=)

lte# :: T# -> T# -> Int#
{-# inline lte# #-}
lte# (# a1, a2 #) (# b1, b2 #) = case ltWord# a1 a2 of
  1# -> 1#
  _ -> case eqWord# a1 b1 of
    1# -> leWord# a2 b2
    _ -> 0#

quot# :: T# -> T# -> T#
{-# inline quot# #-}
quot# (# a1, a2 #) (# b1, b2 #) =
  case quot (Word128 (W64# a1) (W64# a2)) (Word128 (W64# b1) (W64# b2)) of
    Word128 (W64# c1) (W64# c2) -> (# c1, c2 #)

minus# :: T# -> T# -> T#
{-# inline minus# #-}
minus# (# a1, a2 #) (# b1, b2 #) =
  case (Word128 (W64# a1) (W64# a2)) - (Word128 (W64# b1) (W64# b2)) of
    Word128 (W64# c1) (W64# c2) -> (# c1, c2 #)

lift :: T# -> T
{-# inline lift #-}
lift (# a, b #) = Word128 (W64# a) (W64# b)

unlift :: T -> T#
{-# inline unlift #-}
unlift (Word128 (W64# a) (W64# b)) = (# a, b #)

eq# :: T# -> T# -> Int#
{-# inline eq# #-}
eq# (# x1, y1 #) (# x2, y2 #) = ((eqWord# x1 x2) `andI#` (eqWord# y1 y2))

neq# :: T# -> T# -> Int#
{-# inline neq# #-}
neq# (# x1, y1 #) (# x2, y2 #) = ((neWord# x1 x2) `orI#` (neWord# y1 y2))

eq :: T -> T -> Bool
{-# inline eq #-}
eq = (==)

neq :: T -> T -> Bool
{-# inline neq #-}
neq = (/=)

index :: ByteArray -> Int -> T
{-# inline index #-}
index (ByteArray x) (I# i) = lift (index# x i)

read :: MutableByteArray s -> Int -> ST s T
{-# inline read #-}
read (MutableByteArray x) (I# i) = ST
  (\s0 -> case read# x i s0 of
    (# s1, r #) -> (# s1, lift r #)
  )

write :: MutableByteArray s -> Int -> T -> ST s ()
{-# inline write #-}
write (MutableByteArray x) (I# i) (unlift -> e) = ST (\s -> (# write# x i e s, () #) )

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline uninitialized# #-}
uninitialized# sz = Exts.newByteArray# (sz *# 16# )

initialized# ::
     Int# -> T# -> State# s
  -> (# State# s, MutableByteArray# s #)
{-# inline initialized# #-}
initialized# n e s0 = case uninitialized# n s0 of
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
  Exts.copyByteArray# src (soff *# 16#) dst (doff *# 16#) (len *# 16#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 16#) dst (doff *# 16#) (len *# 16#)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline shrink# #-}
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# 16#) s0, m #)

shrink :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
{-# inline shrink #-}
shrink (MutableByteArray x) (I# i) = ST
  (\s0 -> case shrink# x i s0 of
    (# s1, r #) -> (# s1, MutableByteArray r #)
  )

shows :: T -> String -> String
shows = Prelude.shows

#if WORDS_BIGENDIAN
index# :: ByteArray# -> Int# -> T#
{-# inline index# #-}
index# arr# i# =
  (# Exts.indexWordArray# arr# (2# *# i#)
  ,  Exts.indexWordArray# arr# ((2# *# i#) +# 1#) #)

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# arr# i# s0 = case Exts.readWordArray# arr# (2# *# i#) s0 of
  (# s1, i0 #) -> case Exts.readWordArray# arr# ((2# *# i#) +# 1#) s1 of
    (# s2, i1 #) -> (# s2, (# i0, i1 #) #)

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# arr# i# (# a, b #) s0 =
  case Exts.writeWordArray# arr# (2# *# i#) a s0 of
    s1 -> case Exts.writeWordArray# arr# ((2# *# i#) +# 1#) b s1 of
      s2 -> s2
#else
index# :: ByteArray# -> Int# -> T#
{-# inline index# #-}
index# arr# i# =
  (# Exts.indexWordArray# arr# ((2# *# i#) +# 1#)
  ,  Exts.indexWordArray# arr# (2# *# i#) #)

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# arr# i# s0 = case Exts.readWordArray# arr# ((2# *# i#) +# 1#) s0 of
  (# s1, i0 #) -> case Exts.readWordArray# arr# (2# *# i#) s1 of
    (# s2, i1 #) -> (# s2, (# i0, i1 #) #)

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# arr# i# (# a, b #) s0 =
  case Exts.writeWordArray# arr# ((2# *# i#) +# 1#) a s0 of
    s1 -> case Exts.writeWordArray# arr# (2# *# i#) b s1 of
      s2 -> s2
#endif
