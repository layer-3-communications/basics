{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Word32
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
  , gt
  , lt
  , gte
  , lte
  , eq
  , neq
    -- Arithmetic
  , minus#
  , quot#
  , rem#
    -- Array
  , read#
  , write#
  , index#
  , read
  , write
  , index
  , uninitialized#
  , initialized#
  , uninitialized
  , initialized
  , copy#
  , copyMutable#
  , set#
  , shrink#
  , shrink
    -- Constants
  , zero
  , def
  , maxBound
  , minBound
    -- Metadata
  , signed
  , size
    -- Encoding
  , shows
  ) where

import Prelude hiding (shows,minBound,maxBound,read)

import GHC.Exts hiding (setByteArray#)
import GHC.Word
import GHC.ST (ST(ST))
import Data.Primitive (MutableByteArray(..),ByteArray(..))

import qualified Prelude
import qualified GHC.Exts as Exts

type T = Word32
type T# = Word32#
type R = 'Word32Rep

def :: T
{-# inline def #-}
def = 0

zero :: T
{-# inline zero #-}
zero = 0

maxBound :: T
{-# inline maxBound #-}
maxBound = 4294967295

minBound :: T
{-# inline minBound #-}
minBound = 0

signed :: Bool
{-# inline signed #-}
signed = False

size :: Int
{-# inline size #-}
size = 4

lift :: T# -> T
{-# inline lift #-}
lift i = W32# i

unlift :: T -> T#
{-# inline unlift #-}
unlift (W32# i) = i

gt# :: T# -> T# -> Int#
{-# inline gt# #-}
gt# = gtWord32#

lt# :: T# -> T# -> Int#
{-# inline lt# #-}
lt# = ltWord32#

gte# :: T# -> T# -> Int#
{-# inline gte# #-}
gte# = geWord32#

lte# :: T# -> T# -> Int#
{-# inline lte# #-}
lte# = leWord32#

eq# :: T# -> T# -> Int#
{-# inline eq# #-}
eq# = eqWord32#

neq# :: T# -> T# -> Int#
{-# inline neq# #-}
neq# = neWord32#

gt :: T -> T -> Bool
{-# inline gt #-}
gt (W32# a) (W32# b) = Exts.isTrue# (gtWord32# a b)

lt :: T -> T -> Bool
{-# inline lt #-}
lt (W32# a) (W32# b) = Exts.isTrue# (ltWord32# a b)

gte :: T -> T -> Bool
{-# inline gte #-}
gte (W32# a) (W32# b) = Exts.isTrue# (geWord32# a b)

lte :: T -> T -> Bool
{-# inline lte #-}
lte (W32# a) (W32# b) = Exts.isTrue# (leWord32# a b)

eq :: T -> T -> Bool
{-# inline eq #-}
eq (W32# a) (W32# b) = Exts.isTrue# (eqWord32# a b)

neq :: T -> T -> Bool
{-# inline neq #-}
neq (W32# a) (W32# b) = Exts.isTrue# (neWord32# a b)

minus# :: T# -> T# -> T#
{-# inline minus# #-}
minus# x y = subWord32# x y

quot# :: T# -> T# -> T#
{-# inline quot# #-}
quot# = quotWord32#

rem# :: T# -> T# -> T#
{-# inline rem# #-}
rem# = remWord32#

index# :: ByteArray# -> Int# -> T#
{-# inline index# #-}
index# arr i = indexWord32Array# arr i

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# arr i st =
  let !(# st', v #) = readWord32Array# arr i st
   in (# st', v #)

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# arr i v st = writeWord32Array# arr i v st

index :: ByteArray -> Int -> T
{-# inline index #-}
index (ByteArray x) (I# i) = W32# (indexWord32Array# x i)

read :: MutableByteArray s -> Int -> ST s T
{-# inline read #-}
read (MutableByteArray x) (I# i) = ST
  (\s0 -> case readWord32Array# x i s0 of
    (# s1, r #) -> (# s1, W32# r #)
  )

write :: MutableByteArray s -> Int -> T -> ST s ()
{-# inline write #-}
write (MutableByteArray x) (I# i) (W32# e) = ST (\s -> (# writeWord32Array# x i e s, () #) )

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline uninitialized# #-}
uninitialized# sz = Exts.newByteArray# (sz *# 4# )

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
  Exts.copyByteArray# src (soff *# 4#) dst (doff *# 4#) (len *# 4#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 4#) dst (doff *# 4#) (len *# 4#)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline shrink# #-}
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# 4#) s0, m #)

shrink :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
{-# inline shrink #-}
shrink (MutableByteArray x) (I# i) = ST
  (\s0 -> case shrink# x i s0 of
    (# s1, r #) -> (# s1, MutableByteArray r #)
  )

shows :: T -> String -> String
shows = Prelude.shows
