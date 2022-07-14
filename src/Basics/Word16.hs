{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Word16
  ( -- Types
    T
  , T#
  , R
    -- Lifting
  , lift
  , unlift
    -- Arithmetic
  , plus
  , minus
  , plus#
  , minus#
  , times#
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
  , def
  , zero
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

type T = Word16
type T# = Word#
type R = 'WordRep

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
size = 2

maxBound :: T
{-# inline maxBound #-}
maxBound = 65535

minBound :: T
{-# inline minBound #-}
minBound = 0

lift :: T# -> T
{-# inline lift #-}
lift i = W16# (Exts.wordToWord16# i)

unlift :: T -> T#
{-# inline unlift #-}
unlift (W16# i) = Exts.word16ToWord# i

plus :: T -> T -> T
{-# inline plus #-}
plus (W16# x) (W16# y) = W16# (plusWord16# x y)

minus :: T -> T -> T
{-# inline minus #-}
minus (W16# x) (W16# y) = W16# (subWord16# x y)

times# :: T# -> T# -> T#
{-# inline times# #-}
times# = timesWord#

quot# :: T# -> T# -> T#
{-# inline quot# #-}
quot# = quotWord#

rem# :: T# -> T# -> T#
{-# inline rem# #-}
rem# = remWord#

plus# :: T# -> T# -> T#
{-# inline plus# #-}
plus# = plusWord#

minus# :: T# -> T# -> T#
{-# inline minus# #-}
minus# x y = narrow16Word# (minusWord# x y)

gt# :: T# -> T# -> Int#
{-# inline gt# #-}
gt# = gtWord#

lt# :: T# -> T# -> Int#
{-# inline lt# #-}
lt# = ltWord#

gte# :: T# -> T# -> Int#
{-# inline gte# #-}
gte# = geWord#

lte# :: T# -> T# -> Int#
{-# inline lte# #-}
lte# = leWord#

eq# :: T# -> T# -> Int#
{-# inline eq# #-}
eq# = eqWord#

neq# :: T# -> T# -> Int#
{-# inline neq# #-}
neq# = neWord#

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

eq :: T -> T -> Bool
{-# inline eq #-}
eq = (==)

neq :: T -> T -> Bool
{-# inline neq #-}
neq = (/=)

index# :: ByteArray# -> Int# -> T#
{-# inline index# #-}
index# arr i = Exts.word16ToWord# (indexWord16Array# arr i)

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# arr i st =
  let !(# st', v #) = readWord16Array# arr i st
   in (# st', Exts.word16ToWord# v #)

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# arr i v st = writeWord16Array# arr i (Exts.wordToWord16# v) st

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline uninitialized# #-}
uninitialized# sz = Exts.newByteArray# (sz *# 2# )

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
  Exts.copyByteArray# src (soff *# 2#) dst (doff *# 2#) (len *# 2#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 2#) dst (doff *# 2#) (len *# 2#)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline shrink# #-}
shrink# m i s = (# Exts.shrinkMutableByteArray# m (i *# 2# ) s, m #)

shows :: T -> String -> String
shows = Prelude.shows

shrink :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
{-# inline shrink #-}
shrink (MutableByteArray x) (I# i) = ST
  (\s0 -> case shrink# x i s0 of
    (# s1, r #) -> (# s1, MutableByteArray r #)
  )

index :: ByteArray -> Int -> T
{-# inline index #-}
index (ByteArray x) (I# i) = W16# (indexWord16Array# x i)

read :: MutableByteArray s -> Int -> ST s T
{-# inline read #-}
read (MutableByteArray x) (I# i) = ST
  (\s0 -> case readWord16Array# x i s0 of
    (# s1, r #) -> (# s1, W16# r #)
  )

write :: MutableByteArray s -> Int -> T -> ST s ()
{-# inline write #-}
write (MutableByteArray x) (I# i) (W16# e) = ST (\s -> (# writeWord16Array# x i e s, () #) )
