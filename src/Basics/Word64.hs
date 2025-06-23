{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Word64
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

type T = Word64
type T# = Word64#
type R = 'Word64Rep

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
size = 8

maxBound :: T
{-# inline maxBound #-}
maxBound = 18446744073709551615

minBound :: T
{-# inline minBound #-}
minBound = 0

lift :: T# -> T
{-# inline lift #-}
lift = W64#

unlift :: T -> T#
{-# inline unlift #-}
unlift (W64# i) = i

plus :: T -> T -> T
{-# inline plus #-}
plus (W64# x) (W64# y) = W64# (plusWord64# x y)

minus :: T -> T -> T
{-# inline minus #-}
minus (W64# x) (W64# y) = W64# (subWord64# x y)

times# :: T# -> T# -> T#
{-# inline times# #-}
times# = timesWord64#

quot# :: T# -> T# -> T#
{-# inline quot# #-}
quot# = quotWord64#

rem# :: T# -> T# -> T#
{-# inline rem# #-}
rem# = remWord64#

plus# :: T# -> T# -> T#
{-# inline plus# #-}
plus# = plusWord64#

minus# :: T# -> T# -> T#
{-# inline minus# #-}
minus# = subWord64#

gt# :: T# -> T# -> Int#
{-# inline gt# #-}
gt# = gtWord64#

lt# :: T# -> T# -> Int#
{-# inline lt# #-}
lt# = ltWord64#

gte# :: T# -> T# -> Int#
{-# inline gte# #-}
gte# = geWord64#

lte# :: T# -> T# -> Int#
{-# inline lte# #-}
lte# = leWord64#

eq# :: T# -> T# -> Int#
{-# inline eq# #-}
eq# = eqWord64#

neq# :: T# -> T# -> Int#
{-# inline neq# #-}
neq# = neWord64#

gt :: T -> T -> Bool
{-# inline gt #-}
gt (W64# a) (W64# b) = Exts.isTrue# (gtWord64# a b)

lt :: T -> T -> Bool
{-# inline lt #-}
lt (W64# a) (W64# b) = Exts.isTrue# (ltWord64# a b)

gte :: T -> T -> Bool
{-# inline gte #-}
gte (W64# a) (W64# b) = Exts.isTrue# (geWord64# a b)

lte :: T -> T -> Bool
{-# inline lte #-}
lte (W64# a) (W64# b) = Exts.isTrue# (leWord64# a b)

eq :: T -> T -> Bool
{-# inline eq #-}
eq (W64# a) (W64# b) = Exts.isTrue# (eqWord64# a b)

neq :: T -> T -> Bool
{-# inline neq #-}
neq (W64# a) (W64# b) = Exts.isTrue# (neWord64# a b)

index# :: ByteArray# -> Int# -> T#
{-# inline index# #-}
index# = indexWord64Array#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# = readWord64Array#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# = writeWord64Array#

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline uninitialized# #-}
uninitialized# sz = Exts.newByteArray# (sz *# 8# )

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
  Exts.copyByteArray# src (soff *# 8#) dst (doff *# 8#) (len *# 8#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 8#) dst (doff *# 8#) (len *# 8#)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline shrink# #-}
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# 8#) s0, m #)

shows :: T -> String -> String
shows = Prelude.shows

index :: ByteArray -> Int -> T
{-# inline index #-}
index (ByteArray x) (I# i) = W64# (indexWord64Array# x i)

read :: MutableByteArray s -> Int -> ST s T
{-# inline read #-}
read (MutableByteArray x) (I# i) = ST
  (\s0 -> case readWord64Array# x i s0 of
    (# s1, r #) -> (# s1, W64# r #)
  )

write :: MutableByteArray s -> Int -> T -> ST s ()
{-# inline write #-}
write (MutableByteArray x) (I# i) (W64# e) = ST (\s -> (# writeWord64Array# x i e s, () #) )

shrink :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
{-# inline shrink #-}
shrink (MutableByteArray x) (I# i) = ST
  (\s0 -> case shrink# x i s0 of
    (# s1, r #) -> (# s1, MutableByteArray r #)
  )

