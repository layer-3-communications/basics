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
def = 0

zero :: T
zero = 0

signed :: Bool
signed = False

size :: Int
size = 2

maxBound :: T
maxBound = 65535

minBound :: T
minBound = 0

lift :: T# -> T
lift = W16#

unlift :: T -> T#
unlift (W16# i) = i

plus :: T -> T -> T
plus (W16# x) (W16# y) = W16# (plusWord# x y)

minus :: T -> T -> T
minus (W16# x) (W16# y) = W16# (minusWord# x y)

times# :: T# -> T# -> T#
times# = timesWord#

quot# :: T# -> T# -> T#
quot# = quotWord#

rem# :: T# -> T# -> T#
rem# = remWord#

plus# :: T# -> T# -> T#
plus# = plusWord#

minus# :: T# -> T# -> T#
minus# x y = narrow16Word# (minusWord# x y)

gt# :: T# -> T# -> Int#
gt# = gtWord#

lt# :: T# -> T# -> Int#
lt# = ltWord#

gte# :: T# -> T# -> Int#
gte# = geWord#

lte# :: T# -> T# -> Int#
lte# = leWord#

eq# :: T# -> T# -> Int#
eq# = eqWord#

neq# :: T# -> T# -> Int#
neq# = neWord#

index# :: ByteArray# -> Int# -> T#
index# = indexWord16Array#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = readWord16Array#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# = writeWord16Array#

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
uninitialized# sz = Exts.newByteArray# (sz *# 2# )

initialized# ::
     Int# -> T# -> State# s
  -> (# State# s, MutableByteArray# s #)
initialized# n e s0 = case uninitialized# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

uninitialized :: Int -> ST s (MutableByteArray s)
uninitialized (I# sz) = ST $ \s0 -> case uninitialized# sz s0 of
  (# s1, a #) -> (# s1, MutableByteArray a #)

initialized :: Int -> T -> ST s (MutableByteArray s)
initialized (I# sz) e = ST $ \s0 -> case initialized# sz (unlift e) s0 of
  (# s1, a #) -> (# s1, MutableByteArray a #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
copy# dst doff src soff len =
  Exts.copyByteArray# src (soff *# 2#) dst (doff *# 2#) (len *# 2#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 2#) dst (doff *# 2#) (len *# 2#)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
shrink# m i s = (# Exts.shrinkMutableByteArray# m (i *# 2# ) s, m #)

shows :: T -> String -> String
shows = Prelude.shows

shrink :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
shrink (MutableByteArray x) (I# i) = ST
  (\s0 -> case shrink# x i s0 of
    (# s1, r #) -> (# s1, MutableByteArray r #)
  )

index :: ByteArray -> Int -> T
index (ByteArray x) (I# i) = W16# (indexWord16Array# x i)

read :: MutableByteArray s -> Int -> ST s T
read (MutableByteArray x) (I# i) = ST
  (\s0 -> case readWord16Array# x i s0 of
    (# s1, r #) -> (# s1, W16# r #)
  )

write :: MutableByteArray s -> Int -> T -> ST s ()
write (MutableByteArray x) (I# i) (W16# e) = ST (\s -> (# writeWord16Array# x i e s, () #) )
