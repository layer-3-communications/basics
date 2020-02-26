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
type T# = Word#
type R = 'WordRep

def :: T
def = 0

zero :: T
zero = 0

maxBound :: T
maxBound = 4294967295

minBound :: T
minBound = 0

signed :: Bool
signed = False

size :: Int
size = 4

lift :: T# -> T
lift = W32#

unlift :: T -> T#
unlift (W32# i) = i

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

gt :: T -> T -> Bool
gt = (>)

lt :: T -> T -> Bool
lt = (<)

gte :: T -> T -> Bool
gte = (>=)

lte :: T -> T -> Bool
lte = (<=)

eq :: T -> T -> Bool
eq = (==)

neq :: T -> T -> Bool
neq = (/=)

minus# :: T# -> T# -> T#
minus# x y = narrow32Word# (minusWord# x y)

quot# :: T# -> T# -> T#
quot# = quotWord#

rem# :: T# -> T# -> T#
rem# = remWord#

index# :: ByteArray# -> Int# -> T#
index# = indexWord32Array#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = readWord32Array#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# = writeWord32Array#

index :: ByteArray -> Int -> T
index (ByteArray x) (I# i) = W32# (indexWord32Array# x i)

read :: MutableByteArray s -> Int -> ST s T
read (MutableByteArray x) (I# i) = ST
  (\s0 -> case readWord32Array# x i s0 of
    (# s1, r #) -> (# s1, W32# r #)
  )

write :: MutableByteArray s -> Int -> T -> ST s ()
write (MutableByteArray x) (I# i) (W32# e) = ST (\s -> (# writeWord32Array# x i e s, () #) )

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
uninitialized# sz = Exts.newByteArray# (sz *# 4# )

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
  Exts.copyByteArray# src (soff *# 4#) dst (doff *# 4#) (len *# 4#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 4#) dst (doff *# 4#) (len *# 4#)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# 4#) s0, m #)

shrink :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
shrink (MutableByteArray x) (I# i) = ST
  (\s0 -> case shrink# x i s0 of
    (# s1, r #) -> (# s1, MutableByteArray r #)
  )

shows :: T -> String -> String
shows = Prelude.shows
