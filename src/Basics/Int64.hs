{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Int64
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
  , uninitialized#
  , initialized#
  , copy#
  , copyMutable#
  , set#
    -- Constants
  , zero
  , def
    -- Metadata
  , signed
  ) where

import GHC.Exts
import GHC.Int

import qualified GHC.Exts as Exts

type T = Int64
type T# = Int#
type R = 'IntRep

def :: T
def = 0

zero :: T
zero = 0

signed :: Bool
signed = True

lift :: T# -> T
lift = I64#

unlift :: T -> T#
unlift (I64# i) = i

plus :: T -> T -> T
plus (I64# x) (I64# y) = I64# (x +# y)

minus :: T -> T -> T
minus (I64# x) (I64# y) = I64# (x -# y)

times# :: T# -> T# -> T#
times# = (*#)

quot# :: T# -> T# -> T#
quot# = quotInt#

rem# :: T# -> T# -> T#
rem# = remInt#

plus# :: T# -> T# -> T#
plus# = (+#)

minus# :: T# -> T# -> T#
minus# = (-#)

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
index# = indexIntArray#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = readIntArray#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# = writeIntArray#

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
uninitialized# sz = Exts.newByteArray# (sz *# 8# )

initialized# ::
     Int# -> T# -> State# s
  -> (# State# s, MutableByteArray# s #)
initialized# n e s0 = case uninitialized# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
copy# dst doff src soff len =
  Exts.copyByteArray# src (soff *# 8#) dst (doff *# 8#) (len *# 8#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 8#) dst (doff *# 8#) (len *# 8#)
