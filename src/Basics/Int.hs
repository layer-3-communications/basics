{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Int
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
import GHC.Exts ((+#),(*#),(-#))
import GHC.Exts (Int(I#),RuntimeRep(IntRep))
import GHC.Exts (State#,MutableByteArray#,Int#,ByteArray#)
import GHC.ST (ST(ST))

import qualified Prelude
import qualified GHC.Exts as Exts
import qualified Foreign.Storable as FS

type T = Int
type T# = Int#
type R = 'IntRep

def :: T
def = 0

zero :: T
zero = 0

size :: Int
size = FS.sizeOf (undefined :: Int)

signed :: Bool
signed = True

lift :: T# -> T
lift = I#

unlift :: T -> T#
unlift (I# i) = i

plus :: T -> T -> T
plus (I# x) (I# y) = I# (x +# y)

minus :: T -> T -> T
minus (I# x) (I# y) = I# (x -# y)

times# :: T# -> T# -> T#
times# = (*#)

quot# :: T# -> T# -> T#
quot# = Exts.quotInt#

rem# :: T# -> T# -> T#
rem# = Exts.remInt#

plus# :: T# -> T# -> T#
plus# = (+#)

minus# :: T# -> T# -> T#
minus# = (-#)

gt# :: T# -> T# -> Int#
gt# = (Exts.>#)

lt# :: T# -> T# -> Int#
lt# = (Exts.<#)

gte# :: T# -> T# -> Int#
gte# = (Exts.>=#)

lte# :: T# -> T# -> Int#
lte# = (Exts.<=#)

eq# :: T# -> T# -> Int#
eq# = (Exts.==#)

neq# :: T# -> T# -> Int#
neq# = (Exts./=#)

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

index# :: ByteArray# -> Int# -> T#
index# = Exts.indexIntArray#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = Exts.readIntArray#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# = Exts.writeIntArray#

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
uninitialized# sz = Exts.newByteArray# (sz *# (case size of I# i -> i))

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
copy# dst doff src soff len = Exts.copyByteArray#
  src
  (soff *# (case size of I# i -> i))
  dst
  (doff *# (case size of I# i -> i))
  (len *# (case size of I# i -> i))

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutable# dst doff src soff len = Exts.copyMutableByteArray#
  src
  (soff *# (case size of I# i -> i))
  dst
  (doff *# (case size of I# i -> i))
  (len *# (case size of I# i -> i))

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# (case size of I# sz -> sz)) s0, m #)

shows :: T -> String -> String
shows = Prelude.shows
