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
{-# inline def #-}
def = 0

zero :: T
{-# inline zero #-}
zero = 0

size :: Int
{-# inline size #-}
size = FS.sizeOf (undefined :: Int)

signed :: Bool
{-# inline signed #-}
signed = True

lift :: T# -> T
{-# inline lift #-}
lift = I#

unlift :: T -> T#
{-# inline unlift #-}
unlift (I# i) = i

plus :: T -> T -> T
{-# inline plus #-}
plus (I# x) (I# y) = I# (x +# y)

minus :: T -> T -> T
{-# inline minus #-}
minus (I# x) (I# y) = I# (x -# y)

times# :: T# -> T# -> T#
{-# inline times# #-}
times# = (*#)

quot# :: T# -> T# -> T#
{-# inline quot# #-}
quot# = Exts.quotInt#

rem# :: T# -> T# -> T#
{-# inline rem# #-}
rem# = Exts.remInt#

plus# :: T# -> T# -> T#
{-# inline plus# #-}
plus# = (+#)

minus# :: T# -> T# -> T#
{-# inline minus# #-}
minus# = (-#)

gt# :: T# -> T# -> Int#
{-# inline gt# #-}
gt# = (Exts.>#)

lt# :: T# -> T# -> Int#
{-# inline lt# #-}
lt# = (Exts.<#)

gte# :: T# -> T# -> Int#
{-# inline gte# #-}
gte# = (Exts.>=#)

lte# :: T# -> T# -> Int#
{-# inline lte# #-}
lte# = (Exts.<=#)

eq# :: T# -> T# -> Int#
{-# inline eq# #-}
eq# = (Exts.==#)

neq# :: T# -> T# -> Int#
{-# inline neq# #-}
neq# = (Exts./=#)

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
index# = Exts.indexIntArray#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# = Exts.readIntArray#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# = Exts.writeIntArray#

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline uninitialized# #-}
uninitialized# sz = Exts.newByteArray# (sz *# (case size of I# i -> i))

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
copy# dst doff src soff len = Exts.copyByteArray#
  src
  (soff *# (case size of I# i -> i))
  dst
  (doff *# (case size of I# i -> i))
  (len *# (case size of I# i -> i))

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len = Exts.copyMutableByteArray#
  src
  (soff *# (case size of I# i -> i))
  dst
  (doff *# (case size of I# i -> i))
  (len *# (case size of I# i -> i))

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline shrink# #-}
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# (case size of I# sz -> sz)) s0, m #)

shows :: T -> String -> String
{-# inline shows #-}
shows = Prelude.shows
