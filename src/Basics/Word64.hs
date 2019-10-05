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
  , size
  ) where

import GHC.Exts hiding (setByteArray#)
import GHC.Word

import qualified GHC.Exts as Exts

type T = Word64
type T# = Word#
type R = 'WordRep

def :: T
def = 0

zero :: T
zero = 0

signed :: Bool
signed = False

size :: Int
size = 8

lift :: T# -> T
lift = W64#

unlift :: T -> T#
unlift (W64# i) = i

plus :: T -> T -> T
plus (W64# x) (W64# y) = W64# (plusWord# x y)

minus :: T -> T -> T
minus (W64# x) (W64# y) = W64# (minusWord# x y)

times# :: T# -> T# -> T#
times# = timesWord#

quot# :: T# -> T# -> T#
quot# = quotWord#

rem# :: T# -> T# -> T#
rem# = remWord#

plus# :: T# -> T# -> T#
plus# = plusWord#

minus# :: T# -> T# -> T#
minus# = minusWord#

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
index# = indexWordArray#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = readWordArray#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# = writeWordArray#

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
