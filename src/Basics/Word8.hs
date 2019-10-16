{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Word8
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
    -- Array
  , read#
  , write#
  , index#
  , set#
  , uninitialized#
  , initialized#
  , copy#
  , copyMutable#
  , shrink#
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

type T = Word8
type T# = Word#
type R = 'WordRep

def :: T
def = 0

zero :: T
zero = 0

signed :: Bool
signed = False

size :: Int
size = 1

lift :: T# -> T
lift = W8#

unlift :: T -> T#
unlift (W8# i) = i

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
index# = indexWord8Array#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = readWord8Array#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# = writeWord8Array#

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = Exts.setByteArray# marr off len (word2Int# x) s

shrink# :: MutableByteArray# s -> Int# -> State# s -> State# s
shrink# = Exts.shrinkMutableByteArray#

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
uninitialized# = Exts.newByteArray#

initialized# :: Int# -> T# -> State# s -> (# State# s, MutableByteArray# s #)
initialized# n e s0 = case Exts.newByteArray# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
copy# dst doff src soff len =
  Exts.copyByteArray# src soff dst doff len

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src soff dst doff len
