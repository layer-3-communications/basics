{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

-- This provides an interface to bytearrays in which every
-- boolean is represented by a full byte, not a bit. This
-- can waste space, so depending on your use case, you may
-- want something different.
module Basics.Bool
  ( -- Types
    T
  , T#
  , R
    -- Lifting
  , lift
  , unlift
    -- Compare
  , eq#
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
  , def
    -- Metadata
  , size
  ) where

import GHC.Exts (RuntimeRep(IntRep))
import GHC.Exts (Int#,State#,MutableByteArray#,ByteArray#)
import qualified GHC.Exts as Exts

type T = Bool
type T# = Int#
type R = 'IntRep

def :: T
def = False

size :: Int
size = 1

lift :: T# -> T
lift x = Exts.tagToEnum# x :: Bool

unlift :: T -> T#
unlift = \case
  True -> 1#
  False -> 0#

eq# :: Int# -> Int# -> Int#
eq# = (Exts.==#)

index# :: ByteArray# -> Int# -> T#
index# = Exts.indexInt8Array#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = Exts.readInt8Array#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# = Exts.writeInt8Array#

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# = Exts.setByteArray#

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
