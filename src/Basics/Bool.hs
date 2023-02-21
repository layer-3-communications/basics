{-# language BangPatterns #-}
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
  , def
    -- Metadata
  , size
    -- Encoding
  , shows
  ) where

import Prelude hiding (shows)

import GHC.Exts (RuntimeRep(IntRep))
import GHC.Exts (Int#,State#,MutableByteArray#,ByteArray#)

import qualified Prelude
import qualified GHC.Exts as Exts

type T = Bool
type T# = Int#
type R = 'IntRep

def :: T
{-# inline def #-}
def = False

size :: Int
{-# inline size #-}
size = 1

lift :: T# -> T
{-# inline lift #-}
lift x = Exts.tagToEnum# x :: Bool

unlift :: T -> T#
{-# inline unlift #-}
unlift = \case
  True -> 1#
  False -> 0#

eq# :: T# -> T# -> Int#
{-# inline eq# #-}
eq# = (Exts.==#)

neq# :: T# -> T# -> Int#
{-# inline neq# #-}
neq# = (Exts./=#)

index# :: ByteArray# -> Int# -> T#
{-# inline index# #-}
index# arr i = Exts.int8ToInt# (Exts.indexInt8Array# arr i)

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# arr i st =
  let !(# st', v #) = Exts.readInt8Array# arr i st
   in (# st', Exts.int8ToInt# v #)

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# arr i v st = Exts.writeInt8Array# arr i (Exts.intToInt8# v) st

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# = Exts.setByteArray#

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline shrink# #-}
shrink# m i s = (# Exts.shrinkMutableByteArray# m i s, m #)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline uninitialized# #-}
uninitialized# = Exts.newByteArray#

initialized# :: Int# -> T# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline initialized# #-}
initialized# n e s0 = case Exts.newByteArray# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
{-# inline copy# #-}
copy# dst doff src soff len =
  Exts.copyByteArray# src soff dst doff len

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src soff dst doff len

shows :: T -> String -> String
shows = Prelude.shows
