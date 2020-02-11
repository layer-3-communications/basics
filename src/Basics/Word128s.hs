{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Word128s
  ( -- Types
    T
  , T#
  , R
    -- Lifting
  , lift
  , unlift
    -- Array
  , read#
  , write#
  , index#
  , uninitialized#
  , initialized#
  , copy#
  , copyMutable#
  , set#
  , shrink#
  , eq#
  , neq#
    -- Encode
  , shows
  ) where

import Prelude hiding (shows)

import GHC.Exts hiding (setByteArray#)
import Data.Primitive (PrimArray(PrimArray))
import Data.WideWord (Word128)

import qualified GHC.Exts as Exts

type T = PrimArray Word128
type T# = ByteArray#
type R = 'UnliftedRep

lift :: T# -> T
lift = PrimArray

unlift :: T -> T#
unlift (PrimArray x) = x

index# :: ArrayArray# -> Int# -> T#
index# = Exts.indexByteArrayArray#

read# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = Exts.readByteArrayArray#

write# :: MutableArrayArray# s -> Int# -> T# -> State# s -> State# s
write# = Exts.writeByteArrayArray#

set# :: MutableArrayArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

-- | This is very unsafe.
uninitialized# :: Int# -> State# s -> (# State# s, MutableArrayArray# s #)
uninitialized# = Exts.newArrayArray#

initialized# ::
     Int# -> T# -> State# s
  -> (# State# s, MutableArrayArray# s #)
initialized# n e s0 = case uninitialized# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

copy# :: MutableArrayArray# s -> Int# -> ArrayArray# -> Int# -> Int# -> State# s -> State# s
copy# dst doff src soff len =
  Exts.copyArrayArray# src soff dst doff len

copyMutable# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
copyMutable# dst doff src soff len =
  Exts.copyMutableArrayArray# src soff dst doff len

shrink# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableArrayArray# s #)
shrink# m sz s0 = case uninitialized# sz s0 of
  (# s1, dst #) -> case copyMutable# dst 0# m 0# sz s1 of
    s2 -> (# s2, dst #)

eq# :: ByteArray# -> ByteArray# -> Int#
eq# a b = case lenA ==# lenB of
  1# -> Exts.compareByteArrays# a 0# b 0# lenA ==# 0#
  _ -> 0#
  where
  !lenA = Exts.sizeofByteArray# a
  !lenB = Exts.sizeofByteArray# b

neq# :: ByteArray# -> ByteArray# -> Int#
neq# a b = case lenA ==# lenB of
  1# -> Exts.compareByteArrays# a 0# b 0# lenA /=# 0#
  _ -> 1#
  where
  !lenA = Exts.sizeofByteArray# a
  !lenB = Exts.sizeofByteArray# b

-- TODO: fix this
shows :: T -> String -> String
shows _ s = "[...]" ++ s

