{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.ShortTexts
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
import Data.Primitive.Unlifted.Array (UnliftedArray(..))
import Data.Text.Short (ShortText)

import qualified GHC.Exts as Exts

type T = UnliftedArray ShortText
type T# = ArrayArray#
type R = 'BoxedRep 'Unlifted

lift :: T# -> T
{-# inline lift #-}
lift = UnliftedArray

unlift :: T -> T#
{-# inline unlift #-}
unlift (UnliftedArray x) = x

index# :: ArrayArray# -> Int# -> T#
{-# inline index# #-}
index# = Exts.indexArrayArrayArray#

read# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# = Exts.readArrayArrayArray#

write# :: MutableArrayArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# = Exts.writeArrayArrayArray#

set# :: MutableArrayArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

-- | This is very unsafe.
uninitialized# :: Int# -> State# s -> (# State# s, MutableArrayArray# s #)
{-# inline uninitialized# #-}
uninitialized# = Exts.newArrayArray#

initialized# ::
     Int# -> T# -> State# s
  -> (# State# s, MutableArrayArray# s #)
{-# inline initialized# #-}
initialized# n e s0 = case uninitialized# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

copy# :: MutableArrayArray# s -> Int# -> ArrayArray# -> Int# -> Int# -> State# s -> State# s
{-# inline copy# #-}
copy# dst doff src soff len =
  Exts.copyArrayArray# src soff dst doff len

copyMutable# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst doff src soff len =
  Exts.copyMutableArrayArray# src soff dst doff len

shrink# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableArrayArray# s #)
{-# inline shrink# #-}
shrink# m sz s0 = case uninitialized# sz s0 of
  (# s1, dst #) -> case copyMutable# dst 0# m 0# sz s1 of
    s2 -> (# s2, dst #)

-- We do not forcibly inline this because it might be useful to see this
-- in profiling runs.
eq# :: ArrayArray# -> ArrayArray# -> Int#
eq# a b = case lenA ==# lenB of
  1# ->
    let go (-1#) = 1#
        go ix =
          let x = Exts.indexByteArrayArray# a ix
              y = Exts.indexByteArrayArray# b ix
              lenX = Exts.sizeofByteArray# x
              lenY = Exts.sizeofByteArray# y
           in case lenX ==# lenY of
                1# -> case Exts.compareByteArrays# x 0# y 0# lenX of
                  0# -> go (ix -# 1#)
                  _ -> 0#
                _ -> 0#
     in go (lenA -# 1#)
  _ -> 0#
  where
  !lenA = Exts.sizeofArrayArray# a
  !lenB = Exts.sizeofArrayArray# b

neq# :: ArrayArray# -> ArrayArray# -> Int#
{-# inline neq# #-}
neq# a b = case eq# a b of
  1# -> 0#
  _ -> 1#

-- TODO: fix this
shows :: T -> String -> String
{-# inline shows #-}
shows _ s = "[...]" ++ s
