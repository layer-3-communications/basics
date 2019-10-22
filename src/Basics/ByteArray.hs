{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

-- This can be used with vex-indef as either the element module
-- or the array module.
module Basics.ByteArray
  ( -- Types
    A
  , M
  , R
  , A#
  , M#
    -- Element Types
  , T
  , T# 
    -- Lifting
  , lift
  , unlift
  , liftMutable
  , unliftMutable
    -- Array
  , unsafeFreeze#
    -- Array Element
  , read#
  , write#
  , index#
  , set#
  ) where

import GHC.Exts hiding (setByteArray#)
import Data.Primitive (ByteArray(..),MutableByteArray(..))

import qualified GHC.Exts as Exts

type A = ByteArray
type A# = ByteArray#
type M = MutableByteArray
type M# = MutableByteArray#
type R = 'UnliftedRep

type T = ByteArray
type T# = ByteArray#

lift :: A# -> A
lift = ByteArray

unlift :: A -> A#
unlift (ByteArray i) = i

liftMutable :: M# s -> M s
liftMutable = MutableByteArray

unliftMutable :: M s -> M# s
unliftMutable (MutableByteArray i) = i

unsafeFreeze# :: M# s -> State# s -> (# State# s, A# #)
unsafeFreeze# = unsafeFreezeByteArray#

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
