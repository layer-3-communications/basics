{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.ByteArray
  ( -- Types
    A
  , M
  , R
  , A#
  , M#
    -- Lifting
  , lift
  , unlift
  , liftMutable
  , unliftMutable
    -- Array
  , unsafeFreeze#
  ) where

import GHC.Exts hiding (setByteArray#)
import Data.Primitive (ByteArray(..),MutableByteArray(..))

type A = ByteArray
type A# = ByteArray#
type M = MutableByteArray
type M# = MutableByteArray#
type R = 'UnliftedRep

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
