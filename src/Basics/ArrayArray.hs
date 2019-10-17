{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.ArrayArray
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
import Data.Bifunctor.Flip (Flip(Flip))
import Data.Primitive.Unlifted.Array (UnliftedArray(..),MutableUnliftedArray(..))

type A = UnliftedArray ()
type A# = ArrayArray#
type M = Flip MutableUnliftedArray ()
type M# = MutableArrayArray#
type R = 'UnliftedRep

lift :: A# -> A
lift = UnliftedArray

unlift :: A -> A#
unlift (UnliftedArray i) = i

liftMutable :: M# s -> M s
liftMutable x = Flip (MutableUnliftedArray x)

unliftMutable :: M s -> M# s
unliftMutable (Flip (MutableUnliftedArray i)) = i

unsafeFreeze# :: M# s -> State# s -> (# State# s, A# #)
unsafeFreeze# = unsafeFreezeArrayArray#

