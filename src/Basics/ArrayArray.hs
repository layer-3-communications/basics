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
import Data.Primitive.Unlifted.Array (UnliftedArray,MutableUnliftedArray)
import Data.Primitive.Unlifted.Array.ST (UnliftedArray_(UnliftedArray))
import Data.Primitive.Unlifted.Array.Primops (UnliftedArray#(UnliftedArray#))
import Data.Primitive.Unlifted.Array.ST (MutableUnliftedArray_(MutableUnliftedArray))
import Data.Primitive.Unlifted.Array.Primops (MutableUnliftedArray#(MutableUnliftedArray#))
import GHC.Exts (ArrayArray#(ArrayArray#))
import Data.Primitive (ByteArray)

import qualified GHC.Exts as Exts

type A = UnliftedArray_ ByteArray# ByteArray
type A# = ArrayArray#
type M = Flip (MutableUnliftedArray_ ByteArray#) ()
type M# = MutableArrayArray#
type R = 'BoxedRep 'Unlifted

lift :: A# -> A
{-# inline lift #-}
lift (Exts.ArrayArray# x) = UnliftedArray (UnliftedArray# x)

unlift :: A -> A#
{-# inline unlift #-}
unlift (UnliftedArray (UnliftedArray# x)) = Exts.ArrayArray# x

liftMutable :: M# s -> M s
{-# inline liftMutable #-}
liftMutable (Exts.MutableArrayArray# x) = Flip (MutableUnliftedArray (MutableUnliftedArray# x))

unliftMutable :: M s -> M# s
{-# inline unliftMutable #-}
unliftMutable (Flip (MutableUnliftedArray (MutableUnliftedArray# i))) = Exts.MutableArrayArray# i

unsafeFreeze# :: M# s -> State# s -> (# State# s, A# #)
{-# inline unsafeFreeze# #-}
unsafeFreeze# = unsafeFreezeArrayArray#
