{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- | This module defines boxed and unboxed versions of a one-bit signed integer type.
-- That is, it holds values 0 and -1.
--
-- Alternately, one could think about `Int1` as holding ℕ₂ (integers modulo 2).
-- Indeed, all arithmetic is done on machine ints, but only the last bit is significant.
-- As a signed type, the bit pattern @0xb1@ represents -1,
-- but it's just as easy (and perhaps more natural) to think of it as 1.
-- This might show up in the definition of 'Ord' (if we implement one),
-- but it also shows up in the instance for 'Show', where we print 1 as @"1"@.
--
-- A note on representation: we are using 'IntRep' as the representation for 'Int1#'.
-- This might change in the future (perhaps to 'Int8Rep').
-- However, we do promise that 'toInt#' will always return either zero or one
-- (i.e. all high-order bits are zero),
-- and that 'fromInt#' will always ignore high-order bits.
-- The same guarantees hold for any other functions that convert 'Int1' to/from integers.
module Data.Int1
  ( Int1(..)
  -- ** Conversions
  , toInt
  , fromInt
  -- * Unboxed
  , Int1#
  , isTrue#
  , eq#
  , neq#
  , not#
  , xor#
  , or#
  , and#
  -- ** Conversions
  , toInt#
  , fromInt#
  , sextToInt#
  , unsafeFromInt#
  ) where

import GHC.Exts (Int(I#),Int#)

import qualified GHC.Exts as Exts

-- | A fixed-precision signed integer data type exact range @[-1, 0]@.
data Int1 = I1 !Int1#

-- Unboxed version of 'Int1'.
newtype Int1# = I1# Int#

-- | Returns 'False' when its parameter is zero,
-- Returns 'True' otherwise (i.e. when its parameter is -1).
isTrue# :: Int1# -> Bool
{-# INLINE isTrue# #-}
isTrue# (I1# n) = Exts.isTrue# n

-- | Return one when two values are equal,
-- Return zero otherwise.
eq# :: Int1# -> Int1# -> Int#
{-# INLINE eq# #-}
eq# (I1# a) (I1# b) = a Exts.==# b

-- | Return one when two values are not equal,
-- Return zero otherwise.
neq# :: Int1# -> Int1# -> Int#
{-# INLINE neq# #-}
neq# (I1# a) (I1# b) = a Exts./=# b


-- | Bitwise "not", also known as binary complement.
not# :: Int1# -> Int1#
{-# INLINE not# #-}
not# (I1# n) = I1# (Exts.notI# n)

-- | Bitwise "xor".
xor# :: Int1# -> Int1# -> Int1#
{-# INLINE xor# #-}
xor# (I1# a) (I1# b) = I1# (a `Exts.xorI#` b)

-- | Bitwise "or".
or# :: Int1# -> Int1# -> Int1#
{-# INLINE or# #-}
or# (I1# a) (I1# b) = I1# (a `Exts.orI#` b)

-- | Bitwise "and".
and# :: Int1# -> Int1# -> Int1#
{-# INLINE and# #-}
and# (I1# a) (I1# b) = I1# (a `Exts.andI#` b)

instance Show Int1 where
  show (I1 (I1# n)) = show (I# n)

instance Eq Int1 where
  (I1 a) == (I1 b) = Exts.isTrue# (a `eq#` b)

instance Num Int1 where
  fromInteger n = fromInt (fromInteger n)
  negate (I1 n) = I1 (not# n)
  (I1 a) + (I1 b) = I1 (a `xor#` b)
  (I1 a) * (I1 b) = I1 (a `and#` b)
  abs n = n
  signum n = n

toInt :: Int1 -> Int
{-# INLINE toInt #-}
toInt (I1 n) = I# (toInt# n)

fromInt :: Int -> Int1
{-# INLINE fromInt #-}
fromInt (I# n) = I1 (fromInt# n)

toInt# :: Int1# -> Int#
{-# INLINE toInt# #-}
toInt# (I1# n) = n

-- | Return all zero bits when the parameter is 0, but all set bits for when it's -1.
sextToInt# :: Int1# -> Int#
{-# INLINE sextToInt# #-}
sextToInt# (I1# 0#) = 0#
sextToInt# _ = Exts.notI# 0#

-- | Return zero when the low-order bit is clear, one otherwise.
fromInt# :: Int# -> Int1#
{-# INLINE fromInt# #-}
fromInt# n = I1# (n `Exts.andI#` 1#)

-- | As 'fromInt#', but may cause data corruption if the parameter is not either 0 or 1.
-- Saves one bitwise-and oepration.
unsafeFromInt# :: Int# -> Int1#
{-# INLINE unsafeFromInt# #-}
unsafeFromInt# = I1#
