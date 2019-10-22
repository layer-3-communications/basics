{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Word128
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

import Data.WideWord.Word128 (Word128(Word128))
import GHC.Exts hiding (setByteArray#)
import GHC.Word (Word64(W64#))

import qualified Prelude
import qualified GHC.Exts as Exts

type T = Word128
type T# = (# Word#, Word# #)
type R = 'TupleRep '[ 'WordRep, 'WordRep ]

def :: T
def = 0

size :: Int
size = 16

lift :: T# -> T
lift (# a, b #) = Word128 (W64# a) (W64# b)

unlift :: T -> T#
unlift (Word128 (W64# a) (W64# b)) = (# a, b #)

eq# :: T# -> T# -> Int#
eq# (# x1, y1 #) (# x2, y2 #) = ((eqWord# x1 x2) `andI#` (eqWord# y1 y2))

neq# :: T# -> T# -> Int#
neq# (# x1, y1 #) (# x2, y2 #) = ((neWord# x1 x2) `orI#` (neWord# y1 y2))

index# :: ByteArray# -> Int# -> T#
index# arr# i# =
  (# Exts.indexWordArray# arr# (2# *# i#)
  ,  Exts.indexWordArray# arr# ((2# *# i#) +# 1#) #)

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# arr# i# s0 = case Exts.readWordArray# arr# (2# *# i#) s0 of
  (# s1, i0 #) -> case Exts.readWordArray# arr# ((2# *# i#) +# 1#) s1 of
    (# s2, i1 #) -> (# s2, (# i0, i1 #) #)

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# arr# i# (# a, b #) s0 =
  case Exts.writeWordArray# arr# (2# *# i#) a s0 of
    s1 -> case Exts.writeWordArray# arr# ((2# *# i#) +# 1#) b s1 of
      s2 -> s2

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
uninitialized# sz = Exts.newByteArray# (sz *# 16# )

initialized# ::
     Int# -> T# -> State# s
  -> (# State# s, MutableByteArray# s #)
initialized# n e s0 = case uninitialized# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
copy# dst doff src soff len =
  Exts.copyByteArray# src (soff *# 16#) dst (doff *# 16#) (len *# 16#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 16#) dst (doff *# 16#) (len *# 16#)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# 16#) s0, m #)

shows :: T -> String -> String
shows = Prelude.shows
