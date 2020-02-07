{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Word256
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

import Prelude hiding (shows,minBound,maxBound)

import Data.WideWord.Word256 (Word256(Word256))
import GHC.Exts hiding (setByteArray#)
import GHC.Word (Word64(W64#))

import qualified Data.WideWord.Word256 as Word256
import qualified GHC.Exts as Exts

type T = Word256
type T# = (# Word#, Word#, Word#, Word# #)
type R = 'TupleRep '[ 'WordRep, 'WordRep, 'WordRep, 'WordRep ]

lift :: T# -> T
lift (# a, b, c, d #) = Word256 (W64# a) (W64# b) (W64# c) (W64# d)

unlift :: T -> T#
unlift (Word256 (W64# a) (W64# b) (W64# c) (W64# d)) = (# a, b, c, d #)

eq# :: T# -> T# -> Int#
eq# (# x1, y1, z1, w1 #) (# x2, y2, z2, w2 #) =
  (eqWord# x1 x2) `andI#`
  (eqWord# y1 y2) `andI#`
  (eqWord# z1 z2) `andI#`
  (eqWord# w1 w2)

neq# :: T# -> T# -> Int#
neq# (# x1, y1, z1, w1 #) (# x2, y2, z2, w2 #) =
  (neWord# x1 x2) `orI#`
  (neWord# y1 y2) `orI#`
  (neWord# z1 z2) `orI#`
  (neWord# w1 w2)

index# :: ByteArray# -> Int# -> T#
index# arr# i# =
  (# Exts.indexWordArray# arr# (4# *# i#)
  ,  Exts.indexWordArray# arr# ((4# *# i#) +# 1#)
  ,  Exts.indexWordArray# arr# ((4# *# i#) +# 2#)
  ,  Exts.indexWordArray# arr# ((4# *# i#) +# 3#)
  #)

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# arr# i# s0 = case Exts.readWordArray# arr# (4# *# i#) s0 of
  (# s1, i0 #) -> case Exts.readWordArray# arr# ((4# *# i#) +# 1#) s1 of
    (# s2, i1 #) -> case Exts.readWordArray# arr# ((4# *# i#) +# 2#) s2 of
      (# s3, i2 #) -> case Exts.readWordArray# arr# ((4# *# i#) +# 3#) s3 of
        (# s4, i3 #) -> (# s4, (# i0, i1, i2, i3 #) #)

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# arr# i# (# a, b, c, d #) s0 =
  case Exts.writeWordArray# arr# (4# *# i#) a s0 of
    s1 -> case Exts.writeWordArray# arr# ((4# *# i#) +# 1#) b s1 of
      s2 -> case Exts.writeWordArray# arr# ((4# *# i#) +# 2#) c s2 of
        s3 -> case Exts.writeWordArray# arr# ((4# *# i#) +# 3#) d s3 of
          s4 -> s4

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
uninitialized# sz = Exts.newByteArray# (sz *# 32# )

initialized# ::
     Int# -> T# -> State# s
  -> (# State# s, MutableByteArray# s #)
initialized# n e s0 = case uninitialized# n s0 of
  (# s1, a #) -> case set# a 0# n e s1 of
    s2 -> (# s2, a #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
copy# dst doff src soff len =
  Exts.copyByteArray# src (soff *# 32#) dst (doff *# 32#) (len *# 32#)

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutable# dst doff src soff len =
  Exts.copyMutableByteArray# src (soff *# 32#) dst (doff *# 32#) (len *# 32#)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# 32#) s0, m #)

shows :: T -> String -> String
shows x = (Word256.showHexWord256 x ++)
