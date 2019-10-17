{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Word
  ( -- Types
    T
  , T#
  , R
    -- Lifting
  , lift
  , unlift
    -- Arithmetic
  , plus
  , minus
  , plus#
  , minus#
  , times#
  , quot#
  , rem#
    -- Compare
  , gt#
  , lt#
  , gte#
  , lte#
  , eq#
  , neq#
    -- Array
  , readByteArray#
  , writeByteArray#
  , indexByteArray#
  , shrink#
    -- Metadata
  , signed
  , size
  ) where

import GHC.Exts
import qualified GHC.Exts as Exts
import qualified Foreign.Storable as FS

type T = Word
type T# = Word#
type R = 'WordRep

size :: Int
size = FS.sizeOf (undefined :: Word)

signed :: Bool
signed = False

lift :: T# -> T
lift = W#

unlift :: T -> T#
unlift (W# i) = i

plus :: T -> T -> T
plus (W# x) (W# y) = W# (plusWord# x y)

minus :: T -> T -> T
minus (W# x) (W# y) = W# (minusWord# x y)

times# :: T# -> T# -> T#
times# = timesWord#

quot# :: T# -> T# -> T#
quot# = quotWord#

rem# :: T# -> T# -> T#
rem# = remWord#

plus# :: T# -> T# -> T#
plus# = plusWord#

minus# :: T# -> T# -> T#
minus# = minusWord#

gt# :: T# -> T# -> Int#
gt# = gtWord#

lt# :: T# -> T# -> Int#
lt# = ltWord#

gte# :: T# -> T# -> Int#
gte# = geWord#

lte# :: T# -> T# -> Int#
lte# = leWord#

eq# :: T# -> T# -> Int#
eq# = eqWord#

neq# :: T# -> T# -> Int#
neq# = neWord#

indexByteArray# :: ByteArray# -> Int# -> T#
indexByteArray# = indexWordArray#

readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
readByteArray# = readWordArray#

writeByteArray# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
writeByteArray# = writeWordArray#

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# (case size of I# sz -> sz)) s0, m #)
