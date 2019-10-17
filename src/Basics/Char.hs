{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Basics.Char
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
  , set#
  , shrink#
    -- Constants
  , def
    -- Metadata
  , size
  ) where

import GHC.Exts hiding (setByteArray#)

import qualified Foreign.Storable as FS
import qualified GHC.Exts as Exts

type T = Char
type T# = Char#
type R = 'WordRep

def :: T
def = 'z'

size :: Int
size = FS.sizeOf (undefined :: T)

lift :: T# -> T
lift = C#

unlift :: T -> T#
unlift (C# i) = i

index# :: ByteArray# -> Int# -> T#
index# = indexWideCharArray#

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
read# = readWideCharArray#

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
write# = writeWideCharArray#

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
set# marr off len x s = case len of
  0# -> s
  _ -> set# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
shrink# m i s0 = (# Exts.shrinkMutableByteArray# m (i *# 4#) s0, m #)

