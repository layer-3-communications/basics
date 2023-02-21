{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

-- This provides an interface to bytearrays in which every
-- boolean is represented by a single bit.
module Basics.Int1
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
    -- Encoding
  , shows
  ) where

import Prelude hiding (shows)

import Data.Int1 (Int1(I1),Int1#)
import GHC.Exts ((+#),(-#),(*#),(==#),(<#),isTrue#)
import GHC.Exts (Int#,State#,MutableByteArray#,ByteArray#)
import GHC.Exts (RuntimeRep(IntRep))
import GHC.Exts (andI#,orI#,notI#,iShiftL#,iShiftRL#)

import qualified Data.Int1 as I1
import qualified GHC.Exts as Exts
import qualified Prelude

type T = Int1
type T# = Int1#
type R = 'IntRep

def :: T
{-# inline def #-}
def = 0

lift :: T# -> T
{-# inline lift #-}
lift = I1

unlift :: T -> T#
{-# inline unlift #-}
unlift (I1 n) = n

eq# :: T# -> T# -> Int#
{-# inline eq# #-}
eq# = I1.eq#

neq# :: T# -> T# -> Int#
{-# inline neq# #-}
neq# = I1.neq#

splitIndex_ :: Int# -> (# Int#, Int# #)
{-# inline splitIndex_ #-}
splitIndex_ bitIx = (# wordIx, intraWordIx #)
  where
  wordIx = bitIx `iShiftRL#` 6#
  intraWordIx = bitIx `andI#` 0x3F#

index# :: ByteArray# -> Int# -> T#
{-# inline index# #-}
index# arr i =
  let !(# wordIx, intraWordIx #) = splitIndex_ i
      !bitBundle = Exts.indexInt64Array# arr wordIx
      !bit = bitBundle `andI#` (1# `iShiftL#` intraWordIx)
   in I1.unsafeFromInt# (bit `iShiftRL#` intraWordIx)

read# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, T# #)
{-# inline read# #-}
read# arr i st =
  let !(# wordIx, intraWordIx #) = splitIndex_ i
      !(# st', bitBundle #) = Exts.readInt64Array# arr wordIx st
      !bit = bitBundle `andI#` (1# `iShiftL#` intraWordIx)
   in (# st', I1.unsafeFromInt# (bit `iShiftRL#` intraWordIx) #)

write# :: MutableByteArray# s -> Int# -> T# -> State# s -> State# s
{-# inline write# #-}
write# arr i v st =
  let !(# wordIx, intraWordIx #) = splitIndex_ i
      !(# st', bitBundle #) = Exts.readInt64Array# arr wordIx st
      !mask = notI# (1# `iShiftL#` intraWordIx)
      !bitBundle' = (bitBundle `andI#` mask) `orI#` (I1.toInt# v `iShiftL#` intraWordIx)
   in Exts.writeInt64Array# arr wordIx bitBundle' st'

set# :: MutableByteArray# s -> Int# -> Int# -> T# -> State# s -> State# s
{-# inline set# #-}
set# arr off0 len0 v st0 =
    let subOff = off0 `andI#` 7#
      -- set non-byte-aligned, initial bits
        len = min# len0 (8# -# subOff)
        st' = bitLoop off0 len st0
        -- set full bytes
        off' = off0 +# len
        len' = len0 -# len
        st'' = writeBytes off' len' st'
        -- set trailing bits smaller than a byte
        off'' = off' +# ((len' `iShiftRL#` 3#) `iShiftL#` 3#)
        len'' = len' `andI#` 7#
     in bitLoop off'' len'' st''
  where
  -- TODO could split bitLoop into writeBitsUnaligned and writeBitsAligned, which would use masking instead of a loop
  bitLoop _ 0# st = st
  bitLoop off len st =
    let st' = write# arr off v st
     in bitLoop (off +# 1#) (len -# 1#) st'
  writeBytes off len st =
    let !offB = off `iShiftRL#` 3#
        !lenB = len `iShiftRL#` 3#
     in Exts.setByteArray# arr offB lenB vB st
  vB = if I1.isTrue# v then 0xFF# else 0#

shrink# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline shrink# #-}
shrink# marr sz st =
  let !(# wordSz, subWordSz #) = splitIndex_ sz
      !paddedSz = wordSz +# if isTrue# (subWordSz ==# 0#) then 0# else 1#
      !szBytes = paddedSz *# 8#
      !st' = Exts.shrinkMutableByteArray# marr szBytes st
   in (# st', marr #)

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline uninitialized# #-}
uninitialized# sz st =
  let !(# wordSz, subWordSz #) = splitIndex_ sz
      !paddedSz = wordSz +# if isTrue# (subWordSz ==# 0#) then 0# else 1#
      !szBytes = paddedSz *# 8#
   in Exts.newByteArray# szBytes st

initialized# :: Int# -> T# -> State# s -> (# State# s, MutableByteArray# s #)
{-# inline initialized# #-}
initialized# sz v0 st =
  let !(# wordSz, subWordSz #) = splitIndex_ sz
      !paddedSz = wordSz +# if isTrue# (subWordSz ==# 0#) then 0# else 1#
      !szBytes = paddedSz *# 8#
      !(# st', marr #) = Exts.newByteArray# szBytes st
      !v = I1.sextToInt# v0
   in (# Exts.setByteArray# marr 0# szBytes v st', marr #)

copy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
{-# inline copy# #-}
copy# dst 0# src 0# len st =
-- TODO when soff == doff, we can do like set#
-- first align with naiveCopy, then copy by bytes, then copy the traling bits with naiveCopy
-- in fact, this can work even when soff - doff divisible by 8
  let !lenB = len `iShiftRL#` 3#
      !st' = Exts.copyByteArray# src 0# dst 0# lenB st
      !off' = lenB `iShiftL#` 3#
      !len' = len `andI#` 7#
   in naiveCopy# dst off' src off' len' st'
copy# dst doff src soff len st = naiveCopy# dst doff src soff len st

naiveCopy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
-- TODO if I had an index64 :: ByteArray# -> off:Int#  -> len:Int# -> Int#
-- that reads up to `min len 64` unaligned bits starting at off
-- then I could write whole words at a time after aligning the doff, just as in set#
naiveCopy# _ _ _ _ 0# st = st
naiveCopy# dst doff src soff len st =
  let !v = index# src soff
      !st' = write# dst doff v st
   in naiveCopy# dst (doff +# 1#) src (soff +# 1#) (len -# 1#) st'

copyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
{-# inline copyMutable# #-}
copyMutable# dst 0# src 0# len st =
-- TODO when soff == doff, we can do like set#
-- first align with naiveCopyMutable, then copy by bytes, then copy the traling bits with naiveCopyMutable
-- in fact, this can work even when soff - doff divisible by 8
  let !lenB = len `iShiftRL#` 3#
      !st' = Exts.copyMutableByteArray# src 0# dst 0# lenB st
      !off' = lenB `iShiftL#` 3#
      !len' = len `andI#` 7#
   in naiveCopyMutable# dst off' src off' len' st'
copyMutable# dst doff src soff len st = naiveCopyMutable# dst doff src soff len st

naiveCopyMutable# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
-- TODO if I had an index64 :: ByteArray# -> off:Int#  -> len:Int# -> Int#
-- that reads up to `min len 64` unaligned bits starting at off
-- then I could write whole words at a time after aligning the doff, just as in set#
naiveCopyMutable# _ _ _ _ 0# st = st
naiveCopyMutable# dst doff src soff len st =
  let !(# st', v #) = read# src soff st
      !st'' = write# dst doff v st'
   in naiveCopyMutable# dst (doff +# 1#) src (soff +# 1#) (len -# 1#) st''

shows :: T -> String -> String
{-# inline shows #-}
shows = Prelude.shows

min# :: Int# -> Int# -> Int#
{-# inline min# #-}
min# a b = if isTrue# (a <# b) then a else b