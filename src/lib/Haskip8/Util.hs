module Haskip8.Util
    ( mkWord16
    , loByte
    , showC8Long
    , showC8Word
    , nib2Reg
    , nibbles
    , zeroes
    , readMemC8Long
    , unsafeWithMArray
    , storeRegWord
    , storeMemWord
    , c8wordBCD
    ) where

import RIO
import RIO.Partial (toEnum)

import Data.Bits ((.&.), shiftR, shiftL)

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A

import Haskip8.Types
import Text.Printf


mkWord16 :: C8Word -> C8Word -> C8Long
mkWord16 hi lo = shiftL (fromIntegral hi) 8 + fromIntegral lo


loByte :: C8Long -> C8Word
loByte w16 = fromIntegral $ w16 .&. 0x00ff


showC8Long :: C8Long -> String
showC8Long = printf "0x%04x"

showC8Word :: C8Word -> String
showC8Word = printf "0x%02x"


nibbles :: C8Long -> (C8Nibl, C8Nibl, C8Nibl, C8Nibl)
nibbles w16 =
  ( c8nibl . fromIntegral $ shiftR w16 12
  , c8nibl . fromIntegral $ shiftR w16  8 .&. 0x0f
  , c8nibl . fromIntegral $ shiftR w16  4 .&. 0x00f
  , c8nibl . fromIntegral $        w16    .&. 0x000f
  )

nib2Reg :: C8Nibl -> C8Reg
nib2Reg n = toEnum $ fromIntegral n


-- |
zeroes :: Int -> [C8Word]
zeroes n = replicate n 0x0


-- |
readMemC8Long :: C8Addr -> C8Memory -> C8Long
readMemC8Long addr mem = mkWord16 hi lo
  where hi = mem A.! ix0
        lo = mem A.! (ix0 + 1)
        ix0 = fromIntegral addr


-- |
unsafeWithMArray :: ( Mutable r ix e, PrimMonad m )
  => Array r ix e
  -> (MArray (PrimState m) r ix e -> m a)
  -> m (Array r ix e)
unsafeWithMArray arr action = do
  marr <- A.unsafeThaw arr
  _    <- action marr
  A.unsafeFreeze (A.getComp arr) marr


-- |
storeRegWord :: PrimMonad m => C8Registers -> C8Reg -> C8Word -> m C8Registers
storeRegWord rs r w =
  unsafeWithMArray rs (\ma -> A.write' ma (fromEnum r) w)


-- |
storeMemWord :: PrimMonad m => C8Memory -> C8Addr -> C8Word -> m C8Memory
storeMemWord mem addr w =
  unsafeWithMArray mem (\ma -> A.write' ma (fromIntegral addr) w)


-- |
c8wordBCD :: C8Word -> [C8Word]
c8wordBCD c8w = [c, d, u]
  where (cd, u) = c8w `quotRem` 10
        ( c, d) =  cd `quotRem` 10
