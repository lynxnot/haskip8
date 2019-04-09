module Haskip8.Util
    ( mkWord16
    , loByte
    , showC8Long
    , showC8Word
    , nib2Reg
    , nibbles
    ) where

import RIO
import RIO.Partial (toEnum)

import Data.Bits ((.&.), shiftR, shiftL)

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
