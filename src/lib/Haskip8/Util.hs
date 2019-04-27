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
    , storeKeyState
    , storeRegWord
    , storeMemWord
    , c8wordBCD
    , spriteB2Fb
    , drawSpriteFb
    , clearFB
    ) where

import RIO
import RIO.Partial (toEnum)

import Data.BitVector as BV
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
zeroes n = RIO.replicate n 0x0


-- |
c8wordBCD :: C8Word -> [C8Word]
c8wordBCD c8w = [c, d, u]
  where (cd, u) = c8w `quotRem` 10
        ( c, d) =  cd `quotRem` 10


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
storeKeyState :: PrimMonad m => C8KeysState -> C8Key -> Bool -> m ()
storeKeyState kState k state = do
  _ <- unsafeWithMArray kState (\ma -> A.write' ma (fromEnum k) state)
  return ()


-- |
storeRegWord :: PrimMonad m => C8Registers -> C8Reg -> C8Word -> m C8Registers
storeRegWord rs r w =
  unsafeWithMArray rs (\ma -> A.write' ma (fromEnum r) w)


-- |
storeMemWord :: PrimMonad m => C8Memory -> C8Addr -> C8Word -> m C8Memory
storeMemWord mem addr w =
  unsafeWithMArray mem (\ma -> A.write' ma (fromIntegral addr) w)


-- |
updateFbBit :: PrimMonad m => C8FrameBuffer -> (Ix2, Bool) -> m C8FrameBuffer
updateFbBit fb (ix, e) =
  unsafeWithMArray fb (\ma -> A.write' ma ix e)


-- |
clearFB :: PrimMonad m => C8FrameBuffer -> m ()
clearFB c8fb =
  A.imapM_ (curry (updateFbBit c8fb))
           (A.makeArrayR A.U A.Seq (32 :. 64) (const False))


-- |
spriteB2Fb :: C8Word -> C8Word -> [Bool]
spriteB2Fb xPos c8w =
  toBits $ (bitVec 64 c8w <<. 56) >>>. fromIntegral xPos


-- | return the pair:
--    -> (collision?, xor a b)
cxor :: Bool -> Bool -> (Bool, Bool)
cxor True True   = (True, False)
cxor False False = (False, False)
cxor _ _         = (False, True)


-- |
mergeSpriteFb :: C8FrameBuffer -> Int -> Ix2 -> Bool -> (Bool, (Ix2, Bool))
mergeSpriteFb c8fb yPos (y :. x) e = (hasCollided, (ix', e'))
  where (hasCollided, e') = cxor current e
        current = c8fb A.! ix'
        ix' = yPos + y :. x


-- |
drawSpriteFb :: ( PrimMonad m
                , MonadReader env m
                , HasC8Machine env
                ) => Int -> A.Array A.U Ix2 Bool -> m Bool
drawSpriteFb yPos spriteFb = do
  c8fb <- frameBuffer <$> view c8machineG
  let (colls, newFb) = A.unzip $ A.imap (mergeSpriteFb c8fb yPos) spriteFb
  A.mapM_ (updateFbBit c8fb) newFb
  return (A.any id colls)
