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
    , sprite2Row
    , drawRows
    ) where

import RIO
import RIO.Partial (toEnum)

import Data.BitVector as BV
import Data.Bits ((.&.), shiftR, shiftL)
import Data.List as L (head, last, unzip)

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
c8wordBCD :: C8Word -> [C8Word]
c8wordBCD c8w = [c, d, u]
  where (cd, u) = c8w `quotRem` 10
        ( c, d) =  cd `quotRem` 10


-- |
sprite2Row :: C8Word -> C8Word -> [Bool]
sprite2Row pos sprite =
  toBits $ (bitVec 64 sprite <<. 56) >>>. fromIntegral pos


-- | return the pair:
--    -> (collision?, xor a b)
cxor :: (Bool, Bool) -> (Bool, Bool)
cxor (True,  True)  = (True, False)
cxor (False, False) = (False, False)
cxor (_,     _)     = (False, True)


-- |
drawRow :: [Bool] -> [Bool] -> (Bool, [Bool])
drawRow r1 r2 = (collided, pixls)
  where (colls, pixls) = L.unzip $ RIO.map cxor $ RIO.zip r1 r2
        collided = RIO.any id colls


-- |
drawRows :: ( PrimMonad m
            , MonadReader env m
            , HasC8Machine env
            ) => [Int] -> [[Bool]] -> m Bool
drawRows rowsIx rows = do
  c8fb <- frameBuffer <$> view c8machineG
  let actualRows = A.toLists
                      $ A.extractFromTo' (L.head rowsIx     :. 0)
                                         (L.last rowsIx + 1 :. 64) c8fb
  let (colls, newRows) = L.unzip $ uncurry drawRow <$> RIO.zip actualRows rows
  let newIxs = (\row -> (row :.) <$> take 64 [0..]) <$> rowsIx
  let !newIxsPixls = RIO.zip (RIO.concat newIxs) (RIO.concat newRows)
  _ <- RIO.mapM (updateFbBit c8fb) newIxsPixls
  return (RIO.any id colls)
