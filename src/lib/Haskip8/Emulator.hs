module Haskip8.Emulator
  ( bootC8
  , emulC8
  ) where

import RIO

import qualified Data.Massiv.Array as A

import System.Random

import qualified SDL (Renderer, mapEvents)

import Haskip8.Events
import Haskip8.Options
import Haskip8.Types
import Haskip8.UI

--------------------------------------------------------------------------------
-- | CHIP-8 sprites
sprites :: [C8Word]
sprites =
  [ 0xF0, 0x90, 0x90, 0x90, 0xF0 -- 0
  , 0x20, 0x60, 0x20, 0x20, 0x70 -- 1
  , 0xF0, 0x10, 0xF0, 0x80, 0xF0 -- 2
  , 0xF0, 0x10, 0xF0, 0x10, 0xF0 -- 3
  , 0x90, 0x90, 0xF0, 0x10, 0x10 -- 4
  , 0xF0, 0x80, 0xF0, 0x10, 0xF0 -- 5
  , 0xF0, 0x80, 0xF0, 0x90, 0xF0 -- 6
  , 0xF0, 0x10, 0x20, 0x40, 0x40 -- 7
  , 0xF0, 0x90, 0xF0, 0x90, 0xF0 -- 8
  , 0xF0, 0x90, 0xF0, 0x10, 0xF0 -- 9
  , 0xF0, 0x90, 0xF0, 0x90, 0x90 -- A
  , 0xE0, 0x90, 0xE0, 0x90, 0xE0 -- B
  , 0xF0, 0x80, 0x80, 0x80, 0xF0 -- C
  , 0xE0, 0x90, 0x90, 0x90, 0xE0 -- D
  , 0xF0, 0x80, 0xF0, 0x80, 0xF0 -- E
  , 0xF0, 0x80, 0xF0, 0x80, 0x80 -- F
  ]


-- Notable Adresses
addrRomStart, addrSpriteStart :: C8Addr
addrSpriteStart = c8addr 0x1a0
addrRomStart    = c8addr 0x200


-- | Initialize CHIP-8 virtual machine
bootC8 :: RomFile -> IO C8Machine
bootC8 rom = do
  regI'       <- newIORef 0x0
  pc'         <- newIORef addrRomStart
  sp'         <- newIORef 0x0
  dt'         <- newIORef 0x0
  st'         <- newIORef 0x0
  shouldExit' <- newIORef False
  stdGen      <- getStdGen
  let seed    =  fst (randomR (0, 0xff) stdGen)
  pseudoRng   <- newIORef $ mkStdGen seed
  return C8Machine
    { frameBuffer = A.makeArrayR A.S A.Seq (A.Sz 32 A.:. 64) (const False)
    , keysState   = A.makeArrayR A.S A.Seq (A.Sz 16) (const False)
    , stack       = A.makeArrayR A.S A.Seq (A.Sz 16) (const 0)
    , memory      = initMemory rom
    , regz        = A.makeArrayR A.S A.Seq (A.Sz 16) (const 0)
    , regI        = regI'
    , pc          = pc'
    , sp          = sp'
    , dt          = dt'
    , st          = st'
    , rndGen      = pseudoRng
    , shouldExit  = shouldExit'
    }


initMemory :: RomFile -> A.Array A.S A.Ix1 C8Word
initMemory rom =
  A.fromList A.Seq $ concat
    [ zeroes (fromIntegral addrSpriteStart)
    , sprites
    , zeroes (fromIntegral (addrRomStart - addrSpriteStart) - length sprites)
    , rom
    , zeroes (1 + fromIntegral (maxBound - addrRomStart) - length rom)
    ]


zeroes :: Int -> [C8Word]
zeroes n = replicate n 0x0


emulC8 :: ( PrimMonad m,MonadReader env m
          , MonadUnliftIO m
          , HasConfig env
          , HasC8Machine env
          , HasLogFunc env
          , HasCallStack
          ) => m ()
emulC8 = do
  logInfo "Starting CHIP-8 emulator"
  opts <- view configG
  r    <- buildUI (scale opts)
  loopC8 r



loopC8 :: ( PrimMonad m,MonadReader env m
          , MonadUnliftIO m
          , HasConfig env
          , HasC8Machine env
          , HasLogFunc env
          , HasCallStack
          ) => SDL.Renderer -> m ()
loopC8 r = do
  -- manage events
  SDL.mapEvents eventReact



  -- exit or loop
  c8vm    <- view c8machineG
  --logDebug $ "KS: " <> displayShow (keysState c8vm)

  exitNow <- readIORef (shouldExit c8vm)
  unless exitNow (loopC8 r)
