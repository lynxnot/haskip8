module Haskip8.Emulator
  ( bootC8
  , initC8
  ) where

import RIO

import Data.Bits ((.&.))
import Data.Massiv.Array as A

import GHC.Enum (enumFrom)

import System.Random

import qualified SDL (Renderer, mapEvents)

import Haskip8.Events
import Haskip8.Instructions
--import Haskip8.Options
import Haskip8.Types
import Haskip8.UI
import Haskip8.Util

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
    { frameBuffer = A.makeArrayR A.S A.Seq (A.Sz 32 :. 64) (const False)
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


-- |
initMemory :: RomFile -> C8Memory
initMemory rom =
  A.fromList A.Seq $ concat
    [ zeroes (fromIntegral addrSpriteStart)
    , sprites
    , zeroes (fromIntegral (addrRomStart - addrSpriteStart) - length sprites)
    , rom
    , zeroes (1 + fromIntegral (maxBound - addrRomStart) - length rom)
    ]


-- |
initC8 :: ( PrimMonad m, MonadReader env m
          , MonadUnliftIO m
          , HasConfig env
          , HasC8Machine env
          , HasLogFunc env
          , HasCallStack
          ) => m ()
initC8 = do
  logInfo "Starting CHIP-8 emulator"
  r    <- buildUI
  loopC8 r


-- |
loopC8 :: ( PrimMonad m, MonadReader env m
          , MonadUnliftIO m
          , HasConfig env
          , HasC8Machine env
          , HasLogFunc env
          , HasCallStack
          ) => SDL.Renderer -> m ()
loopC8 r = do
  -- manage events
  SDL.mapEvents eventReact
  -- run cycle
  runC8Cycle
  -- draw window
  presentUI r
  -- exit or loop
  c8vm    <- view c8machineG
  --logDebug $ "KS: " <> displayShow (keysState c8vm)
  exitNow <- readIORef (shouldExit c8vm)
  unless exitNow (loopC8 r)


-- |
runC8Cycle :: ( PrimMonad m
              , MonadReader env m
              , MonadUnliftIO m
              --, HasConfig env
              , HasC8Machine env
              --, HasLogFunc env
              --, HasCallStack
              ) => m ()
runC8Cycle = do
  c8vm <- view c8machineG
  pc'  <- readIORef $ pc c8vm
  let op = parseOp $ readMemC8Long pc' (memory c8vm)
  runOp op
  --return ()



-- | Source of Thruth: http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
--
runOp :: ( PrimMonad m
         , MonadReader env m
         , MonadUnliftIO m
         , HasC8Machine env
         --, HasLogFunc env
         --, HasCallStack
         ) => Maybe C8Instruction -> m ()
runOp mop = do
  c8vm  <- view c8machineG
  let regz' = regz c8vm
  case mop of
    Nothing -> undefined -- should do a bluescreen
    Just op ->
      case op of
        CLS            -> undefined
        RET            -> undefined
        SYS  addr      -> undefined
        JMP  addr      -> undefined
        CALL addr      -> undefined
        SEB  r1 word   -> undefined
        SNEB r1 word   -> undefined
        SE   r1 r2     -> undefined
        LDB  r1 word   -> undefined
        ADDB r1 word   -> undefined
        LD   r1 r2     -> undefined
        OR   r1 r2     -> undefined
        AND  r1 r2     -> undefined
        XOR  r1 r2     -> undefined
        ADD  r1 r2     -> undefined
        SUB  r1 r2     -> undefined
        SHR  r1 r2     -> undefined
        SUBN r1 r2     -> undefined
        SHL  r1 r2     -> undefined
        SNE  r1 r2     -> undefined

        -- Annn - LD I, addr
        LDI  addr      -> atomicWriteIORef (regI c8vm) addr

        -- Bnnn - JP V0, addr
        JMP0 addr      -> do
          let newAddr = addr + fromIntegral (regz' A.! 0)
          atomicWriteIORef (pc c8vm) newAddr

        -- Cxkk - RND Vx, byte
        RND  r1 word   -> do
          prng <- readIORef (rndGen c8vm)
          let (rnd, prng') = randomR (0, 0xff) prng
          _ <- storeRegWord regz' r1 (rnd .&. word)
          atomicWriteIORef (rndGen c8vm) prng'

        --
        DRW  r1 r2 nib -> undefined
        SKP  r1        -> undefined
        SKNP r1        -> undefined
        LDDT r1        -> undefined
        LDK  r1        -> undefined
        STDT r1        -> undefined
        STST r1        -> undefined
        ADDI r1        -> undefined
        LDF  r1        -> undefined

        -- Fx33
        LBCD r1        -> do
          regI' <- readIORef (regI c8vm)
          let bcds = c8wordBCD (regz' A.! fromEnum r1)
          _ <- RIO.mapM (uncurry (storeMemWord (memory c8vm)))
                      $  RIO.zip [regI' .. ] bcds
          return ()

        -- Fx55 - LD [I], Vx
        STRI r1        -> undefined

        -- Fx65 - LD Vx, [I]
        REDI r1        -> do
          regI' <- readIORef (regI c8vm)
          let values = A.toList $ A.extract' (fromIntegral regI')
                                             (fromEnum r1 + 1)
                                             (memory c8vm)
          _ <- RIO.mapM (uncurry (storeRegWord regz'))
                      $  RIO.zip (enumFrom V0) values
          return ()
