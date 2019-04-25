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
    { frameBuffer = A.makeArrayR A.S A.Seq (32 :. 64) (const False)
    , keysState   = A.makeArrayR A.S A.Seq 16 (const False)
    , stack       = A.makeArrayR A.S A.Seq 16 (const 0)
    , memory      = initMemory rom
    , regz        = A.makeArrayR A.S A.Seq 16 (const 0)
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
              , HasC8Machine env
              , HasLogFunc env
              , HasCallStack
              ) => m ()
runC8Cycle = do
  c8vm <- view c8machineG
  pc'  <- readIORef $ pc c8vm
  let op = parseOp $ readMemC8Long pc' (memory c8vm)
  mInc <- runOp op
  case mInc of
    Just i -> atomicWriteIORef (pc c8vm) (pc' + fromIntegral i)
    _      -> return ()


incrementZero, incrementOne, incrementTwo :: PrimMonad m => m (Maybe Int)
incrementZero = return Nothing
incrementOne  = return $ Just 2
incrementTwo  = return $ Just 4


-- | Source of Thruth: http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
--
runOp :: ( PrimMonad m
         , MonadReader env m
         , MonadUnliftIO m
         , HasC8Machine env
         , HasLogFunc env
         , HasCallStack
         ) => Maybe C8Instruction -> m (Maybe Int)
runOp mop = do
  c8vm  <- view c8machineG
  let regz' = regz c8vm
  case mop of
    Nothing -> undefined -- should do a bluescreen
    Just op ->
      case op of

        -- 00E0 - CLS
        CLS            -> do
          let _ = c8vm { frameBuffer = A.computeAs A.S $
                              A.map (const False) (frameBuffer c8vm) }
          incrementOne

        -- 00E0 - RET
        RET            -> undefined
        --SYS  addr      -> undefined

        -- 1nnn - JP addr
        JMP  addr      -> do
          atomicWriteIORef (pc c8vm) addr
          incrementZero

        --
        CALL addr      -> undefined
        SEB  r1 word   -> undefined
        SNEB r1 word   -> undefined
        SE   r1 r2     -> undefined


        -- 6xkk - LD Vx, byte
        LDB  r1 word   -> do
          _ <- storeRegWord regz' r1 word
          incrementOne

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
        LDI  addr      -> do
          atomicWriteIORef (regI c8vm) addr
          incrementOne


        -- Bnnn - JP V0, addr
        JMP0 addr      -> do
          let newAddr = addr + fromIntegral (regz' A.! 0)
          atomicWriteIORef (pc c8vm) newAddr
          incrementZero


        -- Cxkk - RND Vx, byte
        RND  r1 word   -> do
          prng <- readIORef (rndGen c8vm)
          let (rnd, prng') = randomR (0, 0xff) prng
          let !res = rnd .&. word
          _ <- storeRegWord regz' r1 res
          logDebug $ "RND: " <> displayShow res
          atomicWriteIORef (rndGen c8vm) prng'
          incrementOne


        -- Dxyn - DRW Vx, Vy, nibble
        DRW  r1 r2 nib -> do
          let x = regz' A.! fromEnum r1
          let y = fromIntegral $ regz' A.! fromEnum r2 :: Int
          let offset = fromIntegral nib
          let rowIxs = take offset $ (`mod` 32) <$> enumFrom y
          regI' <- readIORef (regI c8vm)
          let sprite = A.toList (A.extract' (fromIntegral regI')
                                   offset
                                   (memory c8vm))
          let spriteRows = sprite2Row x <$> sprite
          collided <- drawRows rowIxs spriteRows
          _ <- if collided then storeRegWord regz' VF 1
                           else storeRegWord regz' VF 0
          incrementOne


        --
        SKP  r1        -> undefined
        SKNP r1        -> undefined
        LDDT r1        -> undefined

        --
        LDK  r1        -> incrementZero
        --LDK  r1        -> incrementOne

        STDT r1        -> undefined
        STST r1        -> undefined
        ADDI r1        -> undefined


        -- Fx29 -- LD F, Vx
        LDF  r1        -> do
          let !digit = regz' A.! fromEnum r1
          atomicWriteIORef (regI c8vm)
            (addrSpriteStart + 5 * fromIntegral digit)
          incrementOne


        -- Fx33
        LBCD r1        -> do
          regI' <- readIORef (regI c8vm)
          let bcds = c8wordBCD (regz' A.! fromEnum r1)
          let addrBCDs = RIO.zip [regI' .. ] bcds
          _ <- RIO.mapM (uncurry (storeMemWord (memory c8vm)))
                        addrBCDs
          incrementOne


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
          incrementOne
