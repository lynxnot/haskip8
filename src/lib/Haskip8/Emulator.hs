module Haskip8.Emulator
  ( bootC8
  , initC8
  ) where

import RIO
import RIO.Partial (toEnum)

import Data.Bits ((.&.), (.|.), xor, shift, testBit)
import Data.Massiv.Array as A

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
  c8vm    <- view c8machineG
  -- manage events
  SDL.mapEvents eventReact
  -- eventually decrement timers
  decrementTimer (dt c8vm)
  decrementTimer (st c8vm)
  -- run cycle
  -- replicateM_ 5 runC8Cycle -- this breaks stuff !
  runC8Cycle
  -- draw window
  presentUI r
  -- exit or loop
  --logDebug $ "KS: " <> displayShow (keysState c8vm)
  exitNow <- readIORef (shouldExit c8vm)
  unless exitNow (loopC8 r)


decrementTimer :: (PrimMonad m, MonadUnliftIO m) => IORef C8Word -> m ()
decrementTimer tRef = do
  t <- readIORef tRef
  unless (t == 0) (atomicWriteIORef tRef (t - 1))


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


-- |
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
          clearFB (frameBuffer c8vm)
          incrementOne


        -- 00EE - RET
        RET            -> do
          --logDebug "RET"
          sp' <- readIORef (sp c8vm)
          atomicWriteIORef (sp c8vm) (sp' - 1)
          let c8l = c8st A.! fromIntegral (sp' - 1)
          atomicWriteIORef (pc c8vm) (c8addr c8l + 2)
          incrementZero
            where c8st = stack c8vm


        -- 1nnn - JP addr
        JMP  addr      -> do
          atomicWriteIORef (pc c8vm) addr
          incrementZero


        -- 2nnn -> CALL addr
        CALL addr      -> do
          --logDebug $ "CALL " <> displayShow addr
          pc' <- readIORef (pc c8vm)
          sp' <- readIORef (sp c8vm)
          storeStackLong c8st sp' (fromIntegral pc')
          atomicWriteIORef (sp c8vm) (sp' + 1)
          atomicWriteIORef (pc c8vm) addr
          incrementZero
            where c8st = stack c8vm


        -- 3xkk -> SE Vx, byte - BC_test OK
        SEB  r1 word   ->
          if x == word then incrementTwo else incrementOne
            where x = regz' A.! fromEnum r1


        -- 4xkk -> SNE Vx, byte - BC_test OK
        SNEB r1 word   ->
          if x == word then incrementOne else incrementTwo
            where x = regz' A.! fromEnum r1


        -- 5xy0 -> SE Vx, Vy - BC_test OK
        SE   r1 r2     ->
          if x == y then incrementTwo else incrementOne
            where x = regz' A.! fromEnum r1
                  y = regz' A.! fromEnum r2


        -- 6xkk - LD Vx, byte
        LDB  r1 word   -> do
          --logDebug $ "LDB " <> displayShow r1 <> " " <> displayShow word
          storeRegWord regz' r1 word
          incrementOne


        -- 7xkk - ADD Vx, byte - BC_test OK
        ADDB r1 word   -> do
          storeRegWord regz' r1 (x + word)
          incrementOne
            where !x = regz' A.! fromEnum r1


        -- 8xy0 - LD Vx, Vy
        LD   r1 r2     -> do
          --logDebug $ "LD " <> displayShow r1 <> " " <> displayShow y
          storeRegWord regz' r1 y
          incrementOne
            where !y = regz' A.! fromEnum r2


        -- 8xy1 - OR Vx, Vy - BC_test OK
        OR   r1 r2     -> do
          storeRegWord regz' r1 (x .|. y)
          incrementOne
            where !x = regz' A.! fromEnum r1
                  !y = regz' A.! fromEnum r2


        -- 8xy2 - AND Vx, Vy - BC_test OK
        AND  r1 r2     -> do
          storeRegWord regz' r1 (x .&. y)
          incrementOne
            where !x = regz' A.! fromEnum r1
                  !y = regz' A.! fromEnum r2


        -- 8xy3 - XOR Vx, Vy - BC_test OK
        XOR  r1 r2     -> do
          storeRegWord regz' r1 (x `xor` y)
          incrementOne
            where !x = regz' A.! fromEnum r1
                  !y = regz' A.! fromEnum r2


        -- 8xy4 - ADD Vx, Vy
        ADD  r1 r2     -> do
          storeRegWord regz' VF $
                 fromIntegral . fromEnum $ x > maxBound - y
          storeRegWord regz' r1 (x + y)
          incrementOne
            where !x = regz' A.! fromEnum r1
                  !y = regz' A.! fromEnum r2


        -- 8xy5 - SUB Vx, Vy - BC_test OK
        SUB  r1 r2     -> do
          storeRegWord regz' VF $
                 fromIntegral . fromEnum $ x > y
          storeRegWord regz' r1 (x - y)
          incrementOne
            where !x = regz' A.! fromEnum r1
                  !y = regz' A.! fromEnum r2


        -- 8xy6 - SHR Vx {, Vy} - BC_test OK
        SHR  r1 _      -> do
          storeRegWord regz' VF $
                 fromIntegral . fromEnum $ odd x
          storeRegWord regz' r1 $ x `shift` (-1)
          incrementOne
            where !x = regz' A.! fromEnum r1


        -- 8xy7 - SUBN Vx, Vy - BC_test OK
        SUBN r1 r2     -> do
          storeRegWord regz' VF $
                 fromIntegral . fromEnum $ y > x
          storeRegWord regz' r1 (y - x)
          incrementOne
            where !x = regz' A.! fromEnum r1
                  !y = regz' A.! fromEnum r2


        -- 8xyE - SHL Vx {, Vy} - BC_test OK
        SHL  r1 _      -> do
          storeRegWord regz' VF $
                 fromIntegral . fromEnum $ testBit x 7
          storeRegWord regz' r1 $ x `shift` 1
          incrementOne
            where !x = regz' A.! fromEnum r1


        -- 9xy0 - SNE
        SNE  r1 r2     ->
          if x == y then incrementOne else incrementTwo
            where x = regz' A.! fromEnum r1
                  y = regz' A.! fromEnum r2


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
          storeRegWord regz' r1 res
          --logDebug $ "RND: " <> displayShow res
          atomicWriteIORef (rndGen c8vm) prng'
          incrementOne


        -- Dxyn - DRW Vx, Vy, nibble
        DRW  r1 r2 nib -> do
          regI' <- readIORef (regI c8vm)
          let sprite   = A.extract' (fromIntegral regI') offset (memory c8vm)
          let spriteFb = A.fromLists' A.Seq
                           (A.foldrS (\e a -> spriteB2Fb x e : a) [] sprite)
                           :: A.Array A.U Ix2 Bool
          hasCollided <- drawSpriteFb y spriteFb
          storeRegWord regz' VF $ fromIntegral (fromEnum hasCollided)
          incrementOne
            where x = regz' A.! fromEnum r1
                  y = fromIntegral $ regz' A.! fromEnum r2 :: Int
                  offset = fromIntegral nib


        -- 0x9E - SKP Vx
        SKP  r1        -> do
          --logDebug $ "SKP " <> displayShow r1
          if isPressed then incrementTwo else incrementOne
            where !x = regz' A.! fromEnum r1
                  isPressed = kState A.! (toEnum . fromIntegral $ x)
                  kState = keysState c8vm


        -- 0xA1 - SKNP Vx
        SKNP r1        -> do
          --logDebug $ "SKNP " <> displayShow r1
          if isPressed then incrementOne else incrementTwo
            where !x = regz' A.! fromEnum r1
                  isPressed = kState A.! (toEnum . fromIntegral $ x)
                  kState = keysState c8vm


        -- Fx07 - LD Vx, DT
        LDDT r1        -> do
          dt' <- readIORef (dt c8vm)
          storeRegWord regz' r1 dt'
          incrementOne


        -- Fx0A - LD Vx, K
        LDK  r1        -> do
          --logDebug $ "LDK " <> displayShow r1
          c8key <- waitForKeyPress
          storeRegWord regz' r1 $ fromIntegral (fromEnum c8key)
          incrementOne


        -- Fx15 - LD DT, Vx
        STDT r1        -> do
          logDebug $ "STDT " <> displayShow x
          atomicWriteIORef (dt c8vm) x
          incrementOne
            where !x = regz' A.! fromEnum r1


        -- Fx18 - LD ST, Vx
        STST r1        -> do
          logDebug $ "STDT " <> displayShow x
          atomicWriteIORef (st c8vm) x
          incrementOne
            where !x = regz' A.! fromEnum r1


        -- Fx1E - ADD I, Vx - BC_test OK
        ADDI r1        -> do
          regI' <- readIORef (regI c8vm)
          atomicWriteIORef (regI c8vm) (regI' + fromIntegral x)
          incrementOne
            where !x = regz' A.! fromEnum r1


        -- Fx29 -- LD F, Vx
        LDF  r1        -> do
          let !digit = regz' A.! fromEnum r1
          atomicWriteIORef (regI c8vm)
            (addrSpriteStart + 5 * fromIntegral digit)
          incrementOne


        -- Fx33 - LD B, Vx - BC_test OK
        LBCD r1        -> do
          regI' <- readIORef (regI c8vm)
          RIO.mapM_ (uncurry (storeMemWord (memory c8vm)))
                    (RIO.zip [regI' .. ] bcds)
          incrementOne
            where bcds = c8wordBCD (regz' A.! fromEnum r1)


        -- Fx55 - LD [I], Vx - BC_test OK
        STRI r1        -> do
          regI' <- readIORef (regI c8vm)
          let values = A.extract' 0 (fromEnum r1 + 1) regz'
          A.mapM_ (uncurry (storeMemWord c8mem))
                $ A.zip (A.enumFromN A.Seq regI' $ A.size values)
                         values
          incrementOne
            where c8mem = memory c8vm


        -- Fx65 - LD Vx, [I] - BC_test OK
        REDI r1        -> do
          regI' <- readIORef (regI c8vm)
          let values = A.extract' (fromIntegral regI')
                                  (fromEnum r1 + 1)
                                  (memory c8vm)
          A.mapM_ (uncurry (storeRegWord regz'))
                $  A.zip (A.fromList A.Seq [V0 .. r1] :: Array B Ix1 C8Reg)
                   values
          incrementOne
