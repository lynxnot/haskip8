{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Haskip8.Types
  ( RomFile
  , C8App(..)
  , HasConfig(..)
  , HasC8Machine(..)
  , C8Long
  , C8Addr()
  , C8Word
  , C8Nibl()
  , C8Reg(..)
  , C8Key(..)
  , C8FrameBuffer
  , C8KeysState
  , C8Stack
  , C8Memory
  , C8Registers
  , C8Machine(..)
  , c8addr
  , c8nibl
  )
  where

import RIO
import RIO.Process

import Data.Massiv.Array as A

import Data.Bits ((.&.))
--import Data.IORef (IORef)
import Data.Word (Word16, Word8)

import Text.Printf
import System.Random (StdGen)

import Haskip8.Options


type RomFile = [Word8]

--------------------------------------------------------------------------------
-- RIO App stuff
--------------------------------------------------------------------------------
data C8App = C8App
  { appLogFunc   :: !LogFunc
  , appProcCtx   :: !ProcessContext
  , appOptions   :: !EmulOpts
  , c8machine    :: !C8Machine
  }

instance HasLogFunc C8App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext C8App where
  processContextL = lens appProcCtx (\x y -> x { appProcCtx = y })

class HasConfig env where
  configG :: SimpleGetter env EmulOpts
instance HasConfig C8App where
  configG = to appOptions

class HasC8Machine env where
  c8machineG :: SimpleGetter env C8Machine
instance HasC8Machine C8App where
  c8machineG = to c8machine

--------------------------------------------------------------------------------
-- CHIP-8
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | CHIP-8 16 bit values
type C8Long = Word16
-- | CHIP-8 Memory word
type C8Word = Word8

--------------------------------------------------------------------------------
-- | CHIP-8 memory address
--   wraps a Word16 value and constrains it in 12 bits bounds
newtype C8Addr = C8Addr Word16
  deriving (Eq, Ord, Enum, Integral, Real)

c8addr :: Word16 -> C8Addr
c8addr w16 = C8Addr (w16 .&. 0xfff)

instance Bounded C8Addr where
  minBound = 0
  maxBound = 0xfff

instance Num C8Addr where
  (+) (C8Addr w1) (C8Addr w2) = c8addr (w1 + w2)
  (*) (C8Addr w1) (C8Addr w2) = c8addr (w1 * w2)
  (-) (C8Addr w1) (C8Addr w2) = c8addr (w1 - w2)
  abs (C8Addr w16)            = c8addr (abs w16)
  signum (C8Addr w16)         = c8addr (signum w16)
  fromInteger i               = c8addr (fromInteger i)

instance Show C8Addr where
  show (C8Addr w16) = printf "0x%03x" w16


--------------------------------------------------------------------------------
-- | A nibble (4 bits)
newtype C8Nibl = C8Nibl Word8
  deriving (Eq, Ord, Enum, Integral, Real)

c8nibl :: Word8 -> C8Nibl
c8nibl w8 = C8Nibl (w8 .&. 0xf)

instance Bounded C8Nibl where
  minBound = 0
  maxBound = 0xf

instance Num C8Nibl where
  (+) (C8Nibl w1) (C8Nibl w2) = c8nibl (w1 + w2)
  (*) (C8Nibl w1) (C8Nibl w2) = c8nibl (w1 * w2)
  (-) (C8Nibl w1) (C8Nibl w2) = c8nibl (w1 - w2)
  abs (C8Nibl w8)             = c8nibl (abs w8)
  signum (C8Nibl w8)          = c8nibl (signum w8)
  fromInteger i               = c8nibl (fromInteger i)

instance Show C8Nibl where
  show (C8Nibl w8) = printf "0x%01x" w8


--------------------------------------------------------------------------------
-- | CHIP-8 Registers
data C8Reg
  = V0 | V1 | V2 | V3
  | V4 | V5 | V6 | V7
  | V8 | V9 | VA | VB
  | VC | VD | VE | VF
  deriving (Eq, Bounded, Enum, Show)


--------------------------------------------------------------------------------
-- | CHIP-8 Keys
data C8Key
  = K0 | K1 | K2 | K3
  | K4 | K5 | K6 | K7
  | K8 | K9 | KA | KB
  | KC | KD | KE | KF
  deriving (Eq, Bounded, Enum, Show)


--------------------------------------------------------------------------------
-- |
type C8FrameBuffer = A.Array A.S A.Ix2 Bool
type C8KeysState   = A.Array A.S A.Ix1 Bool
type C8Stack       = A.Array A.S A.Ix1 C8Long
type C8Memory      = A.Array A.S A.Ix1 C8Word
type C8Registers   = A.Array A.S A.Ix1 C8Word

--------------------------------------------------------------------------------
-- | CHIP-8 Virtual Machine
data C8Machine = C8Machine
  { frameBuffer :: C8FrameBuffer
  , keysState   :: C8KeysState
  , stack       :: C8Stack
  , memory      :: C8Memory
  , regz        :: C8Registers
  , regI        :: IORef C8Addr
  , pc          :: IORef C8Addr
  , sp          :: IORef C8Word
  , dt          :: IORef C8Word
  , st          :: IORef C8Word
  , rndGen      :: IORef StdGen
  , shouldExit  :: IORef Bool
  }
