module Haskip8.Instructions
    ( C8Instruction(..)
    , parseOps
    , parseOp
    ) where

import RIO

import Haskip8.Types
import Haskip8.Util


-- source: http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
data C8Instruction
  = CLS                      -- 0x 00 E0
  | RET                      -- 0x 00 EE
  | JMP  C8Addr              -- 0x 1n nn
  | CALL C8Addr              -- 0x 2n nn
  | SEB  C8Reg C8Word        -- 0x 3x vv
  | SNEB C8Reg C8Word        -- 0x 4x vv
  | SE   C8Reg C8Reg         -- 0x 5x y0
  | LDB  C8Reg C8Word        -- 0x 6x vv
  | ADDB C8Reg C8Word        -- 0x 7x vv
  | LD   C8Reg C8Reg         -- 0x 8x y0
  | OR   C8Reg C8Reg         -- 0x 8x y1
  | AND  C8Reg C8Reg         -- 0x 8x y2
  | XOR  C8Reg C8Reg         -- 0x 8x y3
  | ADD  C8Reg C8Reg         -- 0x 8x y4
  | SUB  C8Reg C8Reg         -- 0x 8x y5
  | SHR  C8Reg C8Reg         -- 0x 8x y6
  | SUBN C8Reg C8Reg         -- 0x 8x y7
  | SHL  C8Reg C8Reg         -- 0x 8x yE
  | SNE  C8Reg C8Reg         -- 0x 9x y0
  | LDI  C8Addr              -- 0x An nn
  | JMP0 C8Addr              -- 0x Bn nn
  | RND  C8Reg C8Word        -- 0x Cx vv
  | DRW  C8Reg C8Reg C8Nibl  -- 0x Dx yn
  | SKP  C8Reg               -- 0x Ex 9E
  | SKNP C8Reg               -- 0x Ex A1
  | LDDT C8Reg               -- 0x Fx 07
  | LDK  C8Reg               -- 0x Fx 0A
  | STDT C8Reg               -- 0x Fx 15
  | STST C8Reg               -- 0x Fx 18
  | ADDI C8Reg               -- 0x Fx 1E
  | LDF  C8Reg               -- 0x Fx 29
  | LBCD C8Reg               -- 0x Fx 33
  | STRI C8Reg               -- 0x Fx 55
  | REDI C8Reg               -- 0x Fx 65
  deriving (Show)


parseOps :: [C8Long] -> Maybe [C8Instruction]
parseOps = mapM parseOp


parseOp :: C8Long -> Maybe C8Instruction
parseOp op
  = case nibbles op of
      (0x0, 0x0, 0xE, 0x0) -> Just CLS
      (0x0, 0x0, 0xE, 0xE) -> Just RET
      (0x1,   _,   _,   _) -> Just $ JMP  (c8addr op)
      (0x2,   _,   _,   _) -> Just $ CALL (c8addr op)
      (0x3,  vX,   _,   _) -> Just $ SEB  (nib2Reg vX) (loByte op)
      (0x4,  vX,   _,   _) -> Just $ SNEB (nib2Reg vX) (loByte op)
      (0x5,  vX,  vY, 0x0) -> Just $ SE   (nib2Reg vX) (nib2Reg vY)
      (0x6,  vX,   _,   _) -> Just $ LDB  (nib2Reg vX) (loByte op)
      (0x7,  vX,   _,   _) -> Just $ ADDB (nib2Reg vX) (loByte op)
      (0x8,  vX,  vY, 0x0) -> Just $ LD   (nib2Reg vX) (nib2Reg vY)
      (0x8,  vX,  vY, 0x1) -> Just $ OR   (nib2Reg vX) (nib2Reg vY)
      (0x8,  vX,  vY, 0x2) -> Just $ AND  (nib2Reg vX) (nib2Reg vY)
      (0x8,  vX,  vY, 0x3) -> Just $ XOR  (nib2Reg vX) (nib2Reg vY)
      (0x8,  vX,  vY, 0x4) -> Just $ ADD  (nib2Reg vX) (nib2Reg vY)
      (0x8,  vX,  vY, 0x5) -> Just $ SUB  (nib2Reg vX) (nib2Reg vY)
      (0x8,  vX,  vY, 0x6) -> Just $ SHR  (nib2Reg vX) (nib2Reg vY)
      (0x8,  vX,  vY, 0x7) -> Just $ SUBN (nib2Reg vX) (nib2Reg vY)
      (0x8,  vX,  vY, 0xE) -> Just $ SHL  (nib2Reg vX) (nib2Reg vY)
      (0x9,  vX,  vY, 0x0) -> Just $ SNE  (nib2Reg vX) (nib2Reg vY)
      (0xA,   _,   _,   _) -> Just $ LDI  (c8addr op)
      (0xB,   _,   _,   _) -> Just $ JMP0 (c8addr op)
      (0xC,  vX,   _,   _) -> Just $ RND  (nib2Reg vX) (loByte op)
      (0xD,  vX,  vY, nib) -> Just $ DRW  (nib2Reg vX) (nib2Reg vY) nib
      (0xE,  vX, 0x9, 0xE) -> Just $ SKP  (nib2Reg vX)
      (0xE,  vX, 0xA, 0x1) -> Just $ SKNP (nib2Reg vX)
      (0xF,  vX, 0x0, 0x7) -> Just $ LDDT (nib2Reg vX)
      (0xF,  vX, 0x0, 0xA) -> Just $ LDK  (nib2Reg vX)
      (0xF,  vX, 0x1, 0x5) -> Just $ STDT (nib2Reg vX)
      (0xF,  vX, 0x1, 0x8) -> Just $ STST (nib2Reg vX)
      (0xF,  vX, 0x1, 0xE) -> Just $ ADDI (nib2Reg vX)
      (0xF,  vX, 0x2, 0x9) -> Just $ LDF  (nib2Reg vX)
      (0xF,  vX, 0x3, 0x3) -> Just $ LBCD (nib2Reg vX)
      (0xF,  vX, 0x5, 0x5) -> Just $ STRI (nib2Reg vX)
      (0xF,  vX, 0x6, 0x5) -> Just $ REDI (nib2Reg vX)
      (_  ,   _,   _,   _) -> Nothing
