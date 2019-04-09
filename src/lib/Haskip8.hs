module Haskip8
    ( runHaskip8
    ) where


import           RIO
import qualified RIO.ByteString  as B (readFile, unpack)
import           RIO.Process

import System.IO (putStrLn)

import Haskip8.Instructions (parseOps)
import Haskip8.Emulator
import Haskip8.Options
import Haskip8.Types
import Haskip8.Util


runHaskip8 :: IO ()
runHaskip8 = do
  opts <- getOpts
  case optCommand opts of
    Disasm           romPath -> disasmC8  romPath
    Emulate emulOpts romPath -> emulateC8 romPath emulOpts


disasmC8 :: RomFilePath -> IO ()
disasmC8 romPath = do
  putStrLn $ "disasm " <> romPath
  putStrLn $ replicate 35 '*'
  rom <- parseOps . toW16 <$> unpackRom romPath
  case rom of
    Just r  -> putStrLn $ unlines $ RIO.map show r
    Nothing -> putStrLn "failed to disasm rom file"


emulateC8 :: RomFilePath -> EmulOpts -> IO ()
emulateC8 romPath emulOpts = do
  rom  <- unpackRom romPath
  c8vm <- bootC8 rom
  lo   <- logOptionsHandle stderr True
  pctx <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = C8App
              { appLogFunc = lf
              , appProcCtx = pctx
              , appOptions = emulOpts
              , c8machine  = c8vm
              }
    in runRIO app emulC8


unpackRom :: RomFilePath -> IO RomFile
unpackRom fp = B.unpack <$> B.readFile fp


-- | This is a pretty optimistic approach, that completly ignores
--   the possibility a ROM could contains sprites
toW16 :: RomFile -> [Word16]
toW16 xs = toW16' xs []

toW16' :: RomFile -> [Word16] -> [Word16]
toW16' [] ys = reverse ys
toW16' (x0:x1:xs) ys = toW16' xs (mkWord16 x0  x1 : ys)
toW16' [_] _ = error "odd number of elements"
