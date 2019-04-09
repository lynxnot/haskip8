module Haskip8.Options
    ( getOpts
    , RomFilePath
    , Opts(..)
    , C8Command(..)
    , EmulOpts(..)
    ) where


import RIO

import Options.Applicative
import Data.Semigroup ((<>))

import Foreign.C.Types (CInt)


type RomFilePath = FilePath

newtype Opts = Opts { optCommand :: C8Command }

data C8Command = Disasm RomFilePath
               | Emulate EmulOpts RomFilePath

data EmulOpts =
  EmulOpts
  { scale   :: CInt
  , cycles  :: Int
  }


getOpts :: IO Opts
getOpts = customExecParser
            (prefs (showHelpOnEmpty <> showHelpOnError))
            optsParser


optsParser :: ParserInfo Opts
optsParser = info
                (  programOptions <**> helper )
                (  fullDesc
                <> progDesc "prog desc"
                <> header "haskip8 - haskell CHIP-8 emulator" )


programOptions :: Parser Opts
programOptions = Opts <$> hsubparser (disasmCommand <> runCommand)


disasmCommand :: Mod CommandFields C8Command
disasmCommand =
  command "dasm"
    (info disasmOptions (progDesc "disassemble rom file"))


disasmOptions :: Parser C8Command
disasmOptions = Disasm <$> romFileOpts


runCommand :: Mod CommandFields C8Command
runCommand =
  command "emul"
    (info runOptions (progDesc "run the emulator"))


runOptions :: Parser C8Command
runOptions = Emulate <$> emulOpts <*> romFileOpts


romFileOpts :: Parser RomFilePath
romFileOpts = argument str ( help "rom file path" <> metavar "FILE" )


emulOpts :: Parser EmulOpts
emulOpts =
  EmulOpts
    <$> option auto
      (  short 's'
      <> long "scale"
      <> help "Pixl scale in pixels :)"
      <> showDefault
      <> value 16
      <> metavar "INT" )
    <*> option auto
      (  short 'c'
      <> long "cycles"
      <> help "Number of cycles to run every frame"
      <> showDefault
      <> value 100
      <> metavar "INT" )
