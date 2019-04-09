module Haskip8.UI
    ( buildUI
    ) where

import RIO

import           Foreign.C.Types (CInt)

import           SDL (($=))
import qualified SDL

import Haskip8.Types


baseWidth, baseHeight :: CInt
baseWidth  = 64
baseHeight = 32

-- |
bgColor, fgColor :: SDL.V4 Word8
bgColor = SDL.V4 0x28 0x28 0x28 0xff
fgColor = SDL.V4 0x83 0xa5 0x98 0xff


-- |
windowConfig :: CInt -> SDL.WindowConfig
windowConfig pixlScale = SDL.defaultWindow
  { SDL.windowInitialSize = SDL.V2 (baseWidth * pixlScale)
                                   (baseHeight * pixlScale)
  }

-- |
rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.defaultRenderer
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  }


-- |
buildUI :: ( MonadReader env m
           , MonadUnliftIO m
           --, HasConfig env
           --, HasC8Machine env
           --, HasLogFunc env
           --, HasCallStack
           ) => CInt -> m SDL.Renderer
buildUI scale = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  window   <- SDL.createWindow "Haskip-8" $ windowConfig scale
  SDL.createRenderer window (-1) rendererConfig
