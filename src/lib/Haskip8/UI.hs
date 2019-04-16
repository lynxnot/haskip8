module Haskip8.UI
    ( buildUI
    , presentUI
    ) where

import RIO

import           RIO.Vector.Storable as VS
import           Foreign.C.Types (CInt)

import Data.Massiv.Array as A

import           SDL (($=), (*^))
import qualified SDL

import Haskip8.Options
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
           , HasConfig env
           --, HasC8Machine env
           --, HasLogFunc env
           --, HasCallStack
           ) => m SDL.Renderer
buildUI = do
  pixlScale <- scale <$> view configG
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  window   <- SDL.createWindow "Haskip-8" $ windowConfig pixlScale
  SDL.createRenderer window (-1) rendererConfig


-- |
presentUI :: ( MonadReader env m
             , MonadUnliftIO m
             , HasConfig env
             , HasC8Machine env
             --, HasLogFunc env
             --, HasCallStack
             ) => SDL.Renderer -> m ()
presentUI r = do
  pixlScale <- scale       <$> view configG
  fb        <- frameBuffer <$> view c8machineG
  -- Clear the window
  SDL.rendererDrawColor r $= bgColor
  SDL.clear r
  -- Draw `pixuls`
  SDL.rendererDrawColor r $= fgColor
  SDL.fillRects r $ fb2Rectangles pixlScale fb
  -- Present to screen
  SDL.present r
  --return ()


-- |
fb2Rectangles :: CInt -> C8FrameBuffer -> VS.Vector (SDL.Rectangle CInt)
fb2Rectangles pixlScale = A.ifoldlS f VS.empty
  where f rects ix pixlState =
          if pixlState then VS.snoc rects (pixl2Rect pixlScale ix)
                       else rects


-- |
pixl2Rect :: CInt -> A.Ix2 -> SDL.Rectangle CInt
pixl2Rect pixlScale (row :. col) = SDL.Rectangle origin sz
  where origin      = SDL.P (pixlScale *^ originBaseV)
        originBaseV = SDL.V2 (fromIntegral col) (fromIntegral row)
        sz          = SDL.V2 pixlScale pixlScale
