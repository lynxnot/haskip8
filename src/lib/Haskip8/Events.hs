module Haskip8.Events
    ( eventReact
    ) where

import RIO

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A

import SDL

import Haskip8.Types



quitApp :: ( MonadReader env m
           , MonadUnliftIO m
           , HasC8Machine env
           --, HasLogFunc env
           --, HasCallStack
           ) => m ()
quitApp = do
  c8vm <- view c8machineG
  atomicWriteIORef (shouldExit c8vm) True


eventReact :: ( PrimMonad m,MonadReader env m
              , MonadUnliftIO m
              , HasC8Machine env
              --, HasLogFunc env
              --, HasCallStack
              ) => Event -> m ()
eventReact ev =
  case eventPayload ev of
    QuitEvent -> quitApp
    KeyboardEvent kbdEvent ->
      updateKeyState
        (keyboardEventKeysym kbdEvent)
        (keyboardEventKeyMotion kbdEvent == Pressed)
    _ -> return ()


updateKeyState :: ( PrimMonad m,MonadReader env m
                  , MonadUnliftIO m
                  , HasC8Machine env
                  --, HasLogFunc env
                  --, HasCallStack
                  ) => Keysym -> Bool -> m ()
updateKeyState ksym state =
  case keysymKeycode ksym of
    KeycodeQ          -> quitApp
    KeycodeKP0        -> doUpdate K0 state
    KeycodeKP1        -> doUpdate K1 state
    KeycodeKP2        -> doUpdate K2 state
    KeycodeKP3        -> doUpdate K3 state
    KeycodeKP4        -> doUpdate K4 state
    KeycodeKP5        -> doUpdate K5 state
    KeycodeKP6        -> doUpdate K6 state
    KeycodeKP7        -> doUpdate K7 state
    KeycodeKP8        -> doUpdate K8 state
    KeycodeKP9        -> doUpdate K9 state
    KeycodeKPPeriod   -> doUpdate KA state
    KeycodeKPEnter    -> doUpdate KB state
    KeycodeKPPlus     -> doUpdate KC state
    KeycodeKPMinus    -> doUpdate KD state
    KeycodeKPMultiply -> doUpdate KE state
    KeycodeKPDivide   -> doUpdate KF state
    Keycode0          -> doUpdate K0 state
    Keycode1          -> doUpdate K1 state
    Keycode2          -> doUpdate K2 state
    Keycode3          -> doUpdate K3 state
    Keycode4          -> doUpdate K4 state
    Keycode5          -> doUpdate K5 state
    Keycode6          -> doUpdate K6 state
    Keycode7          -> doUpdate K7 state
    Keycode8          -> doUpdate K8 state
    Keycode9          -> doUpdate K9 state
    KeycodeA          -> doUpdate KA state
    KeycodeS          -> doUpdate KB state
    KeycodeD          -> doUpdate KC state
    KeycodeY          -> doUpdate KD state
    KeycodeX          -> doUpdate KE state
    KeycodeC          -> doUpdate KF state
    _ -> return ()


doUpdate :: ( PrimMonad m
            , MonadReader env m
            , HasC8Machine env
            --, HasLogFunc env
            --, HasCallStack
            ) => C8Key -> Bool -> m ()
doUpdate k state = do
  kState <- keysState <$> view c8machineG
  mKS <- A.unsafeThaw kState
  A.write' mKS (fromEnum k) state
  _ <- A.unsafeFreeze A.Seq mKS
  return ()
