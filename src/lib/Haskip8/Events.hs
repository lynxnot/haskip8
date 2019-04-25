module Haskip8.Events
    ( eventReact
    ) where

import RIO

import SDL

import Haskip8.Types
import Haskip8.Util


-- |
quitApp :: ( MonadReader env m
           , MonadUnliftIO m
           , HasC8Machine env
           ) => m ()
quitApp = do
  c8vm <- view c8machineG
  atomicWriteIORef (shouldExit c8vm) True


-- |
eventReact :: ( PrimMonad m
              , MonadReader env m
              , MonadUnliftIO m
              , HasC8Machine env
              ) => Event -> m ()
eventReact ev =
  case eventPayload ev of
    QuitEvent -> quitApp
    KeyboardEvent kbdEvent ->
      updateKeyState
        (keyboardEventKeysym kbdEvent)
        (keyboardEventKeyMotion kbdEvent == Pressed)
    _ -> return ()


-- |
updateKeyState :: ( PrimMonad m
                  , MonadReader env m
                  , MonadUnliftIO m
                  , HasC8Machine env
                  ) => Keysym -> Bool -> m ()
updateKeyState ksym state = do
  kState <- keysState <$> view c8machineG
  case keysymKeycode ksym of
    KeycodeQ          -> quitApp
    KeycodeKP0        -> storeKeyState kState K0 state
    KeycodeKP1        -> storeKeyState kState K1 state
    KeycodeKP2        -> storeKeyState kState K2 state
    KeycodeKP3        -> storeKeyState kState K3 state
    KeycodeKP4        -> storeKeyState kState K4 state
    KeycodeKP5        -> storeKeyState kState K5 state
    KeycodeKP6        -> storeKeyState kState K6 state
    KeycodeKP7        -> storeKeyState kState K7 state
    KeycodeKP8        -> storeKeyState kState K8 state
    KeycodeKP9        -> storeKeyState kState K9 state
    KeycodeKPPeriod   -> storeKeyState kState KA state
    KeycodeKPEnter    -> storeKeyState kState KB state
    KeycodeKPPlus     -> storeKeyState kState KC state
    KeycodeKPMinus    -> storeKeyState kState KD state
    KeycodeKPMultiply -> storeKeyState kState KE state
    KeycodeKPDivide   -> storeKeyState kState KF state
    Keycode0          -> storeKeyState kState K0 state
    Keycode1          -> storeKeyState kState K1 state
    Keycode2          -> storeKeyState kState K2 state
    Keycode3          -> storeKeyState kState K3 state
    Keycode4          -> storeKeyState kState K4 state
    Keycode5          -> storeKeyState kState K5 state
    Keycode6          -> storeKeyState kState K6 state
    Keycode7          -> storeKeyState kState K7 state
    Keycode8          -> storeKeyState kState K8 state
    Keycode9          -> storeKeyState kState K9 state
    KeycodeA          -> storeKeyState kState KA state
    KeycodeS          -> storeKeyState kState KB state
    KeycodeD          -> storeKeyState kState KC state
    KeycodeY          -> storeKeyState kState KD state
    KeycodeX          -> storeKeyState kState KE state
    KeycodeC          -> storeKeyState kState KF state
    _ -> return ()
