module Haskip8.Events
    ( eventReact
    , waitForKeyPress
    ) where

import RIO

import Data.Maybe (fromJust)

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
    _ -> return ()


-- TODO: should process QuitEvent's
-- |
waitForKeyPress :: MonadUnliftIO m => m C8Key
waitForKeyPress = do
  ev <- waitEvent
  case eventPayload ev of
    KeyboardEvent kbdEvent -> do
      let mKey = isC8Key $ keysymKeycode (keyboardEventKeysym kbdEvent)
      if isJust mKey && keyboardEventKeyMotion kbdEvent == Pressed
        then return (fromJust mKey)
        else waitForKeyPress
    _ -> waitForKeyPress


-- |
isC8Key :: Keycode -> Maybe C8Key
isC8Key KeycodeKP0        = Just K0
isC8Key KeycodeKP1        = Just K1
isC8Key KeycodeKP2        = Just K2
isC8Key KeycodeKP3        = Just K3
isC8Key KeycodeKP4        = Just K4
isC8Key KeycodeKP5        = Just K5
isC8Key KeycodeKP6        = Just K6
isC8Key KeycodeKP7        = Just K7
isC8Key KeycodeKP8        = Just K8
isC8Key KeycodeKP9        = Just K9
isC8Key KeycodeKPPeriod   = Just KA
isC8Key KeycodeKPEnter    = Just KB
isC8Key KeycodeKPPlus     = Just KC
isC8Key KeycodeKPMinus    = Just KD
isC8Key KeycodeKPMultiply = Just KE
isC8Key KeycodeKPDivide   = Just KF
isC8Key _                 = Nothing
