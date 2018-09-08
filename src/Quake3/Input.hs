{-# language LambdaCase #-}
{-# language RecordWildCards #-}

module Quake3.Input where

-- linear
import Linear

-- sdl2
import qualified SDL
import qualified SDL.Event

-- zero-to-quake-3
import qualified Quake3.Model


eventToAction :: SDL.Event -> Quake3.Model.Action
eventToAction =
  eventPayloadToAction . SDL.Event.eventPayload


eventPayloadToAction :: SDL.EventPayload -> Quake3.Model.Action
eventPayloadToAction = \case
  SDL.Event.KeyboardEvent e ->
    keyboardEventToAction e

  _ ->
    mempty


keyboardEventToAction :: SDL.KeyboardEventData -> Quake3.Model.Action
keyboardEventToAction SDL.Event.KeyboardEventData{..} | keyboardEventKeyMotion == SDL.Event.Pressed =
  Quake3.Model.Action
    { impulse =
        pure
          ( case SDL.keysymScancode keyboardEventKeysym of
              SDL.ScancodeW ->
                V3 0 0 (-0.1)

              SDL.ScancodeS ->
                V3 0 0 0.1

              SDL.ScancodeA ->
                V3 (-0.1) 0 0

              SDL.ScancodeD ->
                V3 0.1 0 0

              _ ->
                0
          )
    }
keyboardEventToAction _ =
  mempty
