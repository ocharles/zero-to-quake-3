{-# language LambdaCase #-}
{-# language RecordWildCards #-}

module Quake3.Input where

-- sdl2
import qualified SDL
import qualified SDL.Event

-- zero-to-quake-3
import qualified Quake3.Model


eventToAction :: SDL.Event -> Maybe Quake3.Model.Action
eventToAction =
  eventPayloadToAction . SDL.Event.eventPayload


eventPayloadToAction :: SDL.EventPayload -> Maybe Quake3.Model.Action
eventPayloadToAction = \case
  SDL.Event.KeyboardEvent e ->
    keyboardEventToAction e

  SDL.Event.MouseMotionEvent e ->
    mouseMotionEventToAction e

  _ ->
    Nothing


keyboardEventToAction :: SDL.KeyboardEventData -> Maybe Quake3.Model.Action
keyboardEventToAction SDL.Event.KeyboardEventData{..} =
  case SDL.keysymScancode keyboardEventKeysym of
    SDL.ScancodeW ->
      Just
        ( Quake3.Model.ToggleRunForward
            ( keyboardEventKeyMotion == SDL.Event.Pressed )
        )

    _ ->
      Nothing


mouseMotionEventToAction :: SDL.MouseMotionEventData -> Maybe Quake3.Model.Action
mouseMotionEventToAction SDL.MouseMotionEventData{..} =
  Just
    ( Quake3.Model.TurnBy
        ( fmap ( ( / 100 ) . fromIntegral ) mouseMotionEventRelMotion )
    )
