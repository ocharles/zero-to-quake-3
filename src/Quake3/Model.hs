{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Quake3.Model
  ( Action(..)
  , Quake3State(..)
  , quake3
  ) where

-- base
import qualified Foreign.C
import GHC.Generics ( Generic )

-- generic-lens
import Data.Generics.Sum

-- lens
import Control.Lens ( (<&>), preview )

-- linear
import Linear ( Quaternion, V2(..), V3(..) )
import qualified Linear.Quaternion as Quaternion

-- reactive-banana
import Reactive.Banana


data Quake3State = Quake3State
  { cameraPosition :: V3 Foreign.C.CFloat
  , cameraAngles :: V2 Foreign.C.CFloat
  , cameraOrientation :: Quaternion Foreign.C.CFloat
  }


data Action
  = ToggleRunForward Bool
  | TurnBy ( V2 Foreign.C.CFloat )
  deriving ( Generic )


quake3 :: MonadMoment m => Event Action -> Event Double -> m ( Behavior Quake3State )
quake3 onAction onPhysicsStep = do
  runningForward <-
    stepper
      False
      ( filterJust ( preview ( _As @"ToggleRunForward" ) <$> onAction ) )

  let
    velocity =
      runningForward <&> \yes ->
        if yes
          then V3 0 0 (-1)
          else V3 0 0 0

  let
    onTurn =
      filterJust ( preview ( _As @"TurnBy" ) <$> onAction )

  cameraAngles <-
    accumB ( V2 0 0 ) ( subtract <$> onTurn )

  let
    orientations =
      cameraAngles <&> \( V2 x y ) ->
        Quaternion.axisAngle ( V3 0 1 0 ) x
          * Quaternion.axisAngle ( V3 1 0 0 ) y

    onForward =
      Quaternion.rotate <$> orientations <@> ( velocity <@ onPhysicsStep )

  cameraPositions <-
    accumB ( V3 0 0 0 ) ( (+) <$> onForward )

  return
    ( Quake3State
        <$> cameraPositions
        <*> cameraAngles
        <*> orientations
    )
