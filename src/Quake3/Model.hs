{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language DisambiguateRecordFields #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Quake3.Model
  ( Action(..)
  , Quake3State(..)
  , quake3
  ) where

-- base
import Data.List ( sortOn )
import Data.Maybe ( mapMaybe )
import Data.Ord ( Down(..) )
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

-- zero-to-quake3
import qualified Quake3.Entity
import qualified Quake3.Entity.InfoPlayerDeathmatch as InfoPlayerDeathmatch


data Quake3State = Quake3State
  { cameraPosition :: V3 Foreign.C.CFloat
  , cameraAngles :: V2 Foreign.C.CFloat
  , cameraOrientation :: Quaternion Foreign.C.CFloat
  }


data Action
  = ToggleRunForward Bool
  | TurnBy ( V2 Foreign.C.CFloat )
  deriving ( Generic )


quake3
  :: MonadMoment m
  => [ Quake3.Entity.Entity ]
  -> Event Action
  -> Event Double
  -> m ( Behavior Quake3State )
quake3 initialEntities onAction onPhysicsStep = do
  let
    spawnPoints =
      sortOn
        ( Down . ( Just 1 == ) . InfoPlayerDeathmatch.spawnFlags )
        ( mapMaybe
            ( preview ( _As @"InfoPlayerDeathmatch" ) )
            initialEntities
        )

    initialSpawnPoint =
      case spawnPoints of
        [] ->
          InfoPlayerDeathmatch.InfoPlayerDeathmatch
            { origin = 0
            , angle = 0
            , spawnFlags = Nothing
            }

        ( a : _ ) ->
          a

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
    accumB
      ( V2
          ( degToRad
              ( fromIntegral ( InfoPlayerDeathmatch.angle initialSpawnPoint ) )
          )
          0
      )
      ( subtract . liftA2 (*) ( V2 (-1) 1 ) <$> onTurn )

  let
    orientations =
      cameraAngles <&> \( V2 x y ) ->
        Quaternion.axisAngle ( V3 0 1 0 ) x
          * Quaternion.axisAngle ( V3 1 0 0 ) y

    onForward =
      Quaternion.rotate <$> orientations <@> ( velocity <@ onPhysicsStep )

  cameraPositions <-
    accumB
      ( fromIntegral <$> InfoPlayerDeathmatch.origin initialSpawnPoint )
      ( (+) <$> onForward )

  return
    ( Quake3State
        <$> cameraPositions
        <*> cameraAngles
        <*> orientations
    )


degToRad :: Floating a => a -> a
degToRad x =
  x * ( pi / 180 )
