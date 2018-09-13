{-# language DeriveGeneric #-}
{-# language RecordWildCards #-}

module Quake3.Model
  ( Action(..)
  , Quake3State(..)
  , initial
  , step
  ) where

-- base
import Data.Monoid ( Sum(..) )
import qualified Foreign.C
import GHC.Generics ( Generic )

-- generic-deriving
import Generics.Deriving.Monoid ( memptydefault, mappenddefault )

-- lens
import Control.Lens ( (^.) )

-- linear
import Linear ( _x, _y, V2(..), V3(..) )
import qualified Linear.Quaternion as Quaternion


data Quake3State = Quake3State
  { cameraPosition :: V3 Foreign.C.CFloat
  , cameraAngles :: V2 Foreign.C.CFloat
  }


data Action = Action
  { impulse :: Sum ( V3 Foreign.C.CFloat )
  , rotate :: Sum ( V2 Foreign.C.CFloat )
  } deriving ( Generic )


instance Monoid Action where
  mempty = memptydefault
  mappend = mappenddefault


step :: Quake3State -> Action -> Quake3State
step Quake3State{..} Action{..} =
  let
    orientation =
      Quaternion.axisAngle ( V3 0 1 0 ) ( cameraAngles ^. _x )
        * Quaternion.axisAngle ( V3 1 0 0 ) ( cameraAngles ^. _y )

  in
  Quake3State
    { cameraPosition =
        cameraPosition + Quaternion.rotate orientation ( getSum impulse )
    , cameraAngles =
        cameraAngles - getSum rotate
    }


initial :: Quake3State
initial =
  Quake3State 0 0
