{-# language RecordWildCards #-}

module Quake3.Model ( Quake3State(..), initial, step ) where

-- base
import qualified Foreign.C

-- linear
import Linear ( V3(..) )


data Quake3State = Quake3State
  { cameraPosition :: V3 Foreign.C.CFloat
  }


step :: Quake3State -> Quake3State
step Quake3State {..} =
  Quake3State { cameraPosition = cameraPosition - V3 0 0 0.01 }


initial :: Quake3State
initial =
  Quake3State 0
