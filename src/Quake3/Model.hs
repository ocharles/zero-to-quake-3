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

-- linear
import Linear ( V3(..) )


data Quake3State = Quake3State
  { cameraPosition :: V3 Foreign.C.CFloat
  }


data Action = Action
  { impulse :: Sum ( V3 Foreign.C.CFloat )
  } deriving ( Generic )


instance Monoid Action where
  mempty = memptydefault
  mappend = mappenddefault


step :: Quake3State -> Action -> Quake3State
step Quake3State{..} Action{..} =
  Quake3State
    { cameraPosition =
        cameraPosition + getSum impulse
    }


initial :: Quake3State
initial =
  Quake3State 0
