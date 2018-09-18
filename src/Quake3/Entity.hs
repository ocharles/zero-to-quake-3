{-# language DeriveGeneric #-}

module Quake3.Entity ( Entity(..), parseEntity ) where

-- base
import GHC.Generics ( Generic )

-- zero-to-quake3
import qualified Quake3.BSP.Entities
import qualified Quake3.Entity.InfoPlayerDeathmatch

data Entity
  = InfoPlayerDeathmatch Quake3.Entity.InfoPlayerDeathmatch.InfoPlayerDeathmatch
  | Other
  deriving ( Generic, Show )


parseEntity
  :: Quake3.BSP.Entities.EntityProperties
  -> Maybe Entity
parseEntity props =
  InfoPlayerDeathmatch <$> Quake3.Entity.InfoPlayerDeathmatch.parse props
