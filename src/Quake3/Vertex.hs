{-# language RecordWildCards #-}

module Quake3.Vertex ( Vertex(..), vertexFormat, poke ) where

-- base
import Data.Word ( Word8 )
import qualified Foreign.C

-- contravarient
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible ( Divisible, divided )

-- linear
import Linear

-- zero-to-quake3
import Vulkan.Poke
import Vulkan.VertexFormat


data Vertex = Vertex
  { vPos :: V3 Foreign.C.CFloat
  , vSurfaceUV :: V2 Foreign.C.CFloat
  , vLightmapUV :: V2 Foreign.C.CFloat
  , vNormal :: V3 Foreign.C.CFloat
  , vColor :: V4 Word8
  } deriving ( Show )

vertexFormat :: VertexFormat Vertex
vertexFormat =
  Vertex
    <$> v3_32sfloat
    <*> v2_32sfloat
    <*> v2_32sfloat
    <*> v3_32sfloat
    <*> v4_8uint

  where

    vertex Vertex{..} =
      ( vPos, ( vSurfaceUV, ( vLightmapUV, ( vNormal, vColor ) ) ) )


(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) = divided

infixr 5 >*<


poke :: Poke Vertex
poke =
  vertex
    >$< storable
    >*< storable
    >*< storable
    >*< storable
    >*< storable

  where

    vertex Vertex{..} =
      ( vPos, ( vSurfaceUV, ( vLightmapUV, ( vNormal, vColor ) ) ) )
