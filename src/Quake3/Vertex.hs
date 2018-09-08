module Quake3.Vertex ( Vertex ) where

-- base
import qualified Foreign.C

-- linear
import Linear


type Vertex = V2 ( V3 Foreign.C.CFloat )
