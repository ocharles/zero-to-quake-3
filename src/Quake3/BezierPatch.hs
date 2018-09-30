module Quake3.BezierPatch ( tessellate ) where

import Text.Printf

-- linear
import Linear ( V2(..), V3(..) )

-- zero-to-quake-3
import qualified Quake3.Vertex


tessellatePatch :: V3 ( V3 Quake3.Vertex.Vertex ) -> [ V3 Quake3.Vertex.Vertex ]
tessellatePatch
  ( V3 ( V3 a b c )
       ( V3 d e f )
       ( V3 g h i ) ) =
  [ V3 a g c
  , V3 c g i
  ]


tessellate :: V2 Int -> [ Quake3.Vertex.Vertex ] -> [ V3 Quake3.Vertex.Vertex ]
tessellate dims =
  concatMap tessellatePatch . toPatches dims


toPatches ( V2 width height ) controlPoints =
  let
    v x y =
      controlPoints !! ( y * width + x )

  in
  [ V3 ( V3 ( v x   y       ) ( v ( x + 1 )   y       ) ( v ( x + 2 )   y       ) )
       ( V3 ( v x ( y + 1 ) ) ( v ( x + 1 ) ( y + 1 ) ) ( v ( x + 2 ) ( y + 1 ) ) )
       ( V3 ( v x ( y + 2 ) ) ( v ( x + 1 ) ( y + 2 ) ) ( v ( x + 2 ) ( y + 2 ) ) )
  | x <- [ 0, 2 .. ( width `div` 2 ) * 2 - 1]
  , y <- [ 0, 2 .. ( height `div` 2 ) * 2 - 1]
  ]


printPatch :: PrintfArg a => V3 ( V3 a ) -> IO ()
printPatch ( V3 ( V3 a b c ) ( V3 d e f ) ( V3 g h i ) ) = do
  putStrLn ( printf "%3d" a ++ printf "%3d" b ++ printf "%3d" c )
  putStrLn ( printf "%3d" d ++ printf "%3d" e ++ printf "%3d" f )
  putStrLn ( printf "%3d" g ++ printf "%3d" h ++ printf "%3d" i )


printShape :: PrintfArg a => V2 Int -> [a] -> IO ()
printShape dims@(V2 width _) xs | length xs < width = putStrLn ( concatMap ( printf "%3d" ) xs )
printShape dims@(V2 width _) xs | otherwise         = putStrLn ( concatMap ( printf "%3d" ) ( take width xs ) ) >> printShape dims ( drop width xs )
