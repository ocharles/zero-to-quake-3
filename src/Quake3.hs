{-# language OverloadedStrings #-}

module Main ( main ) where

-- base
import Data.Foldable ( for_ )
import qualified Foreign.C

-- sdl2
import qualified SDL
import qualified SDL.Raw
import qualified SDL.Video.Vulkan


main :: IO ()
main = do
  SDL.Raw.logSetAllPriority SDL.Raw.SDL_LOG_PRIORITY_VERBOSE

  SDL.initialize [ SDL.InitVideo ]

  window <-
    SDL.createWindow
      "Vulkan Quake 3"
      SDL.defaultWindow { SDL.windowVulkan = True }

  neededExtensions <-
    SDL.Video.Vulkan.vkGetInstanceExtensions window

  for_ neededExtensions $ \cStr ->
    Foreign.C.peekCString cStr >>= putStrLn

  return ()
