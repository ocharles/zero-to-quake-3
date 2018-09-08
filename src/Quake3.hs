{-# language RecordWildCards #-}

module Main ( main ) where

-- base
import Control.Monad ( forever )
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- managed
import Control.Monad.Managed ( runManaged )

-- unliftio
import UnliftIO.IORef ( IORef, newIORef, readIORef, writeIORef )

-- sdl
import qualified SDL

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan ()
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
  ( vkQueueWaitIdle
  , VkCommandBuffer
  )
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vulkan ()
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan ()

-- zero-to-quake-3
import Foreign.Vulkan ( throwVkResult )
import Quake3.Context ( Context(..), withQuake3Context )
import qualified Quake3.Input
import qualified Quake3.Render
import qualified Quake3.Model
import Vulkan.CommandBuffer ( submitCommandBuffer )
import Vulkan.WSI ( acquireNextImage , present )


main :: IO ()
main =
  runManaged
    $ withQuake3Context
    $ \context@Context{..} -> do
        resources <-
          Quake3.Render.initResources context

        commandBuffers <-
          traverse
            ( Quake3.Render.renderToFrameBuffer context resources )
            framebuffers

        stateRef <-
          newIORef Quake3.Model.initial

        forever ( frame context resources commandBuffers stateRef )


frame
  :: MonadIO m
  => Context
  -> Quake3.Render.Resources
  -> [ Vulkan.VkCommandBuffer ]
  -> IORef Quake3.Model.Quake3State
  -> m ()
frame Context{..} resources commandBuffers stateRef = do
  events <-
    SDL.pollEvents

  s0 <-
    readIORef stateRef

  let
    s1 =
      Quake3.Model.step s0 ( foldMap Quake3.Input.eventToAction events )

  writeIORef stateRef s1

  Quake3.Render.updateFromModel resources s1

  nextImageIndex <-
    acquireNextImage device swapchain nextImageSem

  let
    commandBuffer =
      commandBuffers !! nextImageIndex

  submitCommandBuffer queue commandBuffer nextImageSem submitted

  present queue swapchain nextImageIndex submitted

  -- TODO Replace with fences
  liftIO ( Vulkan.vkQueueWaitIdle queue )
    >>= throwVkResult

