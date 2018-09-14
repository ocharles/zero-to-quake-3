{-# language RecordWildCards #-}

module Main ( main ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Foldable ( traverse_ )

-- managed
import Control.Monad.Managed ( runManaged )

-- reactive-banana
import Control.Event.Handler
import Reactive.Banana
import Reactive.Banana.Frameworks

-- unliftio
import UnliftIO.IORef ( IORef, newIORef, readIORef )

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

        isRunning <-
          newIORef True

        ( io, game ) <-
          newGame resources

        liftIO ( compile game >>= actuate )

        tick context commandBuffers isRunning io


data InputOutput = InputOutput
  { onActionHandler :: Handler Quake3.Model.Action
  , onRenderHandler :: Handler ()
  , onPhysicsStepHandler :: Handler Double
  }


newGame :: MonadIO m => Quake3.Render.Resources -> m ( InputOutput, MomentIO () )
newGame resources = do
  ( onActionAddHandler, onActionHandler ) <-
    liftIO newAddHandler

  ( onRenderAddHandler, onRenderHandler ) <-
    liftIO newAddHandler

  ( onPhysicsStepAddHandler, onPhysicsStepHandler ) <-
    liftIO newAddHandler

  return
    ( InputOutput { .. }
    , do
        onRender <-
          fromAddHandler onRenderAddHandler

        onAction <-
          fromAddHandler onActionAddHandler

        onPhysicsStep <-
          fromAddHandler onPhysicsStepAddHandler

        model <-
          Quake3.Model.quake3 onAction onPhysicsStep

        reactimate
          ( Quake3.Render.updateFromModel resources
              <$> ( model <@ onRender )
          )
    )


tick
  :: MonadIO m
  => Context
  -> [ Vulkan.VkCommandBuffer ]
  -> IORef Bool
  -> InputOutput
  -> m ()
tick ctx@Context{..} commandBuffers isRunningRef io@InputOutput{..} = do
  isRunning <-
    readIORef isRunningRef

  if isRunning
    then tick'
    else return ()

  where

    tick' = do
      events <-
        SDL.pollEvents

      liftIO
        ( traverse_
            ( traverse_ onActionHandler . Quake3.Input.eventToAction )
            events
        )

      liftIO ( onPhysicsStepHandler 0.01 )

      liftIO ( onRenderHandler () )

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

      tick ctx commandBuffers isRunningRef io
