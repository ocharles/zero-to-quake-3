{-# language RecordWildCards #-}

module Main ( main ) where

-- base
import Control.Monad ( replicateM_ )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Foldable ( traverse_ )
import Data.Maybe ( mapMaybe )

-- clock
import qualified System.Clock

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
import qualified Quake3.BSP
import qualified Quake3.Entity
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

        t0 <-
          liftIO ( System.Clock.getTime System.Clock.Monotonic )

        tick context commandBuffers isRunning io t0 0


data InputOutput = InputOutput
  { onActionHandler :: Handler Quake3.Model.Action
  , onRenderHandler :: Handler ()
  , onPhysicsStepHandler :: Handler Double
  }


newGame
  :: MonadIO m
  => Quake3.Render.Resources -> m ( InputOutput, MomentIO () )
newGame resources = do
  ( onActionAddHandler, onActionHandler ) <-
    liftIO newAddHandler

  ( onRenderAddHandler, onRenderHandler ) <-
    liftIO newAddHandler

  ( onPhysicsStepAddHandler, onPhysicsStepHandler ) <-
    liftIO newAddHandler

  let
    initialEntities =
      mapMaybe
        Quake3.Entity.parseEntity
        ( Quake3.BSP.bspEntities ( Quake3.Render.bsp resources ) )

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
          Quake3.Model.quake3 initialEntities onAction onPhysicsStep

        reactimate
          ( Quake3.Render.updateFromModel resources
              <$> ( model <@ onRender )
          )
    )


-- 1/120s
physicsTimeStep :: System.Clock.TimeSpec
physicsTimeStep =
  8333333


tick
  :: MonadIO m
  => Context
  -> [ Vulkan.VkCommandBuffer ]
  -> IORef Bool
  -> InputOutput
  -> System.Clock.TimeSpec
  -> Integer
  -> m ()
tick ctx@Context{..} commandBuffers isRunningRef io@InputOutput{..} tLastFrame accumulator = do
  isRunning <-
    readIORef isRunningRef

  if isRunning
    then tick'
    else return ()

  where

    tick' :: MonadIO m => m ()
    tick' = do
      tThisFrame <-
        liftIO ( System.Clock.getTime System.Clock.Monotonic )

      let
        frameTime =
          tThisFrame - tLastFrame

      events <-
        SDL.pollEvents

      liftIO
        ( traverse_
            ( traverse_ onActionHandler . Quake3.Input.eventToAction )
            events
        )

      let
        physicsBudget =
          System.Clock.toNanoSecs frameTime + accumulator

        ( steps, accumulator' ) =
          physicsBudget `divMod` System.Clock.toNanoSecs physicsTimeStep

      liftIO
        ( do
            replicateM_
              ( fromIntegral steps )
              ( onPhysicsStepHandler ( toSeconds physicsTimeStep ) )

            onRenderHandler ()
        )

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

      tick ctx commandBuffers isRunningRef io tThisFrame accumulator'


toSeconds :: System.Clock.TimeSpec -> Double
toSeconds =
  ( / 1e-9 ) . fromIntegral . System.Clock.toNanoSecs
