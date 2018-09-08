{-# language AllowAmbiguousTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Main ( main ) where

-- base
import Control.Monad ( forever )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Traversable ( for )
import qualified Foreign
import qualified Foreign.C

-- lens
import Control.Lens ( (.~), (&) )

-- linear
import Linear
  ( (!*!)
  , identity
  , perspective
  , translation
  , transpose
  , M44
  , V2(..)
  , V3(..)
  )
import qualified Linear.Matrix as Matrix
import qualified Linear.Quaternion as Quaternion

-- managed
import Control.Monad.Managed ( MonadManaged, runManaged )

-- sdl2
import qualified SDL
import qualified SDL.Raw
import qualified SDL.Video.Vulkan

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan ()
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
  ( VkFormat( VK_FORMAT_D16_UNORM )
  , VkImageAspectBitmask( VK_IMAGE_ASPECT_COLOR_BIT, VK_IMAGE_ASPECT_DEPTH_BIT )
  , vkCmdDrawIndexed
  , vkQueueWaitIdle
  )
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vulkan ()
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan ()

-- zero-to-quake-3
import Control.Monad.Managed.Extra ( manageBracket )
import Foreign.Vulkan ( throwVkResult )
import Vulkan.Buffer.IndexBuffer ( bindIndexBuffer, createIndexBuffer )
import Vulkan.Buffer.UniformBuffer ( createUniformBuffer )
import Vulkan.Buffer.VertexBuffer ( bindVertexBuffers, createVertexBuffer )
import Vulkan.CommandBuffer
  ( allocateCommandBuffer
  , submitCommandBuffer
  , withCommandBuffer
  )
import Vulkan.CommandPool ( createCommandPool )
import Vulkan.DescriptorSet
  ( allocateDescriptorSet
  , bindDescriptorSets
  , createDescriptorSetLayout
  , createDescriptorPool
  , updateDescriptorSet
  )
import Vulkan.Device ( createDevice, getQueue )
import Vulkan.Framebuffer ( createFramebuffer )
import Vulkan.Image ( createDepthImage )
import Vulkan.ImageView ( createImageView )
import Vulkan.Instance ( createVulkanInstance )
import Vulkan.PhysicalDevice ( createPhysicalDevice, findQueueFamilyIndex )
import Vulkan.Pipeline ( bindPipeline, createPipeline )
import Vulkan.RenderPass ( createRenderPass, withRenderPass )
import Vulkan.Semaphore ( createSemaphore )
import Vulkan.WSI
  ( acquireNextImage
  , assertSurfacePresentable
  , createSwapchain
  , determineSwapchainFormat
  , getSwapchainImages
  , present
  )


main :: IO ()
main = runManaged $ do
  enableSDLLogging
  initializeSDL

  window <-
    logMsg "Creating SDL window"
      *> createWindow

  neededExtensions <-
    logMsg "Loading needed extensions"
      *> getNeededExtensions window

  logMsg ( "Needed instance extensions are: " ++ show neededExtensions )

  vulkanInstance <-
    logMsg "Creating Vulkan instance"
      *> createVulkanInstance neededExtensions

  physicalDevice <-
    logMsg "Creating physical device"
      *> createPhysicalDevice vulkanInstance

  queueFamilyIndex <-
    logMsg "Finding suitable queue family"
      *> findQueueFamilyIndex physicalDevice

  device <-
    logMsg "Creating logical device"
      *> createDevice physicalDevice queueFamilyIndex

  surface <-
    logMsg "Creating SDL surface"
      *> SDL.Video.Vulkan.vkCreateSurface
           window
           ( Foreign.castPtr vulkanInstance )

  assertSurfacePresentable physicalDevice queueFamilyIndex surface

  ( format, colorSpace ) <-
    logMsg "Finding correct swapchain format & color space"
      *> determineSwapchainFormat physicalDevice surface

  ( swapchain, extent ) <-
    logMsg "Creating swapchain"
      *> createSwapchain physicalDevice device surface format colorSpace

  images <-
    logMsg "Getting swapchain images"
      *> getSwapchainImages device swapchain

  let
    depthFormat =
      Vulkan.VK_FORMAT_D16_UNORM

  renderPass <-
    logMsg "Creating a render pass"
      *> createRenderPass device depthFormat format

  framebuffers <- do
    logMsg "Creating frame buffers"

    for images $ \image -> do
      imageView <-
        createImageView device image format Vulkan.VK_IMAGE_ASPECT_COLOR_BIT

      depthImage <-
        createDepthImage physicalDevice device depthFormat extent

      depthImageView <-
        createImageView device depthImage depthFormat Vulkan.VK_IMAGE_ASPECT_DEPTH_BIT

      createFramebuffer device renderPass imageView depthImageView extent

  commandPool <-
    logMsg "Creating command pool"
      *> createCommandPool device queueFamilyIndex

  queue <-
    getQueue device 0

  nextImageSem <-
    createSemaphore device

  submitted <-
    createSemaphore device

  descriptorSetLayout <-
    createDescriptorSetLayout device

  descriptorPool <-
    createDescriptorPool device

  descriptorSet <-
    allocateDescriptorSet device descriptorPool descriptorSetLayout

  ( graphicsPipeline, pipelineLayout ) <-
    createPipeline device renderPass extent descriptorSetLayout

  let
    vertices =
      [ V2 ( V3 (-1) 1 1 ) ( V3 1 0 0 )
      , V2 ( V3 1 1 1 ) ( V3 1 0 0 )
      , V2 ( V3 (-1) (-1) 1 ) ( V3 1 0 0 )
      , V2 ( V3 1 (-1) 1 ) ( V3 1 0 0 )
      , V2 ( V3 (-1) 1 (-1) ) ( V3 0 1 0 )
      , V2 ( V3 1 1 (-1) ) ( V3 0 1 0 )
      , V2 ( V3 (-1) (-1) (-1) ) ( V3 0 1 0 )
      , V2 ( V3 1 (-1) (-1) ) ( V3 0 1 0 )
      ]

    indices =
      [ 2, 3, 0, 3, 0, 1
      , 6, 7, 4, 7, 4, 5
      ]

  vertexBuffer <-
    createVertexBuffer physicalDevice device vertices

  indexBuffer <-
    createIndexBuffer physicalDevice device indices

  uniformBuffer <-
    let
      view =
        identity

      model =
        let
          rotate =
            Matrix.m33_to_m44
              ( Matrix.fromQuaternion
                  ( Quaternion.axisAngle ( V3 1 1 1 )  ( pi / 5 ) )
              )

          translate =
            identity & translation .~ V3 0 0 (-5)

        in
        translate !*! rotate

      projection =
        perspective ( pi / 2 ) ( 4 / 3 ) 0.1 100

      modelViewProjection :: M44 Foreign.C.CFloat
      modelViewProjection =
        transpose ( projection !*! view !*! model )

    in
    createUniformBuffer physicalDevice device modelViewProjection

  updateDescriptorSet device descriptorSet uniformBuffer

  commandBuffers <-
    for framebuffers $ \framebuffer -> do
      commandBuffer <-
        allocateCommandBuffer device commandPool

      withCommandBuffer commandBuffer $ do
        bindVertexBuffers commandBuffer [ vertexBuffer ]

        bindIndexBuffer commandBuffer indexBuffer

        withRenderPass commandBuffer renderPass framebuffer extent $ do
          bindPipeline commandBuffer graphicsPipeline

          bindDescriptorSets commandBuffer pipelineLayout [ descriptorSet ]

          liftIO
            ( Vulkan.vkCmdDrawIndexed
                commandBuffer
                ( fromIntegral ( length indices ) )
                1
                0
                0
                0
            )

      return commandBuffer

  forever $ do
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

  where

    getNeededExtensions w =
      SDL.Video.Vulkan.vkGetInstanceExtensions w
        >>= traverse ( liftIO . Foreign.C.peekCString )


enableSDLLogging :: MonadIO m => m ()
enableSDLLogging =
  SDL.Raw.logSetAllPriority SDL.Raw.SDL_LOG_PRIORITY_VERBOSE


initializeSDL :: MonadIO m => m ()
initializeSDL =
  SDL.initialize [ SDL.InitVideo ]


createWindow :: MonadManaged m => m SDL.Window
createWindow =
  manageBracket
    ( SDL.createWindow
        "Vulkan Quake 3"
        SDL.defaultWindow
          { SDL.windowVulkan = True
          }
    )
    SDL.destroyWindow


logMsg :: MonadIO m => String -> m ()
logMsg =
  liftIO . putStrLn
