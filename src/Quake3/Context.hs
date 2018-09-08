{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Quake3.Context ( Context(..), withQuake3Context ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Traversable ( for )
import qualified Foreign
import qualified Foreign.C

-- managed
import Control.Monad.Managed ( MonadManaged )

-- sdl2
import qualified SDL
import qualified SDL.Raw
import qualified SDL.Video.Vulkan

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Control.Monad.Managed.Extra ( manageBracket )
import Vulkan.CommandPool ( createCommandPool )
import Vulkan.DescriptorSet
  ( allocateDescriptorSet
  , createDescriptorSetLayout
  , createDescriptorPool
  )
import Vulkan.Device ( createDevice, getQueue )
import Vulkan.Framebuffer ( createFramebuffer )
import Vulkan.Image ( createDepthImage )
import Vulkan.ImageView ( createImageView )
import Vulkan.Instance ( createVulkanInstance )
import Vulkan.PhysicalDevice ( createPhysicalDevice, findQueueFamilyIndex )
import Vulkan.Pipeline ( createPipeline )
import Vulkan.RenderPass ( createRenderPass )
import Vulkan.Semaphore ( createSemaphore )
import Vulkan.WSI
  ( assertSurfacePresentable
  , createSwapchain
  , determineSwapchainFormat
  , getSwapchainImages
  )


data Context = Context
  { physicalDevice :: Vulkan.VkPhysicalDevice
  , device :: Vulkan.VkDevice
  , descriptorSet :: Vulkan.VkDescriptorSet
  , framebuffers :: [ Vulkan.VkFramebuffer ]
  , commandPool :: Vulkan.VkCommandPool
  , renderPass :: Vulkan.VkRenderPass
  , extent :: Vulkan.VkExtent2D
  , graphicsPipeline :: Vulkan.VkPipeline
  , pipelineLayout :: Vulkan.VkPipelineLayout
  , swapchain :: Vulkan.VkSwapchainKHR
  , nextImageSem :: Vulkan.VkSemaphore
  , queue :: Vulkan.VkQueue
  , submitted :: Vulkan.VkSemaphore
  }


withQuake3Context :: MonadManaged m => ( Context -> m a ) -> m a
withQuake3Context action = do
  enableSDLLogging
  initializeSDL

  window <-
    logMsg "Creating SDL window"
      *> createWindow

  _ <-
    SDL.setMouseLocationMode SDL.RelativeLocation

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

  action Context {..}

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
