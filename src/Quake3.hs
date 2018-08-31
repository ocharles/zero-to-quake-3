{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Main ( main ) where

-- base
import Control.Exception ( bracket )
import Control.Monad ( (>=>), forever, guard, unless )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bits
import Data.Ord ( Down(..) )
import Data.List
import Data.Traversable ( for )
import qualified Foreign
import qualified Foreign.C
import qualified Foreign.Marshal

-- bytestring
import qualified Data.ByteString

-- managed
import Control.Monad.Managed ( MonadManaged, runManaged )
import qualified Control.Monad.Managed

-- sdl2
import qualified SDL
import qualified SDL.Raw
import qualified SDL.Video.Vulkan

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan


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
      *> createLogicalDevice physicalDevice queueFamilyIndex

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

  renderPass <-
    logMsg "Creating a render pass"
      *> createRenderPass device format

  framebuffers <- do
    logMsg "Creating frame buffers"

    for images $ \image -> do
      imageView <-
        createImageView device image format

      createFramebuffer device renderPass imageView extent

  commandPool <-
    logMsg "Creating command pool"
      *> createCommandPool device queueFamilyIndex

  queue <-
    getQueue device 0

  nextImageSem <-
    createSemaphore device

  submitted <-
    createSemaphore device

  graphicsPipeline <-
    createGraphicsPipeline device renderPass extent

  commandBuffers <-
    for framebuffers $ \framebuffer -> do
      commandBuffer <-
        allocateCommandBuffer device commandPool

      beginCommandBuffer commandBuffer

      recordRenderPass commandBuffer renderPass framebuffer extent

      liftIO $ do
        Vulkan.vkCmdBindPipeline
          commandBuffer
          Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
          graphicsPipeline

        Vulkan.vkCmdDraw commandBuffer 3 1 0 0

      finishRenderPass commandBuffer

      endCommandBuffer commandBuffer

      return commandBuffer

  forever $ do
    nextImageIndex <-
      acquireNextImage device swapchain nextImageSem

    let
      commandBuffer =
        commandBuffers !! nextImageIndex

    submitCommandBuffer queue commandBuffer nextImageSem submitted

    present queue swapchain nextImageIndex submitted

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


createVulkanInstance :: MonadManaged m => [ String ] -> m Vulkan.VkInstance
createVulkanInstance neededExtensions =
  managedVulkanResource
    ( Vulkan.vkCreateInstance ( Vulkan.unsafePtr createInfo ) )
    Vulkan.vkDestroyInstance

  where

    createInfo :: Vulkan.VkInstanceCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL_HANDLE
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"pApplicationInfo" Vulkan.VK_NULL_HANDLE
        &* Vulkan.set @"enabledLayerCount" 1
        &* Vulkan.setStrListRef
             @"ppEnabledLayerNames"
             [ "VK_LAYER_LUNARG_standard_validation" ]
        &* Vulkan.set
            @"enabledExtensionCount"
            ( fromIntegral ( length neededExtensions ) )
        &* Vulkan.setStrListRef
             @"ppEnabledExtensionNames"
             neededExtensions
        )


throwVkResult :: MonadIO m => Vulkan.VkResult -> m ()
throwVkResult Vulkan.VK_SUCCESS =
  return ()
throwVkResult res =
  fail ( show res )


createPhysicalDevice :: MonadIO m => Vulkan.VkInstance -> m Vulkan.VkPhysicalDevice
createPhysicalDevice vk = liftIO $ do
  physicalDevices <-
    fetchAll
      ( \nPtr ptr ->
          Vulkan.vkEnumeratePhysicalDevices vk nPtr ptr
            >>= throwVkResult
      )

  typedDevices <-
    for physicalDevices $ \physicalDevice -> do
      properties <-
        allocaAndPeek
          ( Vulkan.vkGetPhysicalDeviceProperties physicalDevice )

      return ( physicalDevice, Vulkan.getField @"deviceType" properties )

  case filter isSuitableDevice typedDevices of
    [] ->
      fail "Could not find a suitable physical device"

    ( ( d, _deviceType ) : _ds ) ->
      return d

  where

    isSuitableDevice ( _, deviceType ) =
      deviceType
        `elem`
          [ Vulkan.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
          , Vulkan.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
          ]


findQueueFamilyIndex :: MonadIO m => Vulkan.VkPhysicalDevice -> m Int
findQueueFamilyIndex physicalDevice = liftIO $ do
  queueFamilies <-
    fetchAll
      ( \nQueueFamiliesPtr queueFamiliesPtr ->
          Vulkan.vkGetPhysicalDeviceQueueFamilyProperties
            physicalDevice
            nQueueFamiliesPtr
            queueFamiliesPtr
      )

  let
    capableFamilyIndices = do
      ( i, queueFamily ) <- zip [0 ..] queueFamilies

      let
        flags =
          Vulkan.getField @"queueFlags" queueFamily

      guard ( flags .&. Vulkan.VK_QUEUE_GRAPHICS_BIT > 0 )

      return i

  case capableFamilyIndices of
    [] ->
      fail "No queue family has sufficient capabilities"

    ( i : _ ) ->
      return i


createLogicalDevice
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Int
  -> m Vulkan.VkDevice
createLogicalDevice physicalDevice queueFamilyIndex = do
  let
    queueCreateInfo =
      Vulkan.createVk
        (  Vulkan.set
             @"sType"
             Vulkan.VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        &* Vulkan.set @"pNext" Foreign.nullPtr
        &* Vulkan.set @"queueFamilyIndex" ( fromIntegral queueFamilyIndex )
        &* Vulkan.set @"queueCount" 1
        &* Vulkan.setListRef @"pQueuePriorities" [ 1.0 :: Float ]
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
        &* Vulkan.set @"pNext" Foreign.nullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"queueCreateInfoCount" 1
        &* Vulkan.setListRef @"pQueueCreateInfos" [ queueCreateInfo ]
        &* Vulkan.set @"enabledLayerCount" 0
        &* Vulkan.set @"ppEnabledLayerNames" Foreign.nullPtr
        &* Vulkan.set @"enabledExtensionCount" 1
        &* Vulkan.setListRef
             @"ppEnabledExtensionNames"
             [ Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME ]
        &* Vulkan.set @"pEnabledFeatures" Foreign.nullPtr
        )

  managedVulkanResource
    ( Vulkan.vkCreateDevice physicalDevice ( Vulkan.unsafePtr createInfo ) )
    Vulkan.vkDestroyDevice


determineSwapchainFormat
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m ( Vulkan.VkFormat, Vulkan.VkColorSpaceKHR )
determineSwapchainFormat physicalDevice surface = liftIO $ do
  surfaceFormats <-
    fetchAll
      ( \surfaceFormatCountPtr surfaceFormatsPtr ->
        Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR
          physicalDevice
          ( Vulkan.VkPtr surface )
          surfaceFormatCountPtr
          surfaceFormatsPtr
          >>= throwVkResult
      )

  let
    scoredFormats = do
      surfaceFormat <-
        surfaceFormats

      let
        format =
          Vulkan.getField @"format" surfaceFormat

        colorSpace =
          Vulkan.getField @"colorSpace" surfaceFormat

        score :: Int
        score =
          sum
            [ if format == Vulkan.VK_FORMAT_B8G8R8A8_UNORM then 1 else 0
            , if colorSpace == Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR then 1 else 0
            ]

      return ( Down score, format, colorSpace )

  case sortOn ( \( score, _, _ ) -> score ) scoredFormats of
    [] ->
      fail "No formats found"

    ( _score, format, colorSpace ) : _ ->
      return ( format, colorSpace )


createSwapchain
  :: ( MonadIO m, MonadManaged m )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> Vulkan.VkFormat
  -> Vulkan.VkColorSpaceKHR
  -> m ( Vulkan.VkSwapchainKHR, Vulkan.VkExtent2D )
createSwapchain physicalDevice device surface format colorSpace = do
  surfaceCapabilities <- liftIO $
    allocaAndPeek
      ( Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR
          physicalDevice
          ( Vulkan.VkPtr surface )
          >=> throwVkResult
      )

  let
    minImageCount =
      Vulkan.getField @"minImageCount" surfaceCapabilities

    currentExtent =
      Vulkan.getField @"currentExtent" surfaceCapabilities

    currentTransform =
      Vulkan.getField @"currentTransform" surfaceCapabilities

    swapchainCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
        &* Vulkan.set @"pNext" Foreign.nullPtr
        &* Vulkan.set @"surface" ( Vulkan.VkPtr surface )
        &* Vulkan.set @"minImageCount" minImageCount
        &* Vulkan.set @"imageFormat" format
        &* Vulkan.set @"imageColorSpace" colorSpace
        &* Vulkan.set @"imageExtent" currentExtent
        &* Vulkan.set @"imageArrayLayers" 1
        &* Vulkan.set @"imageUsage" Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        &* Vulkan.set @"imageSharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
        &* Vulkan.set @"queueFamilyIndexCount" 0
        &* Vulkan.set @"pQueueFamilyIndices" Vulkan.vkNullPtr
        &* Vulkan.set @"preTransform" currentTransform
        &* Vulkan.set @"compositeAlpha" Vulkan.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        &* Vulkan.set @"presentMode" Vulkan.VK_PRESENT_MODE_FIFO_KHR
        &* Vulkan.set @"clipped" Vulkan.VK_TRUE
        &* Vulkan.set @"oldSwapchain" Vulkan.VK_NULL_HANDLE
        )

  swapchain <-
    managedVulkanResource
      ( Vulkan.vkCreateSwapchainKHR
          device
          ( Vulkan.unsafePtr swapchainCreateInfo )
      )
      ( Vulkan.vkDestroySwapchainKHR device )

  return ( swapchain, currentExtent )


getSwapchainImages
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkSwapchainKHR
  -> m [ Vulkan.VkImage ]
getSwapchainImages device swapchain = liftIO $
  fetchAll
    ( \imageCountPtr imagesPtr ->
        Vulkan.vkGetSwapchainImagesKHR
          device
          swapchain
          imageCountPtr
          imagesPtr
          >>= throwVkResult
    )


createRenderPass
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkFormat
  -> m Vulkan.VkRenderPass
createRenderPass dev format = do
  let
    attachmentDescription =
      Vulkan.createVk
        (  Vulkan.set @"flags" 0
        &* Vulkan.set @"format" format
        &* Vulkan.set @"samples" Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"loadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
        &* Vulkan.set @"storeOp" Vulkan.VK_ATTACHMENT_STORE_OP_STORE
        &* Vulkan.set @"stencilLoadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* Vulkan.set @"initialLayout" Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        &* Vulkan.set @"finalLayout" Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
        )

    colorAttachmentReference =
      Vulkan.createVk
        (  Vulkan.set @"attachment" 0
        &* Vulkan.set @"layout" Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        )

    subpass =
      Vulkan.createVk
        (  Vulkan.set @"flags" 0
        &* Vulkan.set @"pipelineBindPoint" Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
        &* Vulkan.set @"colorAttachmentCount" 1
        &* Vulkan.setListRef @"pColorAttachments" [ colorAttachmentReference ]
        &* Vulkan.set @"inputAttachmentCount" 0
        &* Vulkan.set @"pInputAttachments" Vulkan.vkNullPtr
        &* Vulkan.set @"pResolveAttachments" Vulkan.vkNullPtr
        &* Vulkan.set @"pDepthStencilAttachment" Vulkan.vkNullPtr
        &* Vulkan.set @"preserveAttachmentCount" 0
        &* Vulkan.set @"pPreserveAttachments" Vulkan.vkNullPtr
        )

    dependency1 =
      Vulkan.createVk
        (  Vulkan.set @"srcSubpass" Vulkan.VK_SUBPASS_EXTERNAL
        &* Vulkan.set @"dstSubpass" 0
        &* Vulkan.set @"srcStageMask" Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"srcAccessMask" 0
        &* Vulkan.set @"dstStageMask" Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"dstAccessMask" ( Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT )
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"attachmentCount" 1
        &* Vulkan.setListRef @"pAttachments" [ attachmentDescription ]
        &* Vulkan.set @"subpassCount" 1
        &* Vulkan.setListRef @"pSubpasses" [ subpass ]
        &* Vulkan.set @"dependencyCount" 1
        &* Vulkan.setListRef @"pDependencies" [ dependency1 ]
        )

  managedVulkanResource
    ( Vulkan.vkCreateRenderPass
        dev
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyRenderPass dev )


createFramebuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkImageView
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkFramebuffer
createFramebuffer dev renderPass colorImageView extent = do
  let
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"renderPass" renderPass
        &* Vulkan.set @"attachmentCount" 1
        &* Vulkan.setListRef @"pAttachments" [ colorImageView ]
        &* Vulkan.set @"width" ( Vulkan.getField @"width" extent )
        &* Vulkan.set @"height" ( Vulkan.getField @"height" extent )
        &* Vulkan.set @"layers" 1
        )

  managedVulkanResource
    ( Vulkan.vkCreateFramebuffer
        dev
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyFramebuffer dev )


createImageView
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkImage
  -> Vulkan.VkFormat
  -> m Vulkan.VkImageView
createImageView dev image format = do
  let
    components =
      Vulkan.createVk
        (  Vulkan.set @"r" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        &* Vulkan.set @"g" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        &* Vulkan.set @"b" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        &* Vulkan.set @"a" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        )

    subResourceRange =
      Vulkan.createVk
        (  Vulkan.set @"aspectMask" Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
        &* Vulkan.set @"baseMipLevel" 0
        &* Vulkan.set @"levelCount" 1
        &* Vulkan.set @"baseArrayLayer" 0
        &* Vulkan.set @"layerCount" 1
        )

    createInfo =
      Vulkan.createVk
        ( Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"image" image
        &* Vulkan.set @"viewType" Vulkan.VK_IMAGE_VIEW_TYPE_2D
        &* Vulkan.set @"format" format
        &* Vulkan.set @"components" components
        &* Vulkan.set @"subresourceRange" subResourceRange
        )

  managedVulkanResource
    ( Vulkan.vkCreateImageView
        dev
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyImageView dev )


createCommandPool
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Int
  -> m Vulkan.VkCommandPool
createCommandPool dev queueFamilyIndex = do
  let
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"queueFamilyIndex" ( fromIntegral queueFamilyIndex )
        )

  managedVulkanResource
    ( Vulkan.vkCreateCommandPool
        dev
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyCommandPool dev )


allocateCommandBuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> m Vulkan.VkCommandBuffer
allocateCommandBuffer dev commandPool = do
  let
    allocInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"commandPool" commandPool
        &* Vulkan.set @"level" Vulkan.VK_COMMAND_BUFFER_LEVEL_PRIMARY
        &* Vulkan.set @"commandBufferCount" 1
        )

  manageBracket
    ( allocaAndPeek
        ( Vulkan.vkAllocateCommandBuffers dev ( Vulkan.unsafePtr allocInfo )
            >=> throwVkResult
        )
    )
    ( \a ->
        Foreign.Marshal.withArray [ a ]
          ( Vulkan.vkFreeCommandBuffers dev commandPool 1 )
    )


recordRenderPass
  :: MonadIO m
  => Vulkan.VkCommandBuffer
  -> Vulkan.VkRenderPass
  -> Vulkan.VkFramebuffer
  -> Vulkan.VkExtent2D
  -> m ()
recordRenderPass commandBuffer renderPass framebuffer extent = liftIO $ do
  let
    blue =
      Vulkan.createVk
        (  Vulkan.setAt @"float32" @0 0
        &* Vulkan.setAt @"float32" @1 0
        &* Vulkan.setAt @"float32" @2 1
        &* Vulkan.setAt @"float32" @3 1
        )

    clearValues =
      Vulkan.createVk ( Vulkan.set @"color" blue )

    zeroZero =
      Vulkan.createVk
        (  Vulkan.set @"x" 0
        &* Vulkan.set @"y" 0
        )

    renderArea =
      Vulkan.createVk
        (  Vulkan.set @"offset" zeroZero
        &* Vulkan.set @"extent" extent
        )

    beginInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"renderPass" renderPass
        &* Vulkan.set @"framebuffer" framebuffer
        &* Vulkan.set @"renderArea" renderArea
        &* Vulkan.set @"clearValueCount" 1
        &* Vulkan.setListRef @"pClearValues" [ clearValues ]
        )

  Vulkan.vkCmdBeginRenderPass
    commandBuffer
    ( Vulkan.unsafePtr beginInfo )
    Vulkan.VK_SUBPASS_CONTENTS_INLINE


acquireNextImage
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkSwapchainKHR
  -> Vulkan.VkSemaphore
  -> m Int
acquireNextImage device swapchain signal = liftIO $
  fmap
    fromIntegral
    ( allocaAndPeek
        ( Vulkan.vkAcquireNextImageKHR
            device
            swapchain
            maxBound
            signal
            Vulkan.VK_NULL_HANDLE
            >=> throwVkResult
        )
    )


present
  :: MonadIO m
  => Vulkan.VkQueue
  -> Vulkan.VkSwapchainKHR
  -> Int
  -> Vulkan.VkSemaphore
  -> m ()
present queue swapchain imageIndex wait = liftIO $ do
  let
    presentInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"waitSemaphoreCount" 1
        &* Vulkan.setListRef @"pWaitSemaphores" [ wait ]
        &* Vulkan.set @"swapchainCount" 1
        &* Vulkan.setListRef @"pSwapchains" [ swapchain ]
        &* Vulkan.setListRef @"pImageIndices" [ fromIntegral imageIndex ]
        &* Vulkan.set @"pResults" Vulkan.vkNullPtr
        )

  Vulkan.vkQueuePresentKHR queue ( Vulkan.unsafePtr presentInfo )
    >>= throwVkResult


getQueue :: MonadIO m => Vulkan.VkDevice -> Int -> m Vulkan.VkQueue
getQueue device queueFamilyIndex = liftIO $
  allocaAndPeek
    ( Vulkan.vkGetDeviceQueue
        device
        ( fromIntegral queueFamilyIndex )
        0
    )


createSemaphore :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkSemaphore
createSemaphore device = do
  let
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL_HANDLE
        &* Vulkan.set @"flags" 0
        )

  managedVulkanResource
    ( Vulkan.vkCreateSemaphore device ( Vulkan.unsafePtr createInfo ) )
    ( Vulkan.vkDestroySemaphore device )


assertSurfacePresentable
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> Int
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m ()
assertSurfacePresentable physicalDevice queueFamilyIndex surface = liftIO $ do
  bool <-
    allocaAndPeek
      ( Vulkan.vkGetPhysicalDeviceSurfaceSupportKHR
          physicalDevice
          ( fromIntegral queueFamilyIndex )
          ( Vulkan.VkPtr surface )
          >=> throwVkResult
      )

  unless
    ( bool == Vulkan.VK_TRUE )
    ( fail "Unsupported surface" )


finishRenderPass :: MonadIO m => Vulkan.VkCommandBuffer -> m ()
finishRenderPass =
  liftIO . Vulkan.vkCmdEndRenderPass


submitCommandBuffer
  :: MonadIO m
  => Vulkan.VkQueue
  -> Vulkan.VkCommandBuffer
  -> Vulkan.VkSemaphore
  -> Vulkan.VkSemaphore
  -> m ()
submitCommandBuffer queue commandBuffer wait signal = liftIO $ do
  let
    submitInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SUBMIT_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"waitSemaphoreCount" 1
        &* Vulkan.setListRef @"pWaitSemaphores" [ wait ]
        &* Vulkan.setListRef
             @"pWaitDstStageMask"
             [ Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ]
        &* Vulkan.set @"commandBufferCount" 1
        &* Vulkan.setListRef @"pCommandBuffers" [ commandBuffer ]
        &* Vulkan.set @"signalSemaphoreCount" 1
        &* Vulkan.setListRef @"pSignalSemaphores" [ signal ]
        )

  Foreign.Marshal.withArray [ submitInfo ] $ \submits ->
    Vulkan.vkQueueSubmit
      queue
      1
      submits
      Vulkan.vkNullPtr
      >>= throwVkResult


beginCommandBuffer :: MonadIO m => Vulkan.VkCommandBuffer -> m ()
beginCommandBuffer commandBuffer = liftIO $ do
  let
    commandBufferBeginInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"pInheritanceInfo" Vulkan.vkNullPtr
        )

  Vulkan.vkBeginCommandBuffer
    commandBuffer
    ( Vulkan.unsafePtr commandBufferBeginInfo )
    >>= throwVkResult


endCommandBuffer :: MonadIO m => Vulkan.VkCommandBuffer -> m ()
endCommandBuffer =
  liftIO . Vulkan.vkEndCommandBuffer >=> throwVkResult


allocaAndPeek :: Foreign.Storable a => ( Vulkan.Ptr a -> IO () ) -> IO a
allocaAndPeek f =
  Foreign.Marshal.alloca
    ( \ptr ->
        f ptr *> Foreign.peek ptr
    )


allocaAndPeekArray :: Foreign.Storable a => Int -> ( Vulkan.Ptr a -> IO () ) -> IO [ a ]
allocaAndPeekArray n f =
  Foreign.Marshal.allocaArray
    n
    ( \ptr ->
        f ptr *> Foreign.Marshal.peekArray n ptr
    )


fetchAll
  :: ( Foreign.Storable a, Foreign.Storable b, Integral b )
  => ( Vulkan.Ptr b -> Vulkan.Ptr a -> IO () ) -> IO [a]
fetchAll f = do
  Foreign.Marshal.alloca $ \nPtr -> do
    f nPtr Vulkan.vkNullPtr

    n <-
      fromIntegral <$> Foreign.peek nPtr

    allocaAndPeekArray n ( f nPtr )


manageBracket :: MonadManaged m => IO a -> (a -> IO b) -> m a
manageBracket create destroy =
  managed ( bracket create destroy )


logMsg :: MonadIO m => String -> m ()
logMsg =
  liftIO . putStrLn


managed :: MonadManaged m => (forall r. (a -> IO r) -> IO r) -> m a
managed f =
  Control.Monad.Managed.using ( Control.Monad.Managed.managed f )


managedVulkanResource
  :: ( MonadManaged m, Foreign.Storable x, Vulkan.VulkanPtr ptr )
  => ( ptr a -> Vulkan.Ptr x -> IO Vulkan.VkResult )
  -> ( x -> ptr a -> IO () )
  -> m x
managedVulkanResource create destroy =
  manageBracket
    ( allocaAndPeek
        ( create Vulkan.vkNullPtr >=> throwVkResult )
    )
    ( \a -> destroy a Vulkan.vkNullPtr )


createGraphicsPipeline
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkPipeline
createGraphicsPipeline device renderPass extent = do
  pipelineLayout <-
    let
      pipelineLayoutCreateInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" 0
          &* Vulkan.set @"pSetLayouts" Vulkan.VK_NULL
          &* Vulkan.set @"pPushConstantRanges" Vulkan.VK_NULL
          )

    in
    managedVulkanResource
      ( Vulkan.vkCreatePipelineLayout
          device
          ( Vulkan.unsafePtr pipelineLayoutCreateInfo )
      )
      ( Vulkan.vkDestroyPipelineLayout device )

  vertexShader <-
    loadShader device "/home/ollie/work/zero-to-quake3/vert.spv"

  fragmentShader <-
    loadShader device "/home/ollie/work/zero-to-quake3/frag.spv"

  let
    rasterizationCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"depthClampEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"rasterizerDiscardEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"polygonMode" Vulkan.VK_POLYGON_MODE_FILL
        &* Vulkan.set @"lineWidth" 1
        &* Vulkan.set @"depthBiasEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"depthBiasSlopeFactor" 0
        &* Vulkan.set @"depthBiasClamp" 0
        &* Vulkan.set @"depthBiasConstantFactor" 0
        &* Vulkan.set @"frontFace" Vulkan.VK_FRONT_FACE_CLOCKWISE
        )

    vertexShaderStage =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.setStrRef @"pName" "main"
        &* Vulkan.set @"module" vertexShader
        &* Vulkan.set @"stage" Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        )

    fragmentShaderStage =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.setStrRef @"pName" "main"
        &* Vulkan.set @"module" fragmentShader
        &* Vulkan.set @"stage" Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
        )

    vertexInputState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        &* Vulkan.set @"pVertexAttributeDescriptions" Vulkan.VK_NULL
        &* Vulkan.set @"pVertexBindingDescriptions" Vulkan.VK_NULL
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        )

    assemblyStateCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"topology" Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        &* Vulkan.set @"primitiveRestartEnable" Vulkan.VK_FALSE
        )

    viewport =
      Vulkan.createVk
        (  Vulkan.set @"x" 0
        &* Vulkan.set @"y" 0
        &* Vulkan.set @"width" ( fromIntegral ( Vulkan.getField @"width" extent ) )
        &* Vulkan.set @"height" ( fromIntegral ( Vulkan.getField @"height" extent ) )
        &* Vulkan.set @"minDepth" 0
        &* Vulkan.set @"maxDepth" 1
        )

    scissor =
      let
        offset =
          Vulkan.createVk
            (  Vulkan.set @"x" 0
            &* Vulkan.set @"y" 0
            )

      in
      Vulkan.createVk
        (  Vulkan.set @"offset" offset
        &* Vulkan.set @"extent" extent
        )

    viewportState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"viewportCount" 1
        &* Vulkan.set @"scissorCount" 1
        &* Vulkan.setListRef @"pViewports" [ viewport ]
        &* Vulkan.setListRef @"pScissors" [ scissor ]
        )

    multisampleState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
        &* Vulkan.set @"minSampleShading" 1
        &* Vulkan.set @"rasterizationSamples" Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        )

    attachmentState =
      Vulkan.createVk
        ( Vulkan.set @"blendEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"alphaBlendOp" Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set @"colorBlendOp" Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set
             @"colorWriteMask"
             ( Vulkan.VK_COLOR_COMPONENT_R_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_G_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_B_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_A_BIT
             )
        )

    colorBlendState =
      Vulkan.createVk
        ( Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
        &* Vulkan.setAt @"blendConstants" @0 0
        &* Vulkan.setAt @"blendConstants" @1 0
        &* Vulkan.setAt @"blendConstants" @2 0
        &* Vulkan.setAt @"blendConstants" @3 0
        &* Vulkan.set @"attachmentCount" 1
        &* Vulkan.setListRef @"pAttachments" [ attachmentState ]
        &* Vulkan.set @"logicOp" Vulkan.VK_LOGIC_OP_COPY
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"stageCount" 2
        &* Vulkan.setListRef @"pStages" [ vertexShaderStage, fragmentShaderStage ]
        &* Vulkan.setVkRef @"pVertexInputState" vertexInputState
        &* Vulkan.set @"basePipelineIndex" 0
        &* Vulkan.set @"subpass" 0
        &* Vulkan.set @"renderPass" renderPass
        &* Vulkan.set @"layout" pipelineLayout
        &* Vulkan.setVkRef @"pRasterizationState" rasterizationCreateInfo
        &* Vulkan.setVkRef @"pInputAssemblyState" assemblyStateCreateInfo
        &* Vulkan.setVkRef @"pViewportState" viewportState
        &* Vulkan.setVkRef @"pMultisampleState" multisampleState
        &* Vulkan.setVkRef @"pColorBlendState" colorBlendState
        )

  managedVulkanResource
    ( Vulkan.vkCreateGraphicsPipelines
        device
        Vulkan.vkNullPtr
        1
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyPipeline device )


loadShader :: MonadManaged m => Vulkan.VkDevice -> FilePath -> m Vulkan.VkShaderModule
loadShader device srcFile = do
  bytes <-
    liftIO ( Data.ByteString.readFile srcFile )

  managedVulkanResource
    ( \a b ->
        Data.ByteString.useAsCStringLen bytes $ \( bytesPtr, len ) ->
          let
            createInfo =
              Vulkan.createVk
                (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
                &* Vulkan.set @"pNext" Vulkan.VK_NULL
                &* Vulkan.set @"flags" 0
                &* Vulkan.set @"pCode" ( Foreign.castPtr bytesPtr )
                &* Vulkan.set @"codeSize" ( fromIntegral len )
                )

          in
          Vulkan.vkCreateShaderModule device ( Vulkan.unsafePtr createInfo ) a b
    )
    ( Vulkan.vkDestroyShaderModule device )
