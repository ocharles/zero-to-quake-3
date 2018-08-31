{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Main ( main ) where

-- base
import Control.Monad ( (>=>), forever, guard, unless )
import Data.Bits
import Data.Ord ( Down(..) )
import Data.List
import Data.Traversable ( for )
import qualified Foreign
import qualified Foreign.C
import qualified Foreign.Marshal

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
main = do
  enableSDLLogging
  initializeSDL

  window <-
    putStrLn "Creating SDL window"
      *> createWindow

  neededExtensions <-
    putStrLn "Loading needed extensions"
      *> getNeededExtensions window

  putStrLn ( "Needed instance extensions are: " ++ show neededExtensions )

  vulkanInstance <-
    putStrLn "Creating Vulkan instance"
      *> createVulkanInstance neededExtensions

  physicalDevice <-
    putStrLn "Creating physical device"
      *> createPhysicalDevice vulkanInstance

  queueFamilyIndex <-
    putStrLn "Finding suitable queue family"
      *> findQueueFamilyIndex physicalDevice

  device <-
    putStrLn "Creating logical device"
      *> createLogicalDevice physicalDevice queueFamilyIndex

  surface <-
    putStrLn "Creating SDL surface"
      *> SDL.Video.Vulkan.vkCreateSurface
           window
           ( Foreign.castPtr vulkanInstance )

  assertSurfacePresentable physicalDevice queueFamilyIndex surface

  ( format, colorSpace ) <-
    putStrLn "Finding correct swapchain format & color space"
      *> determineSwapchainFormat physicalDevice surface

  ( swapchain, extent ) <-
    putStrLn "Creating swapchain"
      *> createSwapchain physicalDevice device surface format colorSpace

  images <-
    putStrLn "Getting swapchain images"
      *> getSwapchainImages device swapchain

  renderPass <-
    putStrLn "Creating a render pass"
      *> createRenderPass device format

  framebuffers <- do
    putStrLn "Creating frame buffers"

    for images $ \image -> do
      imageView <-
        createImageView device image format

      createFramebuffer device renderPass imageView extent

  commandPool <-
    putStrLn "Creating command pool"
      *> createCommandPool device queueFamilyIndex

  queue <-
    getQueue device 0

  nextImageSem <-
    createSemaphore device

  submitted <-
    createSemaphore device

  commandBuffers <-
    for framebuffers $ \framebuffer -> do
      commandBuffer <-
        allocateCommandBuffer device commandPool

      beginCommandBuffer commandBuffer

      recordRenderPass commandBuffer renderPass framebuffer extent

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

    Vulkan.vkQueueWaitIdle queue
      >>= throwVkResult

  where

    getNeededExtensions w =
      SDL.Video.Vulkan.vkGetInstanceExtensions w
        >>= traverse Foreign.C.peekCString


enableSDLLogging :: IO ()
enableSDLLogging =
  SDL.Raw.logSetAllPriority SDL.Raw.SDL_LOG_PRIORITY_VERBOSE


initializeSDL :: IO ()
initializeSDL =
  SDL.initialize [ SDL.InitVideo ]


createWindow :: IO SDL.Window
createWindow =
  SDL.createWindow
    "Vulkan Quake 3"
    SDL.defaultWindow
      { SDL.windowVulkan = True
      }

createVulkanInstance :: [ String ] -> IO Vulkan.VkInstance
createVulkanInstance neededExtensions =
  allocaAndPeek $
    Vulkan.vkCreateInstance
      ( Vulkan.unsafePtr createInfo )
      Vulkan.VK_NULL_HANDLE
      >=> throwVkResult

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


throwVkResult :: Vulkan.VkResult -> IO ()
throwVkResult Vulkan.VK_SUCCESS =
  return ()
throwVkResult res =
  fail ( show res )


createPhysicalDevice :: Vulkan.VkInstance -> IO Vulkan.VkPhysicalDevice
createPhysicalDevice vk = do
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

  case filter isSuitabelDevice typedDevices of
    [] ->
      fail "Could not find a suitable physical device"

    ( ( d, _deviceType ) : _ds ) ->
      return d

  where

    isSuitabelDevice ( _, deviceType ) =
      deviceType
        `elem`
          [ Vulkan.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
          , Vulkan.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
          ]


findQueueFamilyIndex :: Vulkan.VkPhysicalDevice -> IO Int
findQueueFamilyIndex physicalDevice = do
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
  :: Vulkan.VkPhysicalDevice
  -> Int
  -> IO Vulkan.VkDevice
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

  allocaAndPeek
    ( Vulkan.vkCreateDevice
        physicalDevice
        ( Vulkan.unsafePtr createInfo )
        Foreign.nullPtr
        >=> throwVkResult
    )


determineSwapchainFormat
  :: Vulkan.VkPhysicalDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> IO ( Vulkan.VkFormat, Vulkan.VkColorSpaceKHR )
determineSwapchainFormat physicalDevice surface = do
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
  :: Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> Vulkan.VkFormat
  -> Vulkan.VkColorSpaceKHR
  -> IO ( Vulkan.VkSwapchainKHR, Vulkan.VkExtent2D )
createSwapchain physicalDevice device surface format colorSpace = do
  surfaceCapabilities <-
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
    allocaAndPeek
      ( Vulkan.vkCreateSwapchainKHR
          device
          ( Vulkan.unsafePtr swapchainCreateInfo )
          Foreign.nullPtr
          >=> throwVkResult
      )

  return ( swapchain, currentExtent )


getSwapchainImages
  :: Vulkan.VkDevice
  -> Vulkan.VkSwapchainKHR
  -> IO [ Vulkan.VkImage ]
getSwapchainImages device swapchain =
  fetchAll
    ( \imageCountPtr imagesPtr ->
        Vulkan.vkGetSwapchainImagesKHR
          device
          swapchain
          imageCountPtr
          imagesPtr
          >>= throwVkResult
    )


createRenderPass :: Vulkan.VkDevice -> Vulkan.VkFormat -> IO Vulkan.VkRenderPass
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
        &* Vulkan.set @"inputAttachmentCount" 0
        &* Vulkan.set @"pInputAttachments" Vulkan.vkNullPtr
        &* Vulkan.set @"colorAttachmentCount" 1
        &* Vulkan.setListRef @"pColorAttachments" [ colorAttachmentReference ]
        &* Vulkan.set @"pResolveAttachments" Vulkan.vkNullPtr
        &* Vulkan.set @"pDepthStencilAttachment" Vulkan.vkNullPtr
        &* Vulkan.set @"preserveAttachmentCount" 0
        &* Vulkan.set @"pPreserveAttachments" Vulkan.vkNullPtr
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
        &* Vulkan.set @"dependencyCount" 0
        &* Vulkan.set @"pDependencies" Vulkan.vkNullPtr
        )

  allocaAndPeek
    ( Vulkan.vkCreateRenderPass
        dev
        ( Vulkan.unsafePtr createInfo )
        Vulkan.vkNullPtr
        >=> throwVkResult
    )


createFramebuffer
  :: Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkImageView
  -> Vulkan.VkExtent2D
  -> IO Vulkan.VkFramebuffer
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

  allocaAndPeek
    ( Vulkan.vkCreateFramebuffer
        dev
        ( Vulkan.unsafePtr createInfo )
        Vulkan.vkNullPtr
        >=> throwVkResult
    )


createImageView
  :: Vulkan.VkDevice
  -> Vulkan.VkImage
  -> Vulkan.VkFormat
  -> IO Vulkan.VkImageView
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

  allocaAndPeek
    ( Vulkan.vkCreateImageView
        dev
        ( Vulkan.unsafePtr createInfo )
        Vulkan.vkNullPtr
        >=> throwVkResult
    )


createCommandPool :: Vulkan.VkDevice -> Int -> IO Vulkan.VkCommandPool
createCommandPool dev queueFamilyIndex = do
  let
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"queueFamilyIndex" ( fromIntegral queueFamilyIndex )
        )

  allocaAndPeek
    ( Vulkan.vkCreateCommandPool
        dev
        ( Vulkan.unsafePtr createInfo )
        Vulkan.vkNullPtr
        >=> throwVkResult
    )


allocateCommandBuffer
  :: Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> IO Vulkan.VkCommandBuffer
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

  allocaAndPeek
    ( Vulkan.vkAllocateCommandBuffers dev ( Vulkan.unsafePtr allocInfo )
        >=> throwVkResult
    )


recordRenderPass
  :: Vulkan.VkCommandBuffer
  -> Vulkan.VkRenderPass
  -> Vulkan.VkFramebuffer
  -> Vulkan.VkExtent2D
  -> IO ()
recordRenderPass commandBuffer renderPass framebuffer extent = do
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
  :: Vulkan.VkDevice
  -> Vulkan.VkSwapchainKHR
  -> Vulkan.VkSemaphore
  -> IO Int
acquireNextImage device swapchain signal =
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
  :: Vulkan.VkQueue
  -> Vulkan.VkSwapchainKHR
  -> Int
  -> Vulkan.VkSemaphore
  -> IO ()
present queue swapchain imageIndex wait = do
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


getQueue :: Vulkan.VkDevice -> Int -> IO Vulkan.VkQueue
getQueue device queueFamilyIndex =
  allocaAndPeek
    ( Vulkan.vkGetDeviceQueue
        device
        ( fromIntegral queueFamilyIndex )
        0
    )


createSemaphore :: Vulkan.VkDevice -> IO Vulkan.VkSemaphore
createSemaphore device = do
  let
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL_HANDLE
        &* Vulkan.set @"flags" 0
        )

  allocaAndPeek
    ( Vulkan.vkCreateSemaphore
        device
        ( Vulkan.unsafePtr createInfo )
        Vulkan.vkNullPtr
        >=> throwVkResult
    )


assertSurfacePresentable
  :: Vulkan.VkPhysicalDevice
  -> Int
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> IO ()
assertSurfacePresentable physicalDevice queueFamilyIndex surface = do
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


finishRenderPass :: Vulkan.VkCommandBuffer -> IO ()
finishRenderPass =
  Vulkan.vkCmdEndRenderPass


submitCommandBuffer
  :: Vulkan.VkQueue
  -> Vulkan.VkCommandBuffer
  -> Vulkan.VkSemaphore
  -> Vulkan.VkSemaphore
  -> IO ()
submitCommandBuffer queue commandBuffer wait signal = do
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


beginCommandBuffer :: Vulkan.VkCommandBuffer -> IO ()
beginCommandBuffer commandBuffer = do
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


endCommandBuffer :: Vulkan.VkCommandBuffer -> IO ()
endCommandBuffer =
  Vulkan.vkEndCommandBuffer >=> throwVkResult


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


fetchAll f = do
  Foreign.Marshal.alloca $ \nPtr -> do
    f nPtr Vulkan.vkNullPtr

    n <-
      fromIntegral <$> Foreign.peek nPtr

    allocaAndPeekArray n ( f nPtr )
