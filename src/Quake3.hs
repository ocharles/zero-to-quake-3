{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Main ( main ) where

-- base
import Control.Monad ( (>=>), forever, unless )
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
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan


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

  names <-
    traverse Foreign.C.peekCString neededExtensions

  putStrLn ( "Needed instance extensions are: " ++ show names )

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


createVulkanInstance :: [ Vulkan.CString ] -> IO Vulkan.VkInstance
createVulkanInstance neededExtensions = do
  createInfo <-
    newVkCreateInfo

  create createInfo

  where


    newVkCreateInfo =
      Vulkan.newVkData $ \iCreateInfoPtr -> do
        Vulkan.writeField
          @"sType"
          iCreateInfoPtr
          Vulkan.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO

        Vulkan.writeField
          @"pNext"
          iCreateInfoPtr
          Vulkan.VK_NULL_HANDLE

        Vulkan.writeField
          @"flags"
          iCreateInfoPtr
          0

        Vulkan.writeField
          @"pApplicationInfo"
          iCreateInfoPtr
          Vulkan.VK_NULL_HANDLE

        Vulkan.writeField
          @"enabledLayerCount"
          iCreateInfoPtr
          1

        Foreign.C.withCString "VK_LAYER_LUNARG_standard_validation" $ \str ->
          Foreign.Marshal.withArray [ str ] $
          Vulkan.writeField @"ppEnabledLayerNames" iCreateInfoPtr

        Vulkan.writeField
          @"enabledExtensionCount"
          iCreateInfoPtr
          ( fromIntegral ( length neededExtensions ) )

        Foreign.Marshal.withArray neededExtensions $
          Vulkan.writeField @"ppEnabledExtensionNames" iCreateInfoPtr

    create createInfo =
      Foreign.Marshal.alloca $ \ptr -> do
        Vulkan.vkCreateInstance
          ( Vulkan.unsafePtr createInfo )
          Vulkan.VK_NULL_HANDLE
          ptr
          >>= throwVkResult

        Foreign.peek ptr


throwVkResult :: Vulkan.VkResult -> IO ()
throwVkResult Vulkan.VK_SUCCESS =
  return ()
throwVkResult res =
  fail ( show res )


createPhysicalDevice :: Vulkan.VkInstance -> IO Vulkan.VkPhysicalDevice
createPhysicalDevice vk =
  Foreign.Marshal.alloca $ \nDevicesPtr -> do
    Vulkan.vkEnumeratePhysicalDevices vk nDevicesPtr Foreign.nullPtr
      >>= throwVkResult

    nDevices <-
      fromIntegral <$> Foreign.peek nDevicesPtr

    physicalDevices <-
      Foreign.Marshal.allocaArray nDevices $ \physicalDevicesPtr -> do
        Vulkan.vkEnumeratePhysicalDevices vk nDevicesPtr physicalDevicesPtr
          >>= throwVkResult

        Foreign.Marshal.peekArray nDevices physicalDevicesPtr

    typedDevices <-
      for physicalDevices $ \physicalDevice -> do
        properties <-
          Foreign.Marshal.alloca $ \propertiesPtr -> do
            Vulkan.vkGetPhysicalDeviceProperties
              physicalDevice
              propertiesPtr

            Foreign.peek propertiesPtr

        deviceType <-
          Vulkan.readField @"deviceType" ( Vulkan.unsafePtr properties )

        return ( physicalDevice, deviceType )

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
findQueueFamilyIndex physicalDevice =
  Foreign.Marshal.alloca $ \nQueueFamiliesPtr -> do
    Vulkan.vkGetPhysicalDeviceQueueFamilyProperties
      physicalDevice
      nQueueFamiliesPtr
      Foreign.nullPtr

    nQueueFamilies <-
      fromIntegral <$> Foreign.peek nQueueFamiliesPtr

    queueFamilies <-
      Foreign.Marshal.allocaArray nQueueFamilies $ \queueFamiliesPtr -> do
        Vulkan.vkGetPhysicalDeviceQueueFamilyProperties
          physicalDevice
          nQueueFamiliesPtr
          queueFamiliesPtr

        Foreign.Marshal.peekArray nQueueFamilies queueFamiliesPtr

    capabilities <-
      for ( zip [0 ..] queueFamilies ) $ \( i, queueFamily ) -> do
        flags <-
          Vulkan.readField
            @"queueFlags"
            ( Vulkan.unsafePtr queueFamily )

        let
          capable =
            flags .&. Vulkan.VK_QUEUE_GRAPHICS_BIT > 0

        return ( i, capable )

    case filter isCapable capabilities of
      [] ->
        fail "No queue family has sufficient capabilities"

      ( ( i, _ ) : _ ) ->
        return i

  where

    -- TODO Check that VK_KHR_swapchain is supported
    isCapable ( _, capable ) =
      capable


createLogicalDevice
  :: Vulkan.VkPhysicalDevice
  -> Int
  -> IO Vulkan.VkDevice
createLogicalDevice physicalDevice queueFamilyIndex = do
  Foreign.Marshal.alloca $ \devicePtr -> do
    queueCreateInfo <-
      Vulkan.newVkData $ \ptr -> do
        Vulkan.writeField
          @"sType"
          ptr
          Vulkan.VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO

        Vulkan.writeField
          @"pNext"
          ptr
          Foreign.nullPtr

        Vulkan.writeField
          @"queueFamilyIndex"
          ptr
          ( fromIntegral queueFamilyIndex )

        Vulkan.writeField
          @"queueCount"
          ptr
          1

        Foreign.Marshal.withArray [ 1.0 :: Float ] $
          Vulkan.writeField
            @"pQueuePriorities"
            ptr

    createInfo <-
      Vulkan.newVkData $ \ptr -> do
        Vulkan.writeField
          @"sType"
          ptr
          Vulkan.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO

        Vulkan.writeField
          @"pNext"
          ptr
          Foreign.nullPtr

        Vulkan.writeField
          @"flags"
          ptr
          0

        Vulkan.writeField
          @"queueCreateInfoCount"
          ptr
          1

        Foreign.Marshal.withArray [ queueCreateInfo ] $
          Vulkan.writeField
            @"pQueueCreateInfos"
            ptr

        Vulkan.writeField
          @"enabledLayerCount"
          ptr
          0

        Vulkan.writeField
          @"ppEnabledLayerNames"
          ptr
          Foreign.nullPtr

        Vulkan.writeField
          @"enabledExtensionCount"
          ptr
          1

        Foreign.Marshal.withArray [ Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME ] $
          Vulkan.writeField @"ppEnabledExtensionNames" ptr

        Vulkan.writeField
          @"pEnabledFeatures"
          ptr
          Foreign.nullPtr

    Vulkan.vkCreateDevice
      physicalDevice
      ( Vulkan.unsafePtr createInfo )
      Foreign.nullPtr
      devicePtr
      >>= throwVkResult

    Foreign.peek devicePtr


determineSwapchainFormat
  :: Vulkan.VkPhysicalDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> IO ( Vulkan.VkFormat, Vulkan.VkColorSpaceKHR )
determineSwapchainFormat physicalDevice surface = do
  surfaceFormats <-
    Foreign.Marshal.alloca $ \surfaceFormatCountPtr -> do
      Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR
        physicalDevice
        ( Vulkan.VkPtr surface )
        surfaceFormatCountPtr
        Foreign.nullPtr
        >>= throwVkResult

      surfaceFormatCount <-
        fromIntegral <$> Foreign.peek surfaceFormatCountPtr

      Foreign.Marshal.allocaArray surfaceFormatCount $ \surfaceFormatsPtr -> do
        Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR
          physicalDevice
          ( Vulkan.VkPtr surface )
          surfaceFormatCountPtr
          surfaceFormatsPtr
          >>= throwVkResult

        Foreign.peekArray surfaceFormatCount surfaceFormatsPtr

  scoredFormats <-
    for surfaceFormats $ \surfaceFormat -> do
      format <-
        Vulkan.readField @"format" ( Vulkan.unsafePtr surfaceFormat )

      colorSpace <-
        Vulkan.readField @"colorSpace" ( Vulkan.unsafePtr surfaceFormat )

      let
        score :: Int
        score =
          sum
            [ if format == Vulkan.VK_FORMAT_B8G8R8A8_UNORM then 1 else 0
            , if colorSpace == Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR then 1 else 0
            ]

      return ( score, format, colorSpace )

  case sortOn ( \( score, _, _ ) -> Down score ) scoredFormats of
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
    Foreign.Marshal.alloca $ \ptr -> do
      Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR
        physicalDevice
        ( Vulkan.VkPtr surface )
        ptr
        >>= throwVkResult

      Foreign.peek ptr

  minImageCount <-
    Vulkan.readField @"minImageCount" ( Vulkan.unsafePtr surfaceCapabilities )

  currentExtent <-
    Vulkan.readField @"currentExtent" ( Vulkan.unsafePtr surfaceCapabilities )

  currentTransform <-
    Vulkan.readField @"currentTransform" ( Vulkan.unsafePtr surfaceCapabilities )

  swapchainCreateInfo <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField
        @"sType"
        ptr
        Vulkan.VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR

      Vulkan.writeField
        @"pNext"
        ptr
        Foreign.nullPtr

      Vulkan.writeField
        @"surface"
        ptr
        ( Vulkan.VkPtr surface )

      Vulkan.writeField
        @"minImageCount"
        ptr
        minImageCount

      Vulkan.writeField
        @"imageFormat"
        ptr
        format

      Vulkan.writeField
        @"imageColorSpace"
        ptr
        colorSpace

      Vulkan.writeField
        @"imageExtent"
        ptr
        currentExtent

      Vulkan.writeField
        @"imageArrayLayers"
        ptr
        1

      Vulkan.writeField
        @"imageUsage"
        ptr
        Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT

      Vulkan.writeField
        @"imageSharingMode"
        ptr
        Vulkan.VK_SHARING_MODE_EXCLUSIVE

      Vulkan.writeField
        @"queueFamilyIndexCount"
        ptr
        0

      Vulkan.writeField
        @"pQueueFamilyIndices"
        ptr
        Vulkan.vkNullPtr

      Vulkan.writeField
        @"preTransform"
        ptr
        currentTransform

      Vulkan.writeField
        @"compositeAlpha"
        ptr
        Vulkan.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR

      Vulkan.writeField
        @"presentMode"
        ptr
        Vulkan.VK_PRESENT_MODE_FIFO_KHR

      Vulkan.writeField
        @"clipped"
        ptr
        Vulkan.VK_TRUE

      Vulkan.writeField
        @"oldSwapchain"
        ptr
        Vulkan.VK_NULL_HANDLE

  swapchain <-
    Foreign.Marshal.alloca $ \swapchainPtr -> do
      Vulkan.vkCreateSwapchainKHR
        device
        ( Vulkan.unsafePtr swapchainCreateInfo )
        Foreign.nullPtr
        swapchainPtr
        >>= throwVkResult

      Foreign.peek swapchainPtr

  return ( swapchain, currentExtent )


getSwapchainImages
  :: Vulkan.VkDevice
  -> Vulkan.VkSwapchainKHR
  -> IO [ Vulkan.VkImage ]
getSwapchainImages device swapchain =
  Foreign.Marshal.alloca $ \imageCountPtr -> do
    Vulkan.vkGetSwapchainImagesKHR
      device
      swapchain
      imageCountPtr
      Foreign.nullPtr
      >>= throwVkResult

    imageCount <-
      fromIntegral <$> Foreign.peek imageCountPtr

    Foreign.Marshal.allocaArray imageCount $ \imagesPtr -> do
      Vulkan.vkGetSwapchainImagesKHR
        device
        swapchain
        imageCountPtr
        imagesPtr
        >>= throwVkResult

      Foreign.peekArray imageCount imagesPtr


createRenderPass :: Vulkan.VkDevice -> Vulkan.VkFormat -> IO Vulkan.VkRenderPass
createRenderPass dev format = do
  attachmentDescription <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField
        @"flags"
        ptr
        0

      Vulkan.writeField
        @"format"
        ptr
        format

      Vulkan.writeField
        @"samples"
        ptr
        Vulkan.VK_SAMPLE_COUNT_1_BIT

      Vulkan.writeField
        @"loadOp"
        ptr
        Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR

      Vulkan.writeField
        @"storeOp"
        ptr
        Vulkan.VK_ATTACHMENT_STORE_OP_STORE

      Vulkan.writeField
        @"stencilLoadOp"
        ptr
        Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE

      Vulkan.writeField
        @"stencilStoreOp"
        ptr
        Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE

      Vulkan.writeField
        @"initialLayout"
        ptr
        Vulkan.VK_IMAGE_LAYOUT_UNDEFINED

      Vulkan.writeField
        @"finalLayout"
        ptr
        Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

  colorAttachmentReference <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"attachment" ptr 0

      Vulkan.writeField
        @"layout"
        ptr
        Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

  subpass <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"flags" ptr 0

      Vulkan.writeField
        @"pipelineBindPoint"
        ptr
        Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS

      Vulkan.writeField
        @"inputAttachmentCount"
        ptr
        0

      Vulkan.writeField
        @"pInputAttachments"
        ptr
        Vulkan.vkNullPtr

      Vulkan.writeField
        @"colorAttachmentCount"
        ptr
        1

      Foreign.Marshal.withArray [ colorAttachmentReference ] $
        Vulkan.writeField @"pColorAttachments" ptr

      Vulkan.writeField
        @"pResolveAttachments"
        ptr
        Vulkan.vkNullPtr

      Vulkan.writeField
        @"pDepthStencilAttachment"
        ptr
        Vulkan.vkNullPtr

      Vulkan.writeField
        @"preserveAttachmentCount"
        ptr
        0

      Vulkan.writeField
        @"pPreserveAttachments"
        ptr
        Vulkan.vkNullPtr

  Foreign.Marshal.alloca $ \renderPassPtr -> do
    createInfo <-
      Vulkan.newVkData $ \ptr -> do
        Vulkan.writeField
          @"sType"
          ptr
          Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO

        Vulkan.writeField
          @"pNext"
          ptr
          Vulkan.vkNullPtr

        Vulkan.writeField
          @"flags"
          ptr
          0

        Vulkan.writeField
          @"attachmentCount"
          ptr
          1

        Foreign.Marshal.withArray [ attachmentDescription ] $
          Vulkan.writeField @"pAttachments" ptr

        Vulkan.writeField
          @"subpassCount"
          ptr
          1

        Foreign.Marshal.withArray [ subpass ] $
          Vulkan.writeField @"pSubpasses" ptr

        Vulkan.writeField
          @"dependencyCount"
          ptr
          0

        Vulkan.writeField
          @"pDependencies"
          ptr
          Vulkan.vkNullPtr

    Vulkan.vkCreateRenderPass
      dev
      ( Vulkan.unsafePtr createInfo )
      Vulkan.vkNullPtr
      renderPassPtr
      >>= throwVkResult

    Foreign.peek renderPassPtr


createFramebuffer
  :: Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkImageView
  -> Vulkan.VkExtent2D
  -> IO Vulkan.VkFramebuffer
createFramebuffer dev renderPass colorImageView extent = do
  Foreign.Marshal.alloca $ \framebufferPtr -> do
    createInfo <-
      Vulkan.newVkData $ \ptr -> do
        Vulkan.writeField
          @"sType"
          ptr
          Vulkan.VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO

        Vulkan.writeField
          @"pNext"
          ptr
          Vulkan.vkNullPtr

        Vulkan.writeField
          @"flags"
          ptr
          0

        Vulkan.writeField
          @"renderPass"
          ptr
          renderPass

        Vulkan.writeField
          @"attachmentCount"
          ptr
          1

        Foreign.Marshal.withArray [ colorImageView ] $
          Vulkan.writeField @"pAttachments" ptr

        Vulkan.readField @"width" ( Vulkan.unsafePtr extent )
          >>= Vulkan.writeField @"width" ptr

        Vulkan.readField @"height" ( Vulkan.unsafePtr extent )
          >>= Vulkan.writeField @"height" ptr

        Vulkan.writeField @"layers" ptr 1

    Vulkan.vkCreateFramebuffer
      dev
      ( Vulkan.unsafePtr createInfo )
      Vulkan.vkNullPtr
      framebufferPtr
      >>= throwVkResult

    Foreign.peek framebufferPtr


createImageView
  :: Vulkan.VkDevice
  -> Vulkan.VkImage
  -> Vulkan.VkFormat
  -> IO Vulkan.VkImageView
createImageView dev image format = do
  components <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"r" ptr Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY

      Vulkan.writeField @"g" ptr Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY

      Vulkan.writeField @"b" ptr Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY

      Vulkan.writeField @"a" ptr Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY

  subResourceRange <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"aspectMask" ptr Vulkan.VK_IMAGE_ASPECT_COLOR_BIT

      Vulkan.writeField @"baseMipLevel" ptr 0

      Vulkan.writeField @"levelCount" ptr 1

      Vulkan.writeField @"baseArrayLayer" ptr 0

      Vulkan.writeField @"layerCount" ptr 1

  Foreign.Marshal.alloca $ \imageViewPtr -> do
    createInfo <-
      Vulkan.newVkData $ \ptr -> do
        Vulkan.writeField
          @"sType"
          ptr
          Vulkan.VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO

        Vulkan.writeField @"pNext" ptr Vulkan.vkNullPtr

        Vulkan.writeField @"flags" ptr 0

        Vulkan.writeField @"image" ptr image

        Vulkan.writeField @"viewType" ptr Vulkan.VK_IMAGE_VIEW_TYPE_2D

        Vulkan.writeField @"format" ptr format

        Vulkan.writeField @"components" ptr components

        Vulkan.writeField @"subresourceRange" ptr subResourceRange

    Vulkan.vkCreateImageView
      dev
      ( Vulkan.unsafePtr createInfo )
      Vulkan.vkNullPtr
      imageViewPtr
      >>= throwVkResult

    Foreign.peek imageViewPtr


createCommandPool :: Vulkan.VkDevice -> Int -> IO Vulkan.VkCommandPool
createCommandPool dev queueFamilyIndex = do
  Foreign.Marshal.alloca $ \commandPoolPtr -> do
    createInfo <-
      Vulkan.newVkData $ \ptr -> do
        Vulkan.writeField
          @"sType"
          ptr
          Vulkan.VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO

        Vulkan.writeField @"pNext" ptr Vulkan.vkNullPtr

        Vulkan.writeField @"flags" ptr 0

        Vulkan.writeField
          @"queueFamilyIndex"
          ptr
          ( fromIntegral queueFamilyIndex )

    Vulkan.vkCreateCommandPool
      dev
      ( Vulkan.unsafePtr createInfo )
      Vulkan.vkNullPtr
      commandPoolPtr
      >>= throwVkResult

    Foreign.peek commandPoolPtr


allocateCommandBuffer
  :: Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> IO Vulkan.VkCommandBuffer
allocateCommandBuffer dev commandPool =
  Foreign.Marshal.alloca $ \commandBufferPtr -> do
    allocInfo <-
      Vulkan.newVkData $ \ptr -> do
        Vulkan.writeField
          @"sType"
          ptr
          Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO

        Vulkan.writeField @"pNext" ptr Vulkan.vkNullPtr

        Vulkan.writeField @"commandPool" ptr commandPool

        Vulkan.writeField @"level" ptr Vulkan.VK_COMMAND_BUFFER_LEVEL_PRIMARY

        Vulkan.writeField @"commandBufferCount" ptr 1

    Vulkan.vkAllocateCommandBuffers
      dev
      ( Vulkan.unsafePtr allocInfo )
      commandBufferPtr
      >>= throwVkResult

    Foreign.peek commandBufferPtr


recordRenderPass
  :: Vulkan.VkCommandBuffer
  -> Vulkan.VkRenderPass
  -> Vulkan.VkFramebuffer
  -> Vulkan.VkExtent2D
  -> IO ()
recordRenderPass commandBuffer renderPass framebuffer extent = do
  blue <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeFieldArray @"float32" @0 ptr 0

      Vulkan.writeFieldArray @"float32" @1 ptr 0

      Vulkan.writeFieldArray @"float32" @2 ptr 1

      Vulkan.writeFieldArray @"float32" @3 ptr 1

  clearValues <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"color" ptr blue

  zeroZero <-
    Vulkan.newVkData $ \ptr -> do
    Vulkan.writeField @"x" ptr 0
    Vulkan.writeField @"y" ptr 0

  renderArea <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"offset" ptr zeroZero
      Vulkan.writeField @"extent" ptr extent

  beginInfo <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"sType" ptr Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO

      Vulkan.writeField @"pNext" ptr Vulkan.vkNullPtr

      Vulkan.writeField @"renderPass" ptr renderPass

      Vulkan.writeField @"framebuffer" ptr framebuffer

      Vulkan.writeField @"renderArea" ptr renderArea

      Vulkan.writeField @"clearValueCount" ptr 1

      Foreign.Marshal.withArray [ clearValues ] $
        Vulkan.writeField @"pClearValues" ptr

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
  Foreign.Marshal.alloca $ \ptr -> do
    Vulkan.vkAcquireNextImageKHR
      device
      swapchain
      maxBound
      signal
      Vulkan.VK_NULL_HANDLE
      ptr
      >>= throwVkResult

    fromIntegral <$> Foreign.peek ptr


present
  :: Vulkan.VkQueue
  -> Vulkan.VkSwapchainKHR
  -> Int
  -> Vulkan.VkSemaphore
  -> IO ()
present queue swapchain imageIndex wait = do
  presentInfo <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"sType" ptr Vulkan.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR

      Vulkan.writeField @"pNext" ptr Vulkan.vkNullPtr

      Vulkan.writeField @"waitSemaphoreCount" ptr 1

      Foreign.Marshal.withArray [ wait ] $
        Vulkan.writeField @"pWaitSemaphores" ptr

      Vulkan.writeField @"swapchainCount" ptr 1

      Foreign.Marshal.withArray [ swapchain ] $
        Vulkan.writeField @"pSwapchains" ptr

      Foreign.Marshal.withArray [ fromIntegral imageIndex ] $
        Vulkan.writeField @"pImageIndices" ptr

      Vulkan.writeField @"pResults" ptr Vulkan.vkNullPtr

  Vulkan.vkQueuePresentKHR queue ( Vulkan.unsafePtr presentInfo )
    >>= throwVkResult


getQueue :: Vulkan.VkDevice -> Int -> IO Vulkan.VkQueue
getQueue device queueFamilyIndex =
  Foreign.Marshal.alloca $ \queuePtr -> do
    Vulkan.vkGetDeviceQueue
      device
      ( fromIntegral queueFamilyIndex )
      0
      queuePtr

    Foreign.peek queuePtr


createSemaphore :: Vulkan.VkDevice -> IO Vulkan.VkSemaphore
createSemaphore device = do
  createInfo <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField
        @"sType"
        ptr
        Vulkan.VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO

      Vulkan.writeField @"pNext" ptr Vulkan.VK_NULL_HANDLE

      Vulkan.writeField @"flags" ptr 0

  Foreign.Marshal.alloca $ \semaphorePtr -> do
    Vulkan.vkCreateSemaphore
      device
      ( Vulkan.unsafePtr createInfo )
      Vulkan.vkNullPtr
      semaphorePtr
      >>= throwVkResult

    Foreign.peek semaphorePtr


assertSurfacePresentable
  :: Vulkan.VkPhysicalDevice
  -> Int
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> IO ()
assertSurfacePresentable physicalDevice queueFamilyIndex surface = do
  bool <-
    Foreign.Marshal.alloca $ \presentablePtr -> do
      Vulkan.vkGetPhysicalDeviceSurfaceSupportKHR
        physicalDevice
        ( fromIntegral queueFamilyIndex )
        ( Vulkan.VkPtr surface )
        presentablePtr
        >>= throwVkResult

      Foreign.peek presentablePtr

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
  submitInfo <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"sType" ptr Vulkan.VK_STRUCTURE_TYPE_SUBMIT_INFO

      Vulkan.writeField @"pNext" ptr Vulkan.vkNullPtr

      Vulkan.writeField @"waitSemaphoreCount" ptr 1

      Foreign.Marshal.withArray [ wait ] $
        Vulkan.writeField @"pWaitSemaphores" ptr

      Foreign.Marshal.withArray [ Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ] $
        Vulkan.writeField @"pWaitDstStageMask" ptr

      Vulkan.writeField @"commandBufferCount" ptr 1

      Foreign.Marshal.withArray [ commandBuffer ] $
        Vulkan.writeField @"pCommandBuffers" ptr

      Vulkan.writeField @"signalSemaphoreCount" ptr 1

      Foreign.Marshal.withArray [ signal ] $
        Vulkan.writeField @"pSignalSemaphores" ptr

  Foreign.Marshal.withArray [ submitInfo ] $ \submits ->
    Vulkan.vkQueueSubmit
      queue
      1
      submits
      Vulkan.vkNullPtr
      >>= throwVkResult


beginCommandBuffer :: Vulkan.VkCommandBuffer -> IO ()
beginCommandBuffer commandBuffer = do
  commandBufferBeginInfo <-
    Vulkan.newVkData $ \ptr -> do
      Vulkan.writeField @"sType" ptr Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO

      Vulkan.writeField @"pNext" ptr Vulkan.vkNullPtr

      Vulkan.writeField @"flags" ptr 0

      Vulkan.writeField @"pInheritanceInfo" ptr Vulkan.vkNullPtr

  Vulkan.vkBeginCommandBuffer
    commandBuffer
    ( Vulkan.unsafePtr commandBufferBeginInfo )
    >>= throwVkResult


endCommandBuffer :: Vulkan.VkCommandBuffer -> IO ()
endCommandBuffer =
  Vulkan.vkEndCommandBuffer >=> throwVkResult
