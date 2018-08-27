{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Main ( main ) where

-- base
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
    SDL.Video.Vulkan.vkCreateSurface
      window
      ( Foreign.castPtr vulkanInstance )

  swapchain <-
    createSwapchain physicalDevice device surface

  images <-
    getSwapchainImages device swapchain

  fail "TODO"

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
          0

        Vulkan.writeField
          @"ppEnabledLayerNames"
          iCreateInfoPtr
          Vulkan.VK_NULL_HANDLE

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


createSwapchain
  :: Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> IO Vulkan.VkSwapchainKHR
createSwapchain physicalDevice device surface = do
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

  putStrLn "??"

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

  ( format, colorSpace ) <-
    case sortOn ( \( score, _, _ ) -> Down score ) scoredFormats of
      [] ->
        fail "No formats found"

      ( _score, format, colorSpace ) : _ ->
        return ( format, colorSpace )

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

  Foreign.Marshal.alloca $ \swapchainPtr -> do
    Vulkan.vkCreateSwapchainKHR
      device
      ( Vulkan.unsafePtr swapchainCreateInfo )
      Foreign.nullPtr
      swapchainPtr
      >>= throwVkResult

    Foreign.peek swapchainPtr


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
