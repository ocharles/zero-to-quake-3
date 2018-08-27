{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Main ( main ) where

-- base
import Data.Bits
import Data.Foldable ( for_ )
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
    SDL.defaultWindow { SDL.windowVulkan = True }


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
