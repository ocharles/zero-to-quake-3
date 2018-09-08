{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.Device ( createDevice, getQueue ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource )
import Foreign.Marshal.Extra ( allocaAndPeek )


createDevice
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Int
  -> m Vulkan.VkDevice
createDevice physicalDevice queueFamilyIndex = do
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


getQueue :: MonadIO m => Vulkan.VkDevice -> Int -> m Vulkan.VkQueue
getQueue device queueFamilyIndex = liftIO $
  allocaAndPeek
    ( Vulkan.vkGetDeviceQueue
        device
        ( fromIntegral queueFamilyIndex )
        0
    )
