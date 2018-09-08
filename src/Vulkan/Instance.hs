{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.Instance ( createVulkanInstance ) where

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource )


createVulkanInstance
  :: MonadManaged m
  => [ String ] -> m Vulkan.VkInstance
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
