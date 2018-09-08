{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.CommandPool ( createCommandPool ) where

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource )


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
