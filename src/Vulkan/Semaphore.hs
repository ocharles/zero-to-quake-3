{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.Semaphore ( createSemaphore ) where

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource )


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
