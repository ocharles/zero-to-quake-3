{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.Image ( createDepthImage ) where

-- base
import Control.Monad.IO.Class ( liftIO )

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource, throwVkResult )
import Foreign.Marshal.Extra ( allocaAndPeek )
import Vulkan.Memory ( allocateMemoryFor )


createDepthImage
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkFormat
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkImage
createDepthImage physicalDevice device depthFormat extent = do
  let
    extent3d =
      -- TODO
      Vulkan.createVk
        (  Vulkan.set @"width" ( Vulkan.getField @"width" extent )
        &* Vulkan.set @"height" ( Vulkan.getField @"height" extent )
        &* Vulkan.set @"depth" 1
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"imageType" Vulkan.VK_IMAGE_TYPE_2D
        &* Vulkan.set @"format" depthFormat
        &* Vulkan.set @"extent" extent3d
        &* Vulkan.set @"mipLevels" 1
        &* Vulkan.set @"arrayLayers" 1
        &* Vulkan.set @"samples" Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"tiling" Vulkan.VK_IMAGE_TILING_OPTIMAL
        &* Vulkan.set @"usage" Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
        &* Vulkan.set @"sharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
        &* Vulkan.set @"queueFamilyIndexCount" 0
        &* Vulkan.set @"pQueueFamilyIndices" Vulkan.VK_NULL
        &* Vulkan.set @"initialLayout" Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        )

  image <-
    managedVulkanResource
      ( Vulkan.vkCreateImage device ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroyImage device )

  memoryRequirements <-
    allocaAndPeek
      ( Vulkan.vkGetImageMemoryRequirements device image )

  memory <-
    allocateMemoryFor physicalDevice device memoryRequirements

  liftIO
    ( Vulkan.vkBindImageMemory device image memory 0
        >>= throwVkResult
    )

  return image
