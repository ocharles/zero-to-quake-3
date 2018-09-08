{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.ImageView ( createImageView ) where

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource )


createImageView
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkImage
  -> Vulkan.VkFormat
  -> Vulkan.VkImageAspectBitmask Vulkan.FlagMask
  -> m Vulkan.VkImageView
createImageView dev image format aspectMask = do
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
        (  Vulkan.set @"aspectMask" aspectMask
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
