{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.Framebuffer ( createFramebuffer ) where

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource )


createFramebuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkImageView
  -> Vulkan.VkImageView
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkFramebuffer
createFramebuffer dev renderPass colorImageView depthView extent = do
  let
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"renderPass" renderPass
        &* Vulkan.set @"attachmentCount" 2
        &* Vulkan.setListRef @"pAttachments" [ colorImageView, depthView ]
        &* Vulkan.set @"width" ( Vulkan.getField @"width" extent )
        &* Vulkan.set @"height" ( Vulkan.getField @"height" extent )
        &* Vulkan.set @"layers" 1
        )

  managedVulkanResource
    ( Vulkan.vkCreateFramebuffer
        dev
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyFramebuffer dev )
