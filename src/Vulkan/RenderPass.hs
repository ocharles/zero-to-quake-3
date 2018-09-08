{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.RenderPass ( createRenderPass, withRenderPass ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bits

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


createRenderPass
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkFormat
  -> Vulkan.VkFormat
  -> m Vulkan.VkRenderPass
createRenderPass dev depthFormat imageFormat = do
  let
    colorAttachmentDescription =
      Vulkan.createVk
        (  Vulkan.set @"flags" 0
        &* Vulkan.set @"format" imageFormat
        &* Vulkan.set @"samples" Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"loadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
        &* Vulkan.set @"storeOp" Vulkan.VK_ATTACHMENT_STORE_OP_STORE
        &* Vulkan.set @"stencilLoadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* Vulkan.set @"initialLayout" Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        &* Vulkan.set @"finalLayout" Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
        )

    depthAttachmentDescription =
      Vulkan.createVk
        (  Vulkan.set @"flags" 0
        &* Vulkan.set @"format" depthFormat
        &* Vulkan.set @"samples" Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"loadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
        &* Vulkan.set @"storeOp" Vulkan.VK_ATTACHMENT_STORE_OP_STORE
        &* Vulkan.set @"stencilLoadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* Vulkan.set @"initialLayout" Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        &* Vulkan.set @"finalLayout" Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        )

    colorAttachmentReference =
      Vulkan.createVk
        (  Vulkan.set @"attachment" 0
        &* Vulkan.set @"layout" Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        )

    depthAttachmentReference =
      Vulkan.createVk
        (  Vulkan.set @"attachment" 1
        &* Vulkan.set @"layout" Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        )

    subpass =
      Vulkan.createVk
        (  Vulkan.set @"flags" 0
        &* Vulkan.set @"pipelineBindPoint" Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
        &* Vulkan.set @"colorAttachmentCount" 1
        &* Vulkan.setListRef @"pColorAttachments" [ colorAttachmentReference ]
        &* Vulkan.set @"inputAttachmentCount" 0
        &* Vulkan.set @"pInputAttachments" Vulkan.vkNullPtr
        &* Vulkan.set @"pResolveAttachments" Vulkan.vkNullPtr
        &* Vulkan.setVkRef @"pDepthStencilAttachment" depthAttachmentReference
        &* Vulkan.set @"preserveAttachmentCount" 0
        &* Vulkan.set @"pPreserveAttachments" Vulkan.vkNullPtr
        )

    dependency1 =
      Vulkan.createVk
        (  Vulkan.set @"srcSubpass" Vulkan.VK_SUBPASS_EXTERNAL
        &* Vulkan.set @"dstSubpass" 0
        &* Vulkan.set @"srcStageMask" Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"srcAccessMask" 0
        &* Vulkan.set @"dstStageMask" Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"dstAccessMask" ( Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT )
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"attachmentCount" 2
        &* Vulkan.setListRef @"pAttachments" [ colorAttachmentDescription, depthAttachmentDescription ]
        &* Vulkan.set @"subpassCount" 1
        &* Vulkan.setListRef @"pSubpasses" [ subpass ]
        &* Vulkan.set @"dependencyCount" 1
        &* Vulkan.setListRef @"pDependencies" [ dependency1 ]
        )

  managedVulkanResource
    ( Vulkan.vkCreateRenderPass
        dev
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyRenderPass dev )


withRenderPass
  :: MonadIO m
  => Vulkan.VkCommandBuffer
  -> Vulkan.VkRenderPass
  -> Vulkan.VkFramebuffer
  -> Vulkan.VkExtent2D
  -> m a
  -> m a
withRenderPass commandBuffer renderPass framebuffer extent action = do
  let
    blue =
      Vulkan.createVk
        (  Vulkan.setAt @"float32" @0 0
        &* Vulkan.setAt @"float32" @1 0
        &* Vulkan.setAt @"float32" @2 1
        &* Vulkan.setAt @"float32" @3 1
        )

    depthStencil =
      Vulkan.createVk
        (  Vulkan.set @"depth" 1
        &* Vulkan.set @"stencil" 0
        )

    clearColorValues =
      Vulkan.createVk ( Vulkan.set @"color" blue )

    clearDepthValues =
      Vulkan.createVk ( Vulkan.set @"depthStencil" depthStencil )

    zeroZero =
      Vulkan.createVk
        (  Vulkan.set @"x" 0
        &* Vulkan.set @"y" 0
        )

    renderArea =
      Vulkan.createVk
        (  Vulkan.set @"offset" zeroZero
        &* Vulkan.set @"extent" extent
        )

    beginInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"renderPass" renderPass
        &* Vulkan.set @"framebuffer" framebuffer
        &* Vulkan.set @"renderArea" renderArea
        &* Vulkan.set @"clearValueCount" 2
        &* Vulkan.setListRef @"pClearValues" [ clearColorValues, clearDepthValues ]
        )

  let
    begin =
      liftIO
        ( Vulkan.vkCmdBeginRenderPass
            commandBuffer
            ( Vulkan.unsafePtr beginInfo )
            Vulkan.VK_SUBPASS_CONTENTS_INLINE
        )

    end =
      liftIO ( Vulkan.vkCmdEndRenderPass commandBuffer )

  begin *> action <* end
