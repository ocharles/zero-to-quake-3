{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.CommandBuffer
  ( allocateCommandBuffer
  , submitCommandBuffer
  , withCommandBuffer
  ) where

-- base
import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign.Marshal

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Control.Monad.Managed.Extra ( manageBracket )
import Foreign.Marshal.Extra ( allocaAndPeek )
import Foreign.Vulkan ( throwVkResult )


allocateCommandBuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> m Vulkan.VkCommandBuffer
allocateCommandBuffer dev commandPool = do
  let
    allocInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"commandPool" commandPool
        &* Vulkan.set @"level" Vulkan.VK_COMMAND_BUFFER_LEVEL_PRIMARY
        &* Vulkan.set @"commandBufferCount" 1
        )

  manageBracket
    ( allocaAndPeek
        ( Vulkan.vkAllocateCommandBuffers dev ( Vulkan.unsafePtr allocInfo )
            >=> throwVkResult
        )
    )
    ( \a ->
        Foreign.Marshal.withArray [ a ]
          ( Vulkan.vkFreeCommandBuffers dev commandPool 1 )
    )


withCommandBuffer :: MonadIO m => Vulkan.VkCommandBuffer -> m a -> m a
withCommandBuffer commandBuffer action = do
  let
    commandBufferBeginInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"pInheritanceInfo" Vulkan.vkNullPtr
        )

  let
    begin =
      liftIO
        ( Vulkan.vkBeginCommandBuffer
            commandBuffer
            ( Vulkan.unsafePtr commandBufferBeginInfo )
            >>= throwVkResult
        )

    end =
      liftIO ( Vulkan.vkEndCommandBuffer commandBuffer >>= throwVkResult )

  begin *> action <* end


submitCommandBuffer
  :: MonadIO m
  => Vulkan.VkQueue
  -> Vulkan.VkCommandBuffer
  -> Vulkan.VkSemaphore
  -> Vulkan.VkSemaphore
  -> m ()
submitCommandBuffer queue commandBuffer wait signal = liftIO $ do
  let
    submitInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SUBMIT_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"waitSemaphoreCount" 1
        &* Vulkan.setListRef @"pWaitSemaphores" [ wait ]
        &* Vulkan.setListRef
             @"pWaitDstStageMask"
             [ Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ]
        &* Vulkan.set @"commandBufferCount" 1
        &* Vulkan.setListRef @"pCommandBuffers" [ commandBuffer ]
        &* Vulkan.set @"signalSemaphoreCount" 1
        &* Vulkan.setListRef @"pSignalSemaphores" [ signal ]
        )

  Foreign.Marshal.withArray [ submitInfo ] $ \submits ->
    Vulkan.vkQueueSubmit
      queue
      1
      submits
      Vulkan.vkNullPtr
      >>= throwVkResult
