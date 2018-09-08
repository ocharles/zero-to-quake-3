module Vulkan.Buffer.IndexBuffer
  ( IndexBuffer(..)
  , bindIndexBuffer
  , createIndexBuffer
  ) where

-- base
import Data.Coerce ( coerce )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Vulkan.Buffer ( createBufferFromList )


newtype IndexBuffer = IndexBuffer Vulkan.VkBuffer

createIndexBuffer
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ Foreign.Word32 ]
  -> m IndexBuffer
createIndexBuffer physicalDevice device indices =
  fmap
    IndexBuffer
    ( createBufferFromList
        Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
        physicalDevice
        device
        indices
    )


bindIndexBuffer :: MonadIO m => Vulkan.VkCommandBuffer -> IndexBuffer -> m ()
bindIndexBuffer commandBuffer indexBuffer =
  liftIO $
    Vulkan.vkCmdBindIndexBuffer
      commandBuffer
      ( coerce indexBuffer )
      0
      Vulkan.VK_INDEX_TYPE_UINT32
