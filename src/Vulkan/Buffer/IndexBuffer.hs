module Vulkan.Buffer.IndexBuffer
  ( IndexBuffer(..)
  , bindIndexBuffer
  , createIndexBuffer
  ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Vulkan.Buffer ( Buffer(buffer), createBuffer )
import Vulkan.Poke


newtype IndexBuffer a = IndexBuffer { unIndexBuffer :: Buffer a }

createIndexBuffer
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Poke a
  -> a
  -> m ( IndexBuffer a )
createIndexBuffer physicalDevice device poke a =
  fmap
    IndexBuffer
    ( createBuffer
        device
        physicalDevice
        Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
        poke
        a
    )


bindIndexBuffer :: MonadIO m => Vulkan.VkCommandBuffer -> IndexBuffer a -> m ()
bindIndexBuffer commandBuffer indexBuffer =
  liftIO $
    Vulkan.vkCmdBindIndexBuffer
      commandBuffer
      ( buffer ( unIndexBuffer indexBuffer ) )
      0
      Vulkan.VK_INDEX_TYPE_UINT32
