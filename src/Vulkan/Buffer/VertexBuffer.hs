module Vulkan.Buffer.VertexBuffer ( VertexBuffer, bindVertexBuffers, createVertexBuffer ) where

-- base
import Data.Coerce ( coerce )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign.Marshal

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Vulkan.Buffer ( createBufferFromList )
import Quake3.Vertex ( Vertex ) -- TODO Remove coupling


newtype VertexBuffer = VertexBuffer Vulkan.VkBuffer


createVertexBuffer
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ Vertex ]
  -> m VertexBuffer
createVertexBuffer physicalDevice device vertices =
  fmap
    VertexBuffer
    ( createBufferFromList
        Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
        physicalDevice
        device
        vertices
    )


bindVertexBuffers
  :: MonadIO m
  => Vulkan.VkCommandBuffer
  -> [ VertexBuffer ]
  -> m ()
bindVertexBuffers commandBuffer vertexBuffers =
  liftIO $
    Foreign.Marshal.withArray ( coerce vertexBuffers ) $ \buffers ->
    Foreign.Marshal.withArray ( 0 <$ vertexBuffers ) $ \offsets ->
    Vulkan.vkCmdBindVertexBuffers
      commandBuffer
      0
      ( fromIntegral ( length vertexBuffers ) )
      buffers
      offsets
