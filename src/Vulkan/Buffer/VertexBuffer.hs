module Vulkan.Buffer.VertexBuffer
  ( VertexBuffer
  , bindVertexBuffers
  , createVertexBuffer
  ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign.Marshal

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Vulkan.Buffer ( Buffer(buffer), createBuffer )
import Vulkan.Poke ( Poke )


newtype VertexBuffer vertex = VertexBuffer { unVertexBuffer :: Buffer vertex }


createVertexBuffer
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Poke a
  -> a
  -> m ( VertexBuffer a )
createVertexBuffer physicalDevice device poke a =
  fmap
    VertexBuffer
    ( createBuffer
        device
        physicalDevice
        Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
        poke
        a
    )


bindVertexBuffers
  :: MonadIO m
  => Vulkan.VkCommandBuffer
  -> [ VertexBuffer a ]
  -> m ()
bindVertexBuffers commandBuffer vertexBuffers =
  liftIO $
    Foreign.Marshal.withArray ( map ( buffer . unVertexBuffer ) vertexBuffers ) $ \buffers ->
    Foreign.Marshal.withArray ( 0 <$ vertexBuffers ) $ \offsets ->
    Vulkan.vkCmdBindVertexBuffers
      commandBuffer
      0
      ( fromIntegral ( length vertexBuffers ) )
      buffers
      offsets
