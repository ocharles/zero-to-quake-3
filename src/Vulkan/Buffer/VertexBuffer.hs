module Vulkan.Buffer.VertexBuffer
  ( VertexBuffer
  , bindVertexBuffers
  , createVertexBuffer
  ) where

-- base
import Data.Coerce ( coerce )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign
import qualified Foreign.Marshal
import qualified Foreign.Marshal.Utils

-- bytestring
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Unsafe

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Vulkan.Buffer ( Buffer(buffer), createBuffer )
import Quake3.BSP ( VertexList(..) )
import Quake3.Vertex ( Vertex ) -- TODO Remove coupling

newtype VertexBuffer = VertexBuffer Buffer


createVertexBuffer
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> VertexList -- Remove coupling
  -> m VertexBuffer
createVertexBuffer physicalDevice device vertices =
  let
    nBytes =
      Data.ByteString.Lazy.length ( vertexListBytes vertices )

  in
  fmap
    VertexBuffer
    ( createBuffer
        device
        physicalDevice
        Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
        ( \dst ->
            Data.ByteString.Unsafe.unsafeUseAsCString
              ( Data.ByteString.Lazy.toStrict ( vertexListBytes vertices ) )
              ( \src ->
                  Foreign.Marshal.Utils.copyBytes
                    ( Foreign.castPtr dst )
                    src
                    ( fromIntegral nBytes )
              )
        )
        ( fromIntegral nBytes )
    )


bindVertexBuffers
  :: MonadIO m
  => Vulkan.VkCommandBuffer
  -> [ VertexBuffer ]
  -> m ()
bindVertexBuffers commandBuffer vertexBuffers =
  liftIO $
    Foreign.Marshal.withArray ( map ( buffer . coerce ) vertexBuffers ) $ \buffers ->
    Foreign.Marshal.withArray ( 0 <$ vertexBuffers ) $ \offsets ->
    Vulkan.vkCmdBindVertexBuffers
      commandBuffer
      0
      ( fromIntegral ( length vertexBuffers ) )
      buffers
      offsets
