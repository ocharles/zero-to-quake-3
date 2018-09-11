module Vulkan.Buffer.IndexBuffer
  ( IndexBuffer(..)
  , bindIndexBuffer
  , createIndexBuffer
  ) where

-- base
import Data.Coerce ( coerce )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign
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
import Quake3.BSP ( MeshVertList(..) )
import Vulkan.Buffer ( Buffer(buffer), createBuffer )


newtype IndexBuffer = IndexBuffer Buffer

createIndexBuffer
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> MeshVertList
  -> m IndexBuffer
createIndexBuffer physicalDevice device meshVerts =
  let
    nBytes =
      Data.ByteString.Lazy.length ( meshVertListBytes meshVerts )

  in
  fmap
    IndexBuffer
    ( createBuffer
        device
        physicalDevice
        Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
        ( \dst ->
            Data.ByteString.Unsafe.unsafeUseAsCString
              ( Data.ByteString.Lazy.toStrict ( meshVertListBytes meshVerts ) )
              ( \src ->
                  Foreign.Marshal.Utils.copyBytes
                    ( Foreign.castPtr dst )
                    src
                    ( fromIntegral nBytes )
              )
        )
        ( fromIntegral nBytes )
    )


bindIndexBuffer :: MonadIO m => Vulkan.VkCommandBuffer -> IndexBuffer -> m ()
bindIndexBuffer commandBuffer indexBuffer =
  liftIO $
    Vulkan.vkCmdBindIndexBuffer
      commandBuffer
      ( buffer ( coerce indexBuffer ) )
      0
      Vulkan.VK_INDEX_TYPE_UINT32
