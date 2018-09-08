module Vulkan.Buffer.UniformBuffer
  ( UniformBuffer(..)
  , createUniformBuffer
  ) where

-- base
import qualified Foreign

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Vulkan.Buffer ( createBuffer )


newtype UniformBuffer = UniformBuffer Vulkan.VkBuffer


createUniformBuffer
  :: ( MonadManaged m, Foreign.Storable a )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> a
  -> m UniformBuffer
createUniformBuffer physicalDevice device bufferData =
  fmap
    UniformBuffer
    ( createBuffer
        device
        physicalDevice
        Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
        ( \memPtr -> Foreign.poke ( Foreign.castPtr memPtr ) bufferData )
        ( fromIntegral ( Foreign.sizeOf bufferData ) )
    )
