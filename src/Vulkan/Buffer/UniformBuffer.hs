module Vulkan.Buffer.UniformBuffer
  ( UniformBuffer(..)
  , createUniformBuffer
  ) where

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Vulkan.Buffer ( Buffer, createBuffer )
import Vulkan.Poke ( Poke )


newtype UniformBuffer a = UniformBuffer { unUniformBuffer :: Buffer a }


createUniformBuffer
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Poke a
  -> a
  -> m ( UniformBuffer a )
createUniformBuffer physicalDevice device poke bufferData =
  fmap
    UniformBuffer
    ( createBuffer
        device
        physicalDevice
        Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
        poke
        bufferData
    )
