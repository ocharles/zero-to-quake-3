{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.Buffer ( createBuffer, createBufferFromList ) where

-- base
import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( liftIO )
import qualified Foreign
import qualified Foreign.Marshal

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Marshal.Extra ( allocaAndPeek )
import Foreign.Vulkan ( managedVulkanResource, throwVkResult )
import Vulkan.Memory ( allocateMemoryFor )


createBufferFromList
  :: ( MonadManaged m, Foreign.Storable a )
  => Vulkan.VkBufferUsageBitmask Vulkan.FlagMask
  -> Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ a ]
  -> m Vulkan.VkBuffer
createBufferFromList usage physicalDevice device elems =
  createBuffer
    device
    physicalDevice
    usage
    ( \memPtr -> Foreign.Marshal.pokeArray ( Foreign.castPtr memPtr ) elems )
    ( fromIntegral ( length elems * Foreign.sizeOf ( head elems ) ) )


createBuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkPhysicalDevice
  -> Vulkan.VkBufferUsageBitmask Vulkan.FlagMask
  -> (Vulkan.Ptr Vulkan.Void -> IO ())
  -> Vulkan.VkDeviceSize
  -> m Vulkan.VkBuffer
createBuffer device physicalDevice usage poke sizeInBytes = do
  let
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"size" sizeInBytes
        &* Vulkan.set @"usage" usage
        &* Vulkan.set @"sharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
        &* Vulkan.set @"queueFamilyIndexCount" 0
        &* Vulkan.set @"pQueueFamilyIndices" Vulkan.VK_NULL
        )

  buffer <-
    managedVulkanResource
      ( Vulkan.vkCreateBuffer device ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroyBuffer device )

  requirements <-
    allocaAndPeek
      ( Vulkan.vkGetBufferMemoryRequirements device buffer )

  memory <-
    allocateMemoryFor physicalDevice device requirements

  liftIO $ do
    Vulkan.vkBindBufferMemory device buffer memory 0
      >>= throwVkResult

    memPtr <-
      allocaAndPeek ( Vulkan.vkMapMemory device memory 0 sizeInBytes 0 >=> throwVkResult )

    poke memPtr

    Vulkan.vkUnmapMemory device memory

  return buffer
