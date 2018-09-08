{-# language DataKinds #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Vulkan.Buffer
  ( Buffer(..)
  , createBuffer
  , createBufferFromList
  , pokeBuffer
  ) where

-- base
import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( MonadIO, liftIO )
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


data Buffer = Buffer
  { buffer :: Vulkan.VkBuffer
  , device :: Vulkan.VkDevice
  , memory :: Vulkan.VkDeviceMemory
  }


createBufferFromList
  :: ( MonadManaged m, Foreign.Storable a )
  => Vulkan.VkBufferUsageBitmask Vulkan.FlagMask
  -> Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ a ]
  -> m Buffer
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
  -> m Buffer
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

  liftIO
    ( Vulkan.vkBindBufferMemory device buffer memory 0
        >>= throwVkResult
    )

  let
    res =
      Buffer {..}

  pokeBufferWith poke sizeInBytes res

  return res


pokeBuffer
  :: ( Foreign.Storable a, MonadIO m )
  => Buffer -> a -> m ()
pokeBuffer buffer contents =
  pokeBufferWith
    ( \ptr -> Foreign.poke ( Foreign.castPtr ptr ) contents )
    ( fromIntegral ( Foreign.sizeOf contents ) )
    buffer

pokeBufferWith
  :: MonadIO m
  => ( Vulkan.Ptr Vulkan.Void -> IO () )
  -> Vulkan.VkDeviceSize
  -> Buffer
  -> m ()
pokeBufferWith poke sizeInBytes Buffer {..} =
  liftIO $ do
    memPtr <-
      allocaAndPeek ( Vulkan.vkMapMemory device memory 0 sizeInBytes 0 >=> throwVkResult )

    poke memPtr

    Vulkan.vkUnmapMemory device memory
