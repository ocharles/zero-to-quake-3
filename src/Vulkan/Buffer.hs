{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Vulkan.Buffer
  ( Buffer(..)
  , createBuffer
  , pokeBuffer
  ) where

-- base
import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign

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
import qualified Vulkan.Poke as Poke


data Buffer a = Buffer
  { buffer :: Vulkan.VkBuffer
  , device :: Vulkan.VkDevice
  , memory :: Vulkan.VkDeviceMemory
  , poke :: Poke.Poke a
  }


createBuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkPhysicalDevice
  -> Vulkan.VkBufferUsageBitmask Vulkan.FlagMask
  -> Poke.Poke a
  -> a
  -> m ( Buffer a )
createBuffer device physicalDevice usage poke a = do
  let
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"size" ( fromIntegral ( Poke.size poke a ) )
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
    allocateMemoryFor
      physicalDevice
      device
      requirements
      [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
      , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
      ]

  liftIO
    ( Vulkan.vkBindBufferMemory device buffer memory 0
        >>= throwVkResult
    )

  let
    res =
      Buffer {..}

  pokeBuffer res a

  return res


pokeBuffer
  :: MonadIO m => Buffer a -> a -> m ()
pokeBuffer Buffer { device, poke, memory } a =
  liftIO $ do
    memPtr <-
      allocaAndPeek
        ( Vulkan.vkMapMemory
            device
            memory
            0
            ( fromIntegral ( Poke.size poke a ) )
            0
            >=> throwVkResult
        )

    liftIO ( Poke.pokePtr poke ( Foreign.castPtr memPtr ) a )

    Vulkan.vkUnmapMemory device memory
