{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.Memory ( allocateMemoryFor ) where

-- base
import Control.Monad ( (>=>), guard )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bits
import qualified Foreign
import qualified Foreign.Marshal

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Marshal.Extra ( allocaAndPeek )
import Foreign.Vulkan ( throwVkResult )


allocateMemoryFor
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkMemoryRequirements
  -> m Vulkan.VkDeviceMemory
allocateMemoryFor physicalDevice device requirements = do
  memoryProperties <-
    allocaAndPeek
      ( Vulkan.vkGetPhysicalDeviceMemoryProperties physicalDevice )

  let
    memoryTypeCount =
      Vulkan.getField @"memoryTypeCount" memoryProperties

  memoryTypes <-
    liftIO $
    Foreign.Marshal.peekArray
      @Vulkan.VkMemoryType
      ( fromIntegral memoryTypeCount )
      ( Vulkan.unsafePtr memoryProperties
          `Foreign.plusPtr` Vulkan.fieldOffset @"memoryTypes" @Vulkan.VkPhysicalDeviceMemoryProperties
      )

  let
    requiredFlags =
      Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT

    possibleMemoryTypeIndices = do
      ( i, memoryType ) <-
        zip [ 0 .. ] memoryTypes

      guard
        ( testBit
            ( Vulkan.getField @"memoryTypeBits" requirements )
            ( fromIntegral i )
        )

      guard
        ( Vulkan.getField @"propertyFlags" memoryType .&. requiredFlags > 0 )

      return i

  memoryTypeIndex <-
      case possibleMemoryTypeIndices of
        [] ->
          fail "No possible memory types"

        ( i : _ ) ->
          return i

  let
    allocateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"allocationSize" ( Vulkan.getField @"size" requirements )
        &* Vulkan.set @"memoryTypeIndex" memoryTypeIndex
        )

  allocaAndPeek
    ( Vulkan.vkAllocateMemory
        device
        ( Vulkan.unsafePtr allocateInfo )
        Vulkan.VK_NULL_HANDLE
        >=> throwVkResult
    )
