{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.DescriptorSet
  ( allocateDescriptorSet
  , bindDescriptorSets
  , createDescriptorPool
  , createDescriptorSetLayout
  , updateDescriptorSet
  ) where

-- base
import Data.Coerce ( coerce )
import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign.Marshal

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Control.Monad.Managed.Extra ( manageBracket )
import Foreign.Marshal.Extra ( allocaAndPeek )
import Foreign.Vulkan ( managedVulkanResource, throwVkResult )
import qualified Vulkan.Buffer
import Vulkan.Buffer.UniformBuffer ( UniformBuffer(..) )


createDescriptorSetLayout
  :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkDescriptorSetLayout
createDescriptorSetLayout device = do
  let
    binding =
      Vulkan.createVk
        (  Vulkan.set @"binding" 0
        &* Vulkan.set @"descriptorType" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        &* Vulkan.set @"descriptorCount" 1
        &* Vulkan.set @"stageFlags" Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        &* Vulkan.set @"pImmutableSamplers" Vulkan.VK_NULL
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"bindingCount" 1
        &* Vulkan.setListRef @"pBindings" [ binding ]
        )

  managedVulkanResource
    ( Vulkan.vkCreateDescriptorSetLayout
        device
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyDescriptorSetLayout device )


createDescriptorPool
  :: MonadManaged m
  => Vulkan.VkDevice -> m Vulkan.VkDescriptorPool
createDescriptorPool device =
  let
    poolSize0 =
      Vulkan.createVk
        (  Vulkan.set @"type" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        &* Vulkan.set @"descriptorCount" 1
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"poolSizeCount" 1
        &* Vulkan.setListRef @"pPoolSizes" [ poolSize0 ]
        &* Vulkan.set @"maxSets" 1
        )

  in
  managedVulkanResource
    ( Vulkan.vkCreateDescriptorPool device ( Vulkan.unsafePtr createInfo ) )
    ( Vulkan.vkDestroyDescriptorPool device )


allocateDescriptorSet
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkDescriptorPool
  -> Vulkan.VkDescriptorSetLayout
  -> m Vulkan.VkDescriptorSet
allocateDescriptorSet dev descriptorPool layout0 = do
  let
    allocateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"descriptorPool" descriptorPool
        &* Vulkan.set @"descriptorSetCount" 1
        &* Vulkan.setListRef @"pSetLayouts" [ layout0 ]
        )

  manageBracket
    ( allocaAndPeek
        ( Vulkan.vkAllocateDescriptorSets
            dev
            ( Vulkan.unsafePtr allocateInfo )
            >=> throwVkResult
        )
    )
    ( \a ->
        Foreign.Marshal.withArray [ a ]
          ( Vulkan.vkFreeDescriptorSets dev descriptorPool 1 )
    )


updateDescriptorSet
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkDescriptorSet
  -> UniformBuffer
  -> m ()
updateDescriptorSet device descriptorSet buffer = do
  let
    bufferInfo =
      Vulkan.createVk
        (  Vulkan.set @"buffer" ( Vulkan.Buffer.buffer ( coerce buffer ) )
        &* Vulkan.set @"offset" 0
        &* Vulkan.set @"range" ( fromIntegral Vulkan.VK_WHOLE_SIZE )
        )

    writeUpdate0 =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"dstSet" descriptorSet
        &* Vulkan.set @"dstBinding" 0
        &* Vulkan.set @"descriptorType" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        &* Vulkan.set @"pTexelBufferView" Vulkan.VK_NULL
        &* Vulkan.set @"pImageInfo" Vulkan.VK_NULL
        &* Vulkan.setListRef @"pBufferInfo" [ bufferInfo ]
        &* Vulkan.set @"descriptorCount" 1
        &* Vulkan.set @"dstArrayElement" 0
        )

  liftIO $
    Foreign.Marshal.withArray [ writeUpdate0 ] $ \writeUpdatesPtr ->
      Vulkan.vkUpdateDescriptorSets device 1 writeUpdatesPtr 0 Vulkan.vkNullPtr


bindDescriptorSets
  :: MonadIO m
  => Vulkan.VkCommandBuffer
  -> Vulkan.VkPipelineLayout
  -> [ Vulkan.VkDescriptorSet ]
  -> m ()
bindDescriptorSets commandBuffer pipelineLayout descriptorSets =
  liftIO
    ( Foreign.Marshal.withArray
        descriptorSets
        ( \descriptorSetsPtr ->
            Vulkan.vkCmdBindDescriptorSets
              commandBuffer
              Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
              pipelineLayout
              0
              ( fromIntegral ( length descriptorSets ) )
              descriptorSetsPtr
              0
              Vulkan.vkNullPtr
        )
    )
