{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.Pipeline
  ( createPipeline
  , bindPipeline
  ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bits
import qualified Foreign
import qualified Foreign.C

-- bytestring
import qualified Data.ByteString

-- linear
import Linear ( V3(..) )

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource )

-- TODO Remove this coupling
import Quake3.Vertex


createPipeline
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkExtent2D
  -> Vulkan.VkDescriptorSetLayout
  -> m ( Vulkan.VkPipeline, Vulkan.VkPipelineLayout )
createPipeline device renderPass extent layout0 = do
  pipelineLayout <-
    let
      pipelineLayoutCreateInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" 0
          &* Vulkan.set @"setLayoutCount" 1
          &* Vulkan.setListRef @"pSetLayouts" [ layout0 ]
          &* Vulkan.set @"pPushConstantRanges" Vulkan.VK_NULL
          )

    in
    managedVulkanResource
      ( Vulkan.vkCreatePipelineLayout
          device
          ( Vulkan.unsafePtr pipelineLayoutCreateInfo )
      )
      ( Vulkan.vkDestroyPipelineLayout device )

  -- TODO Remove hard coding
  vertexShader <-
    loadShader device "/home/ollie/work/zero-to-quake3/vert.spv"

  fragmentShader <-
    loadShader device "/home/ollie/work/zero-to-quake3/frag.spv"

  let
    rasterizationCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"depthClampEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"rasterizerDiscardEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"polygonMode" Vulkan.VK_POLYGON_MODE_FILL
        &* Vulkan.set @"lineWidth" 1
        &* Vulkan.set @"depthBiasEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"depthBiasSlopeFactor" 0
        &* Vulkan.set @"depthBiasClamp" 0
        &* Vulkan.set @"depthBiasConstantFactor" 0
        &* Vulkan.set @"frontFace" Vulkan.VK_FRONT_FACE_CLOCKWISE
        )

    vertexShaderStage =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.setStrRef @"pName" "main"
        &* Vulkan.set @"module" vertexShader
        &* Vulkan.set @"stage" Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        )

    fragmentShaderStage =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.setStrRef @"pName" "main"
        &* Vulkan.set @"module" fragmentShader
        &* Vulkan.set @"stage" Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
        )

    vertexBindingDescription =
      Vulkan.createVk
        (  Vulkan.set @"binding" 0
        &* Vulkan.set @"stride" ( fromIntegral ( Foreign.sizeOf ( undefined :: Vertex ) ) )
        &* Vulkan.set @"inputRate" Vulkan.VK_VERTEX_INPUT_RATE_VERTEX
        )

    positionAttributeDescription =
      Vulkan.createVk
        (  Vulkan.set @"location" 0
        &* Vulkan.set @"binding" 0
        &* Vulkan.set @"format" Vulkan.VK_FORMAT_R32G32B32_SFLOAT
        &* Vulkan.set @"offset" 0
        )

    colorAttributeDescription =
      Vulkan.createVk
        (  Vulkan.set @"location" 1
        &* Vulkan.set @"binding" 0
        &* Vulkan.set @"format" Vulkan.VK_FORMAT_R8G8B8A8_UINT
        &* Vulkan.set
             @"offset"
             ( fromIntegral
                 ( let
                     v :: Vertex
                     v =
                       undefined

                   in
                   sum
                     [ Foreign.sizeOf ( vPos v )
                     , Foreign.sizeOf ( vSurfaceUV v )
                     , Foreign.sizeOf ( vLightmapUV v )
                     , Foreign.sizeOf ( vNormal v )
                     ]
                 )
             )
        )

    vertexInputState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"vertexBindingDescriptionCount" 1
        &* Vulkan.setListRef @"pVertexBindingDescriptions" [ vertexBindingDescription ]
        &* Vulkan.set @"vertexAttributeDescriptionCount" 2
        &* Vulkan.setListRef
             @"pVertexAttributeDescriptions"
             [ positionAttributeDescription
             , colorAttributeDescription
             ]
        )

    assemblyStateCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"topology" Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        &* Vulkan.set @"primitiveRestartEnable" Vulkan.VK_FALSE
        )

    viewport =
      Vulkan.createVk
        (  Vulkan.set @"x" 0
        &* Vulkan.set @"y" 0
        &* Vulkan.set @"width" ( fromIntegral ( Vulkan.getField @"width" extent ) )
        &* Vulkan.set @"height" ( fromIntegral ( Vulkan.getField @"height" extent ) )
        &* Vulkan.set @"minDepth" 0
        &* Vulkan.set @"maxDepth" 1
        )

    scissor =
      let
        offset =
          Vulkan.createVk
            (  Vulkan.set @"x" 0
            &* Vulkan.set @"y" 0
            )

      in
      Vulkan.createVk
        (  Vulkan.set @"offset" offset
        &* Vulkan.set @"extent" extent
        )

    viewportState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"viewportCount" 1
        &* Vulkan.set @"scissorCount" 1
        &* Vulkan.setListRef @"pViewports" [ viewport ]
        &* Vulkan.setListRef @"pScissors" [ scissor ]
        )

    multisampleState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
        &* Vulkan.set @"minSampleShading" 1
        &* Vulkan.set @"rasterizationSamples" Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        )

    attachmentState =
      Vulkan.createVk
        ( Vulkan.set @"blendEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"alphaBlendOp" Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set @"colorBlendOp" Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set
             @"colorWriteMask"
             ( Vulkan.VK_COLOR_COMPONENT_R_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_G_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_B_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_A_BIT
             )
        )

    colorBlendState =
      Vulkan.createVk
        ( Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
        &* Vulkan.setAt @"blendConstants" @0 0
        &* Vulkan.setAt @"blendConstants" @1 0
        &* Vulkan.setAt @"blendConstants" @2 0
        &* Vulkan.setAt @"blendConstants" @3 0
        &* Vulkan.set @"attachmentCount" 1
        &* Vulkan.setListRef @"pAttachments" [ attachmentState ]
        &* Vulkan.set @"logicOp" Vulkan.VK_LOGIC_OP_COPY
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        )

    nullStencilOp =
      Vulkan.createVk
        (  Vulkan.set @"reference" 0
        &* Vulkan.set @"writeMask" 0
        &* Vulkan.set @"compareMask" 0
        &* Vulkan.set @"compareOp" Vulkan.VK_COMPARE_OP_EQUAL
        &* Vulkan.set @"depthFailOp" Vulkan.VK_STENCIL_OP_KEEP
        &* Vulkan.set @"passOp" Vulkan.VK_STENCIL_OP_KEEP
        &* Vulkan.set @"failOp" Vulkan.VK_STENCIL_OP_KEEP
        )

    depthStencilState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"depthTestEnable" Vulkan.VK_TRUE
        &* Vulkan.set @"depthWriteEnable" Vulkan.VK_TRUE
        &* Vulkan.set @"depthCompareOp" Vulkan.VK_COMPARE_OP_LESS_OR_EQUAL
        &* Vulkan.set @"maxDepthBounds" 1
        &* Vulkan.set @"minDepthBounds" 0
        &* Vulkan.set @"stencilTestEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"front" nullStencilOp
        &* Vulkan.set @"back" nullStencilOp
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"stageCount" 2
        &* Vulkan.setListRef @"pStages" [ vertexShaderStage, fragmentShaderStage ]
        &* Vulkan.setVkRef @"pVertexInputState" vertexInputState
        &* Vulkan.set @"basePipelineIndex" 0
        &* Vulkan.set @"subpass" 0
        &* Vulkan.set @"renderPass" renderPass
        &* Vulkan.set @"layout" pipelineLayout
        &* Vulkan.setVkRef @"pRasterizationState" rasterizationCreateInfo
        &* Vulkan.setVkRef @"pInputAssemblyState" assemblyStateCreateInfo
        &* Vulkan.setVkRef @"pViewportState" viewportState
        &* Vulkan.setVkRef @"pMultisampleState" multisampleState
        &* Vulkan.setVkRef @"pColorBlendState" colorBlendState
        &* Vulkan.setVkRef @"pDepthStencilState" depthStencilState
        )

  pipeline <-
    managedVulkanResource
      ( Vulkan.vkCreateGraphicsPipelines
          device
          Vulkan.vkNullPtr
          1
          ( Vulkan.unsafePtr createInfo )
      )
      ( Vulkan.vkDestroyPipeline device )

  return ( pipeline, pipelineLayout )


-- TODO This module shouldn't be responsible for loading shaders!

loadShader :: MonadManaged m => Vulkan.VkDevice -> FilePath -> m Vulkan.VkShaderModule
loadShader device srcFile = do
  bytes <-
    liftIO ( Data.ByteString.readFile srcFile )

  managedVulkanResource
    ( \a b ->
        Data.ByteString.useAsCStringLen bytes $ \( bytesPtr, len ) ->
          let
            createInfo =
              Vulkan.createVk
                (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
                &* Vulkan.set @"pNext" Vulkan.VK_NULL
                &* Vulkan.set @"flags" 0
                &* Vulkan.set @"pCode" ( Foreign.castPtr bytesPtr )
                &* Vulkan.set @"codeSize" ( fromIntegral len )
                )

          in
          Vulkan.vkCreateShaderModule device ( Vulkan.unsafePtr createInfo ) a b
    )
    ( Vulkan.vkDestroyShaderModule device )


bindPipeline :: MonadIO m => Vulkan.VkCommandBuffer -> Vulkan.VkPipeline -> m ()
bindPipeline commandBuffer graphicsPipeline =
  liftIO
    ( Vulkan.vkCmdBindPipeline
        commandBuffer
        Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
        graphicsPipeline
    )
