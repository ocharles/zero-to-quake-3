{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeApplications #-}

module Vulkan.VertexFormat
  ( VertexFormat

    -- * Inspecting @VertexFormat@s
  , attributeDescriptions
  , strideSize

    -- * Constructing @VertexFormat@s
  , v2_32sfloat
  , v3_32sfloat
  , v4_8uint
  ) where

-- base
import Control.Applicative ( Const(..), ZipList(..) )
import Data.Word ( Word8 )
import qualified Foreign.C

-- contravarient
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible ( Divisible )

-- linear
import Linear ( V2, V3, V4 )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan


newtype Component =
  Component { format :: Vulkan.VkFormat }


-- | 'VertexFormat's are functions from vertex types @v@ to their individual
-- components.
newtype VertexFormat v =
  VertexFormat ( Const [ Component ] v )
  deriving ( Contravariant, Divisible )


mkVertexFormat :: Vulkan.VkFormat -> VertexFormat v
mkVertexFormat =
  VertexFormat . Const . pure . Component


v2_32sfloat :: VertexFormat ( V2 Foreign.C.CFloat )
v2_32sfloat =
  mkVertexFormat Vulkan.VK_FORMAT_R32G32_SFLOAT


v3_32sfloat :: VertexFormat ( V3 Foreign.C.CFloat )
v3_32sfloat =
  mkVertexFormat Vulkan.VK_FORMAT_R32G32B32_SFLOAT


v4_8uint :: VertexFormat ( V4 Word8 )
v4_8uint =
  mkVertexFormat Vulkan.VK_FORMAT_R8G8B8A8_UINT


strideSize :: VertexFormat v -> Int
strideSize ( VertexFormat ( Const components ) ) =
  sum ( map componentSize components )


componentSize :: Component -> Int
componentSize c =
  case format c of
    Vulkan.VK_FORMAT_R32G32_SFLOAT ->
      2 * 4

    Vulkan.VK_FORMAT_R32G32B32_SFLOAT ->
      3 * 4

    Vulkan.VK_FORMAT_R8G8B8A8_UINT ->
      4 * 1


attributeDescriptions
  :: Int
  -> VertexFormat v
  -> [ Vulkan.VkVertexInputAttributeDescription ]
attributeDescriptions binding ( VertexFormat ( Const components ) ) =
  getZipList
    ( toAttributeDescription
        <$> ZipList components
        <*> ZipList ( scanl (+) 0 ( map componentSize components ) )
        <*> ZipList [ 0.. ]
    )

  where

    toAttributeDescription
      :: Component
      -> Int
      -> Int
      -> Vulkan.VkVertexInputAttributeDescription
    toAttributeDescription component offset location =
      Vulkan.createVk
        (  Vulkan.set @"location" ( fromIntegral location )
        &* Vulkan.set @"binding" ( fromIntegral binding )
        &* Vulkan.set @"format" ( format component )
        &* Vulkan.set @"offset" ( fromIntegral offset )
        )
