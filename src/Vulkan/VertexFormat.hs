{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeApplications #-}

module Vulkan.VertexFormat
  ( VertexFormat
  , parse

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
import Data.Functor.Product ( Product(..) )
import Data.Word ( Word8 )
import qualified Foreign.C

-- binary
import qualified Data.Binary.Get

-- contravarient
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible ( Divisible )

-- deriving-compat
import Data.Deriving.Via

-- linear
import Linear ( V2(..), V3(..), V4(..) )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan


newtype Component =
  Component { format :: Vulkan.VkFormat }


-- | 'VertexFormat's are functions from vertex types @v@ to their individual
-- components.
data VertexFormat v =
  VertexFormat
    { components :: [ Component ]
    , parse :: Data.Binary.Get.Get v
    }


instance Functor VertexFormat where
  fmap f ( VertexFormat cs p ) =
    VertexFormat cs ( fmap f p )


instance Applicative VertexFormat where
  pure a =
    VertexFormat [] ( pure a )

  VertexFormat c1 f <*> VertexFormat c2 x =
    VertexFormat ( c1 ++ c2 ) ( f <*> x )


mkVertexFormat :: Vulkan.VkFormat -> Data.Binary.Get.Get v -> VertexFormat v
mkVertexFormat format parse =
  VertexFormat
    { components = [ Component format ]
    , parse = parse
    }


v2_32sfloat :: VertexFormat ( V2 Foreign.C.CFloat )
v2_32sfloat =
  mkVertexFormat
    Vulkan.VK_FORMAT_R32G32_SFLOAT
    ( fmap Foreign.C.CFloat <$> getV2 Data.Binary.Get.getFloatle )


v3_32sfloat :: VertexFormat ( V3 Foreign.C.CFloat )
v3_32sfloat =
  mkVertexFormat
    Vulkan.VK_FORMAT_R32G32B32_SFLOAT
    ( fmap Foreign.C.CFloat <$> getV3 Data.Binary.Get.getFloatle )


v4_8uint :: VertexFormat ( V4 Word8 )
v4_8uint =
  mkVertexFormat
    Vulkan.VK_FORMAT_R8G8B8A8_UINT
    ( getV4 Data.Binary.Get.getWord8 )


strideSize :: VertexFormat v -> Int
strideSize ( VertexFormat components _ ) =
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
attributeDescriptions binding ( VertexFormat components _ ) =
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


getV2 :: Data.Binary.Get.Get a -> Data.Binary.Get.Get ( V2 a )
getV2 c =
  V2 <$> c <*> c


getV3 :: Data.Binary.Get.Get a -> Data.Binary.Get.Get ( V3 a )
getV3 c =
  V3 <$> c <*> c <*> c


getV4 :: Data.Binary.Get.Get a -> Data.Binary.Get.Get ( V4 a )
getV4 c =
  V4 <$> c <*> c <*> c <*> c
