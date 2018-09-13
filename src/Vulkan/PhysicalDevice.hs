{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language NoMonomorphismRestriction #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Vulkan.PhysicalDevice
  ( createPhysicalDevice
  , findOptimalDepthFormat
  , findQueueFamilyIndex
  ) where

-- base
import Control.Monad ( guard )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Traversable ( for )
import Data.Bits ( (.&.) )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( fetchAll, throwVkResult )
import Foreign.Marshal.Extra ( allocaAndPeek )


createPhysicalDevice
  :: MonadIO m
  => Vulkan.VkInstance -> m Vulkan.VkPhysicalDevice
createPhysicalDevice vk = liftIO $ do
  physicalDevices <-
    fetchAll
      ( \nPtr ptr ->
          Vulkan.vkEnumeratePhysicalDevices vk nPtr ptr
            >>= throwVkResult
      )

  typedDevices <-
    for physicalDevices $ \physicalDevice -> do
      properties <-
        allocaAndPeek
          ( Vulkan.vkGetPhysicalDeviceProperties physicalDevice )

      return ( physicalDevice, Vulkan.getField @"deviceType" properties )

  case filter isSuitableDevice typedDevices of
    [] ->
      fail "Could not find a suitable physical device"

    ( ( d, _deviceType ) : _ds ) ->
      return d

  where

    isSuitableDevice ( _, deviceType ) =
      deviceType
        `elem`
          [ Vulkan.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
          , Vulkan.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
          ]


findQueueFamilyIndex :: MonadIO m => Vulkan.VkPhysicalDevice -> m Int
findQueueFamilyIndex physicalDevice = liftIO $ do
  queueFamilies <-
    fetchAll
      ( \nQueueFamiliesPtr queueFamiliesPtr ->
          Vulkan.vkGetPhysicalDeviceQueueFamilyProperties
            physicalDevice
            nQueueFamiliesPtr
            queueFamiliesPtr
      )

  let
    capableFamilyIndices = do
      ( i, queueFamily ) <- zip [0 ..] queueFamilies

      let
        flags =
          Vulkan.getField @"queueFlags" queueFamily

      guard ( flags .&. Vulkan.VK_QUEUE_GRAPHICS_BIT > 0 )

      return i

  case capableFamilyIndices of
    [] ->
      fail "No queue family has sufficient capabilities"

    ( i : _ ) ->
      return i


findOptimalDepthFormat
  :: MonadIO m
  => Vulkan.VkPhysicalDevice -> m Vulkan.VkFormat
findOptimalDepthFormat physicalDevice =
  findBest
    [ Vulkan.VK_FORMAT_D32_SFLOAT
    , Vulkan.VK_FORMAT_D32_SFLOAT_S8_UINT
    , Vulkan.VK_FORMAT_D24_UNORM_S8_UINT
    ]

  where

    findBest [] =
      fail "Could not find a valid depth format"

    findBest ( x : xs ) = do
      properties <-
        allocaAndPeek
          ( Vulkan.vkGetPhysicalDeviceFormatProperties physicalDevice x )

      if supportsDepthAttachment properties
        then return x
        else findBest xs

    supportsDepthAttachment :: Vulkan.VkFormatProperties -> Bool
    supportsDepthAttachment props =
      testBitmask
        ( Vulkan.getField @"optimalTilingFeatures" props )
        Vulkan.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT

    testBitmask bitmask featureFlag =
      bitmask .&. featureFlag > 0

