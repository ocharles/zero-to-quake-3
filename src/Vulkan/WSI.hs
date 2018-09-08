{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.WSI
  ( acquireNextImage
  , assertSurfacePresentable
  , createSwapchain
  , determineSwapchainFormat
  , getSwapchainImages
  , present
  ) where

-- base
import Control.Monad ( (>=>), unless )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.List hiding ( transpose )
import Data.Ord ( Down(..) )
import qualified Foreign

-- managed
import Control.Monad.Managed ( MonadManaged )

-- sdl2
import qualified SDL.Video.Vulkan

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Marshal.Extra ( allocaAndPeek )
import Foreign.Vulkan ( fetchAll, managedVulkanResource, throwVkResult )


createSwapchain
  :: ( MonadIO m, MonadManaged m )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> Vulkan.VkFormat
  -> Vulkan.VkColorSpaceKHR
  -> m ( Vulkan.VkSwapchainKHR, Vulkan.VkExtent2D )
createSwapchain physicalDevice device surface format colorSpace = do
  surfaceCapabilities <- liftIO $
    allocaAndPeek
      ( Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR
          physicalDevice
          ( Vulkan.VkPtr surface )
          >=> throwVkResult
      )

  let
    minImageCount =
      Vulkan.getField @"minImageCount" surfaceCapabilities

    currentExtent =
      Vulkan.getField @"currentExtent" surfaceCapabilities

    currentTransform =
      Vulkan.getField @"currentTransform" surfaceCapabilities

    swapchainCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
        &* Vulkan.set @"pNext" Foreign.nullPtr
        &* Vulkan.set @"surface" ( Vulkan.VkPtr surface )
        &* Vulkan.set @"minImageCount" minImageCount
        &* Vulkan.set @"imageFormat" format
        &* Vulkan.set @"imageColorSpace" colorSpace
        &* Vulkan.set @"imageExtent" currentExtent
        &* Vulkan.set @"imageArrayLayers" 1
        &* Vulkan.set @"imageUsage" Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        &* Vulkan.set @"imageSharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
        &* Vulkan.set @"queueFamilyIndexCount" 0
        &* Vulkan.set @"pQueueFamilyIndices" Vulkan.vkNullPtr
        &* Vulkan.set @"preTransform" currentTransform
        &* Vulkan.set @"compositeAlpha" Vulkan.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        &* Vulkan.set @"presentMode" Vulkan.VK_PRESENT_MODE_FIFO_KHR
        &* Vulkan.set @"clipped" Vulkan.VK_TRUE
        &* Vulkan.set @"oldSwapchain" Vulkan.VK_NULL_HANDLE
        )

  swapchain <-
    managedVulkanResource
      ( Vulkan.vkCreateSwapchainKHR
          device
          ( Vulkan.unsafePtr swapchainCreateInfo )
      )
      ( Vulkan.vkDestroySwapchainKHR device )

  return ( swapchain, currentExtent )


getSwapchainImages
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkSwapchainKHR
  -> m [ Vulkan.VkImage ]
getSwapchainImages device swapchain = liftIO $
  fetchAll
    ( \imageCountPtr imagesPtr ->
        Vulkan.vkGetSwapchainImagesKHR
          device
          swapchain
          imageCountPtr
          imagesPtr
          >>= throwVkResult
    )


determineSwapchainFormat
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m ( Vulkan.VkFormat, Vulkan.VkColorSpaceKHR )
determineSwapchainFormat physicalDevice surface = liftIO $ do
  surfaceFormats <-
    fetchAll
      ( \surfaceFormatCountPtr surfaceFormatsPtr ->
        Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR
          physicalDevice
          ( Vulkan.VkPtr surface )
          surfaceFormatCountPtr
          surfaceFormatsPtr
          >>= throwVkResult
      )

  let
    scoredFormats = do
      surfaceFormat <-
        surfaceFormats

      let
        format =
          Vulkan.getField @"format" surfaceFormat

        colorSpace =
          Vulkan.getField @"colorSpace" surfaceFormat

        score :: Int
        score =
          sum
            [ if format == Vulkan.VK_FORMAT_B8G8R8A8_UNORM then 1 else 0
            , if colorSpace == Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR then 1 else 0
            ]

      return ( Down score, format, colorSpace )

  case sortOn ( \( score, _, _ ) -> score ) scoredFormats of
    [] ->
      fail "No formats found"

    ( _score, format, colorSpace ) : _ ->
      return ( format, colorSpace )


acquireNextImage
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkSwapchainKHR
  -> Vulkan.VkSemaphore
  -> m Int
acquireNextImage device swapchain signal = liftIO $
  fmap
    fromIntegral
    ( allocaAndPeek
        ( Vulkan.vkAcquireNextImageKHR
            device
            swapchain
            maxBound
            signal
            Vulkan.VK_NULL_HANDLE
            >=> throwVkResult
        )
    )


present
  :: MonadIO m
  => Vulkan.VkQueue
  -> Vulkan.VkSwapchainKHR
  -> Int
  -> Vulkan.VkSemaphore
  -> m ()
present queue swapchain imageIndex wait = liftIO $ do
  let
    presentInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"waitSemaphoreCount" 1
        &* Vulkan.setListRef @"pWaitSemaphores" [ wait ]
        &* Vulkan.set @"swapchainCount" 1
        &* Vulkan.setListRef @"pSwapchains" [ swapchain ]
        &* Vulkan.setListRef @"pImageIndices" [ fromIntegral imageIndex ]
        &* Vulkan.set @"pResults" Vulkan.vkNullPtr
        )

  Vulkan.vkQueuePresentKHR queue ( Vulkan.unsafePtr presentInfo )
    >>= throwVkResult


assertSurfacePresentable
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> Int
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m ()
assertSurfacePresentable physicalDevice queueFamilyIndex surface = liftIO $ do
  bool <-
    allocaAndPeek
      ( Vulkan.vkGetPhysicalDeviceSurfaceSupportKHR
          physicalDevice
          ( fromIntegral queueFamilyIndex )
          ( Vulkan.VkPtr surface )
          >=> throwVkResult
      )

  unless
    ( bool == Vulkan.VK_TRUE )
    ( fail "Unsupported surface" )
