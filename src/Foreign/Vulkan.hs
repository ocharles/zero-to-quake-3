module Foreign.Vulkan ( fetchAll, managedVulkanResource, throwVkResult ) where

-- base
import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ( MonadIO )
import qualified Foreign
import qualified Foreign.Marshal

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- zero-to-quake-3
import Control.Monad.Managed.Extra ( manageBracket )
import Foreign.Marshal.Extra ( allocaAndPeek, allocaAndPeekArray )


managedVulkanResource
  :: ( MonadManaged m, Foreign.Storable x, Vulkan.VulkanPtr ptr )
  => ( ptr a -> Vulkan.Ptr x -> IO Vulkan.VkResult )
  -> ( x -> ptr a -> IO () )
  -> m x
managedVulkanResource create destroy =
  manageBracket
    ( allocaAndPeek
        ( create Vulkan.vkNullPtr >=> throwVkResult )
    )
    ( \a -> destroy a Vulkan.vkNullPtr )


throwVkResult :: MonadIO m => Vulkan.VkResult -> m ()
throwVkResult Vulkan.VK_SUCCESS =
  return ()
throwVkResult res =
  fail ( show res )


fetchAll
  :: ( Foreign.Storable a, Foreign.Storable b, Integral b )
  => ( Foreign.Ptr b -> Foreign.Ptr a -> IO () ) -> IO [a]
fetchAll f = do
  Foreign.Marshal.alloca $ \nPtr -> do
    f nPtr Foreign.nullPtr

    n <-
      fromIntegral <$> Foreign.peek nPtr

    allocaAndPeekArray n ( f nPtr )
