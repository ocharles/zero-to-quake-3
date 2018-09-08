module Foreign.Marshal.Extra ( allocaAndPeek, allocaAndPeekArray ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign
import qualified Foreign.Marshal


allocaAndPeek
  :: ( Foreign.Storable a, MonadIO m )
  => ( Foreign.Ptr a -> IO () ) -> m a
allocaAndPeek f =
  liftIO $
  Foreign.Marshal.alloca
    ( \ptr ->
        f ptr *> Foreign.peek ptr
    )


allocaAndPeekArray
  :: Foreign.Storable a
  => Int -> ( Foreign.Ptr a -> IO () ) -> IO [ a ]
allocaAndPeekArray n f =
  Foreign.Marshal.allocaArray
    n
    ( \ptr ->
        f ptr *> Foreign.Marshal.peekArray n ptr
    )
