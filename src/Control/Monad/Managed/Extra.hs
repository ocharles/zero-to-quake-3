{-# language RankNTypes #-}

module Control.Monad.Managed.Extra ( managed, manageBracket ) where

-- base
import Control.Exception ( bracket )

-- managed
import Control.Monad.Managed ( MonadManaged )
import qualified Control.Monad.Managed


managed :: MonadManaged m => (forall r. (a -> IO r) -> IO r) -> m a
managed f =
  Control.Monad.Managed.using ( Control.Monad.Managed.managed f )


manageBracket :: MonadManaged m => IO a -> (a -> IO b) -> m a
manageBracket create destroy =
  managed ( bracket create destroy )
