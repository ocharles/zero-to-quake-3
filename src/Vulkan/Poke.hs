{-# language RecordWildCards #-}

module Vulkan.Poke
  ( Poke
  , pokePtr
  , size
  , lazyBytestring
  , storable
  , list
  ) where

-- base
import Control.Monad ( zipWithM_ )
import Data.Foldable ( traverse_ )
import qualified Foreign
import qualified Foreign.Marshal.Utils

-- bytestring
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Unsafe

-- contravariant
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible ( Divisible(..) )


data Poke a = Poke
  { pokePtr :: Foreign.Ptr a -> a -> IO ()
  , size :: a -> Int
  }


instance Contravariant Poke where
  contramap f Poke{..} =
    Poke
      { pokePtr =
          \ptr a ->
            pokePtr ( Foreign.castPtr ptr ) ( f a )
      , size =
          \a -> size ( f a )
      }


instance Divisible Poke where
  conquer =
    Poke
      { pokePtr = const ( const ( return () ) )
      , size = const 0
      }

  divide split fb fc =
    Poke
      { pokePtr =
          \ptr a ->
            let
              ( b, c ) =
                split a

            in
            sequence_
              [ pokePtr fb ( Foreign.castPtr ptr ) b
              , pokePtr fc ( Foreign.castPtr ptr `Foreign.plusPtr` size fb b ) c
              ]
      , size =
          \a ->
            let
              ( b, c ) =
                split a

            in
            sum
              [ size fb b
              , size fc c
              ]
      }


lazyBytestring :: Poke Data.ByteString.Lazy.ByteString
lazyBytestring =
  Poke
    { pokePtr =
        \dst a ->
            Data.ByteString.Unsafe.unsafeUseAsCString
              ( Data.ByteString.Lazy.toStrict a )
              ( \src ->
                  Foreign.Marshal.Utils.copyBytes
                    ( Foreign.castPtr dst )
                    src
                    ( fromIntegral ( Data.ByteString.Lazy.length a ) )
              )
    , size =
        fromIntegral . Data.ByteString.Lazy.length
    }


list :: Poke a -> Poke [ a ]
list Poke{..} =
  Poke
    { pokePtr =
        \ptr xs -> do
          let
            offsets =
              take
                ( length xs )
                ( iterate
                    ( `Foreign.plusPtr` size ( head xs ) )
                    ( Foreign.castPtr ptr )
                )

          zipWithM_ pokePtr offsets xs
    , size =
        sum . map size
    }


storable :: Foreign.Storable a => Poke a
storable =
  Poke
    { pokePtr =
        Foreign.poke . Foreign.castPtr
    , size =
        Foreign.sizeOf
    }
