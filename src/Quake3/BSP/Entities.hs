{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Quake3.BSP.Entities
  ( EntityProperties
  , getInt
  , getIntV3
  , getText
  , parseEntityDefinitions
  ) where

-- base
import Control.Applicative ( liftA2, many )
import Data.Void ( Void )
import Text.Read ( readMaybe )

-- containers
import qualified Data.Map.Strict as Map

-- linear
import Linear ( V3(..) )

-- megaparsec
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Megaparsec hiding ( space )

-- text
import qualified Data.Text as StrictText


newtype EntityProperties =
  EntityProperties ( Map.Map StrictText.Text StrictText.Text )
  deriving ( Show )


getInt :: StrictText.Text -> EntityProperties -> Maybe Int
getInt key ( EntityProperties m ) =
  fmap StrictText.unpack ( Map.lookup key m )
    >>= readMaybe


getIntV3 :: StrictText.Text -> EntityProperties -> Maybe ( V3 Int )
getIntV3 key ( EntityProperties m ) = do
  [ x, z, y ] <-
    fmap ( words . StrictText.unpack ) ( Map.lookup key m )

  traverse readMaybe ( V3 x y z )


getText :: StrictText.Text -> EntityProperties -> Maybe StrictText.Text
getText key ( EntityProperties m ) =
  Map.lookup key m


parseEntityDefinitions
  :: StrictText.Text
  -> Either ( Megaparsec.ParseError Char Void ) [ EntityProperties ]
parseEntityDefinitions =
  Megaparsec.parse parser "(entities)"


parser
  :: Megaparsec.MonadParsec e StrictText.Text m
  => m [ EntityProperties ]
parser =
  many entityDefinition <* Megaparsec.eof


entityDefinition
  :: Megaparsec.MonadParsec e StrictText.Text m
  => m EntityProperties
entityDefinition =
  EntityProperties
    <$>
      Megaparsec.between
        ( symbol "{" )
        ( symbol "}" )
        ( fmap
            Map.fromList
            ( Megaparsec.many ( liftA2 (,) quotedString quotedString ) )
        )


quotedString
  :: Megaparsec.MonadParsec e StrictText.Text m
  => m StrictText.Text
quotedString =
  symbol "\""
    *> fmap StrictText.pack ( many ( Megaparsec.satisfy ( /= '"' ) ) )
    <* symbol "\""


space
  :: Megaparsec.MonadParsec e StrictText.Text m
  => m ()
space =
  Megaparsec.space


symbol
  :: Megaparsec.MonadParsec e StrictText.Text m
  => StrictText.Text -> m StrictText.Text
symbol =
  Megaparsec.symbol space
