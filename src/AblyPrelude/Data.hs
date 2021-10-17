
{-# LANGUAGE ExplicitNamespaces, PackageImports #-}

module AblyPrelude.Data
    ( module X
    , type Bytes
    , type LBytes
    , type LText
    , readFile
    , writeFile
    , IsUTF8(..)
    , _Serialize

    , DA.FromJSON(..)
    , DA.ToJSON(..)
    , _Aeson
    , _Value
    ) where

import Data.Maybe as X (mapMaybe)

import Prelude hiding (readFile, writeFile)

import qualified Data.Aeson as DA
import           Data.Serialize as X (Serialize)
import           Data.DList as X (DList)
import           Data.HashSet as X (HashSet)
import           Data.HashMap.Strict as X (HashMap)
import           Data.IntMap as X (IntMap)
import           Data.IntSet as X (IntSet)
import           Data.Set as X (Set)
import           Data.Map as X (Map)
import           Data.Text as X (Text)
import qualified Data.Text.Lazy
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

import qualified Control.Lens as Lens
import qualified Data.Text.Strict.Lens
import qualified Data.Text.Lazy.Lens
import qualified Data.Serialize as Serialize

type LText = Data.Text.Lazy.Text
type Bytes = Data.ByteString.ByteString
type LBytes = Data.ByteString.Lazy.ByteString

class IsUTF8 bytes text | bytes -> text , text -> bytes where
    _UTF8 :: Lens.Prism' bytes text
instance IsUTF8 LBytes LText where
    _UTF8 = Data.Text.Lazy.Lens.utf8
instance IsUTF8 Bytes Text where
    _UTF8 = Data.Text.Strict.Lens.utf8

_Serialize :: (Serialize a) => Lens.Prism' Bytes a
_Serialize = Lens.prism' Serialize.encode (Lens.preview Lens._Right . Serialize.decode)

_Aeson :: (DA.FromJSON a, DA.ToJSON a) => Lens.Prism' Bytes a
_Aeson = Lens.prism' (Data.ByteString.Lazy.toStrict . DA.encode) DA.decodeStrict'

_Value :: forall a. (DA.FromJSON a, DA.ToJSON a) => Lens.Prism' DA.Value a
_Value = Lens.prism' DA.toJSON (fromResult . DA.fromJSON)
  where
    fromResult (DA.Success a) = Just a
    fromResult _ = Nothing

readFile :: (Serialize a) => FilePath -> IO (Maybe a)
readFile fp = fmap decode (Data.ByteString.readFile fp)
  where
    decode :: (Serialize a) => Bytes -> Maybe a
    decode bytes = Lens.preview Lens._Right (Serialize.decode bytes)

writeFile :: (Serialize a) => FilePath -> a -> IO ()
writeFile fp obj = Data.ByteString.writeFile fp (Serialize.encode obj)
