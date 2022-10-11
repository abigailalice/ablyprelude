
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
    , DTL._Text
    , groupOn
    , groupOn'
    ) where

import Control.Lens (view)
import Data.Maybe as X (mapMaybe)

import Prelude hiding (readFile, writeFile)
import Data.Function

import qualified Data.Aeson as DA
import           Data.Serialize as X (Serialize)
import           Data.DList as X (DList)
import           Data.List.NonEmpty as X (NonEmpty)
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

import qualified Control.Lens as L
import qualified Data.Function as DF
import qualified Data.List as DL
import qualified Data.Text.Strict.Lens
import qualified Data.Map as DM
import qualified Data.Text.Lens as DTL
import qualified Data.Text.Lazy.Lens
import qualified Data.Serialize as DS

import qualified System.FilePath.Lens as SFL
import qualified System.Directory as SD
import Data.Foldable

type LText = Data.Text.Lazy.Text
type Bytes = Data.ByteString.ByteString
type LBytes = Data.ByteString.Lazy.ByteString

class IsUTF8 bytes text | bytes -> text , text -> bytes where
    _UTF8 :: L.Prism' bytes text
instance IsUTF8 LBytes LText where
    _UTF8 = Data.Text.Lazy.Lens.utf8
instance IsUTF8 Bytes Text where
    _UTF8 = Data.Text.Strict.Lens.utf8

_Serialize :: (DS.Serialize a) => L.Prism' Bytes a
_Serialize = L.prism' DS.encode (L.preview L._Right . DS.decode)

_Aeson :: (DA.FromJSON a, DA.ToJSON a) => L.Prism' Bytes a
_Aeson = L.prism' (Data.ByteString.Lazy.toStrict . DA.encode) DA.decodeStrict'

_Value :: (DA.FromJSON a, DA.ToJSON a) => L.Prism' DA.Value a
_Value = L.prism' DA.toJSON (fromResult . DA.fromJSON)
  where
    fromResult (DA.Success a) = Just a
    fromResult _ = Nothing

readFile :: (Serialize a) => FilePath -> IO (Maybe a)
readFile fp = fmap (L.preview _Serialize) (Data.ByteString.readFile fp)

writeFile :: (Serialize a) => FilePath -> a -> IO ()
writeFile fp obj = do
    SD.createDirectoryIfMissing True (view SFL.directory fp)
    Data.ByteString.writeFile fp (L.review _Serialize obj)

groupOn :: forall a b. (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f = DL.groupBy ((==) `DF.on` f) . DL.sortBy (compare `DF.on` f)

groupOn' :: forall a b. (Ord b) => (a -> b) -> [a] -> Map b [a]
groupOn' f xs = xs
    & fmap (\x -> (f x, pure x))
    & DM.fromListWith (<>)
    & fmap (toList @DList)

