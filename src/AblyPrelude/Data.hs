
{-# LANGUAGE ExplicitNamespaces, PackageImports #-}

module AblyPrelude.Data
    ( module X
    , type Bytes
    , type LBytes
    , type LText
    , _Serialize
    , readFile
    , writeFile
    ) where

import Prelude hiding (readFile, writeFile)

import           "cereal" Data.Serialize as X (Serialize)
import           "dlist" Data.DList as X (DList)
import           "unordered-containers" Data.HashSet as X (HashSet)
--import           "unordered-containers" Data.HashMap as X (HashMap)
import           "containers" Data.IntMap as X (IntMap)
import           "containers" Data.IntSet as X (IntSet)
import           "containers" Data.Set as X (Set)
import           "containers" Data.Map as X (Map)
import           "text" Data.Text as X (Text)
import qualified "text" Data.Text.Lazy
import qualified "bytestring" Data.ByteString
import qualified "bytestring" Data.ByteString.Lazy

import qualified Control.Lens as Lens
import qualified Data.Serialize as Serialize

type LText = Data.Text.Lazy.Text
type Bytes = Data.ByteString.ByteString
type LBytes = Data.ByteString.Lazy.ByteString

_Serialize :: (Serialize a) => Lens.Prism' Bytes a
_Serialize = Lens.prism' Serialize.encode (either (const Nothing) pure . Serialize.decode)

readFile :: (Serialize a) => FilePath -> IO (Maybe a)
readFile fp = fmap decode (Data.ByteString.readFile fp)
  where
    decode :: (Serialize a) => Bytes -> Maybe a
    decode bytes = Lens.preview Lens._Right (Serialize.decode bytes)

writeFile :: (Serialize a) => FilePath -> a -> IO ()
writeFile fp obj = Data.ByteString.writeFile fp (Serialize.encode obj)