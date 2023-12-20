
{-# LANGUAGE OverloadedStrings #-}

module AblyPrelude.Development
    ( module X
    , undefined
    , deadCode
    , notImplemented
    , pPrint
    , pPrintHtml
    , pPrintJson
    , pShowJson
    , pTraceJson
    , GS.HasCallStack
    , GS.callStack
    , GS.prettyCallStack
    , embedFile
    ) where

import Prelude hiding (undefined)
import qualified Prelude 

import Debug.Trace as X
import qualified Data.Aeson as DA
import qualified GHC.Stack as GS

import Text.Show.Pretty (pPrint)
import qualified Text.Blaze.Html as TBH
import qualified Text.Blaze.Html.Renderer.String as TBHRS

import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.Aeson.Encode.Pretty as DAEP

import qualified Language.Haskell.TH.Syntax as LHTS
import qualified Data.FileEmbed as DF

embedFile :: [Char] -> LHTS.Q LHTS.Exp
embedFile x = do
    LHTS.addDependentFile x
    DF.embedStringFile x

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: GS.HasCallStack => a
undefined = Prelude.undefined

-- |'deadCode' serves as a signal that a given call to 'error' should never be
-- called regardless of the inputs of a function where it is used.
deadCode :: GS.HasCallStack => [Char] -> a
deadCode = error

{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: GS.HasCallStack => a
notImplemented = Prelude.undefined

pPrintHtml :: TBH.ToMarkup a => a -> IO ()
pPrintHtml = Prelude.putStrLn . TBHRS.renderHtml . TBH.toHtml

pPrintJson :: DA.ToJSON a => a -> IO ()
pPrintJson = DBLC.putStrLn . DAEP.encodePretty

pShowJson :: DA.ToJSON a => a -> String
pShowJson = DBLC.unpack . DAEP.encodePretty

pTraceJson :: DA.ToJSON a => a -> a
pTraceJson x = trace (pShowJson x) x

