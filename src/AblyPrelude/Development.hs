
{-# LANGUAGE OverloadedStrings #-}

module AblyPrelude.Development
    ( module X
    , undefined
    , traceM
    , debugTraceM
    , deadCode
    , notImplemented
    , pPrint

    , pPrintHtml
    , pTraceHtml
    , pShowHtml

    , pPrintJson
    , pShowJson
    , pTraceJson
    , pTraceJsonM
    , GS.HasCallStack
    , GS.callStack
    , GS.prettyCallStack
    , embedFile
    ) where

import Prelude hiding (undefined)
import qualified Prelude 

import Debug.Trace as X hiding (traceM)
import qualified Debug.Trace as DT
import qualified Data.Aeson as DA
import qualified Data.Text as DT
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

debugTraceM :: (Applicative m, GS.HasCallStack) => DT.Text -> m ()
debugTraceM msg = DT.traceM (GS.prettyCallStack (pop GS.callStack) <> "\n" <> DT.unpack msg)
  where
    pop = id
    -- pop = GS.fromCallSiteList . drop 1 . GS.getCallStack

traceM :: Applicative m => DT.Text -> m ()
traceM = DT.traceM . DT.unpack

pPrintHtml :: TBH.ToMarkup a => a -> IO ()
pPrintHtml = Prelude.putStrLn . TBHRS.renderHtml . TBH.toHtml

pShowHtml :: TBH.ToMarkup a => a -> [Char]
pShowHtml = TBHRS.renderHtml . TBH.toHtml

pTraceHtml :: (Monad m) => TBH.ToMarkup a => a -> m ()
pTraceHtml = traceM . DT.pack . TBHRS.renderHtml . TBH.toHtml

pPrintJson :: DA.ToJSON a => a -> IO ()
pPrintJson = DBLC.putStrLn . DAEP.encodePretty

pShowJson :: DA.ToJSON a => a -> DT.Text
pShowJson = DT.pack . DBLC.unpack . DAEP.encodePretty

pTraceJson :: DA.ToJSON a => a -> a
pTraceJson x = trace (DT.unpack $ pShowJson x) x

pTraceJsonM :: Applicative m => DA.ToJSON a => a -> m ()
pTraceJsonM = traceM . pShowJson

