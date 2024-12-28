
{-# LANGUAGE OverloadedStrings #-}

module AblyPrelude.Development
    ( module X
    , undefined
    , textDims
    , pad
    , traceM
    , debugTraceM
    , deadCode
    , pShowTable
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
import Control.Lens
import qualified Prelude 

import Debug.Trace as X hiding (traceM)
import qualified Debug.Trace as DT
import qualified Data.Aeson as DA
import qualified Data.List as DL
import qualified Data.Text as DT
import qualified Data.Text.Lens as DTL
import qualified GHC.Stack as GS

import Text.Show.Pretty (pPrint)
import qualified Text.Blaze.Html as TBH
import qualified Text.Blaze.Html.Renderer.String as TBHRS

import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.Aeson.Encode.Pretty as DAEP

import qualified Language.Haskell.TH.Syntax as LHTS
import qualified Data.FileEmbed as DF

textDims :: DT.Text -> (Int, Int)
textDims xs =
    let ys :: [DT.Text]
        ys = DT.lines xs
    in (maximum $ fmap textWidth $ ys, length ys)

textWidth :: DT.Text -> Int
textWidth = sumOf (DTL.unpacked . folded . from enum . to go)
  where
    go :: Int -> Int
    go n
        -- cjk unified ideographs
        | 0x4e00 <= n && n <= 0x9fff = 2
        | 0x3400 <= n && n <= 0x4dbf = 2
        | 0x20000 <= n && n <= 0x2a6df = 2
        | 0x2a700 <= n && n <= 0x2b73f = 2
        -- hangul
        | 0xac00 <= n && n <= 0xd7af = 2
        -- hiragana and katakana
        | 0x3040 <= n && n <= 0x309f = 2
        | 0x30a0 <= n && n <= 0x30ff = 2
        -- fullwidth forms
        | 0xff01 <= n && n <= 0xff60 = 2
        | 0xffe0 <= n && n <= 0xffe6 = 2
        -- cjk compatibility ideographs
        | 0xf900 <= n && n <= 0xfaff = 2
        -- symbols and punctuation
        | 0x3000 <= n && n <= 0x303f = 2
        | otherwise = 1

padWidth :: Int -> DT.Text -> DT.Text
padWidth n xs =
    let m = textWidth xs
    in DT.replicate (n - m) " " <> xs

pad :: (Int, Int) -> DT.Text -> DT.Text
pad (x, y) xs =
    let xs', xs'' :: [DT.Text]
        xs' = DT.lines xs
        xs'' = xs' <> replicate (y - length xs') ""
    in DT.intercalate "\n" $ fmap (padWidth x) $ xs''

-- |@pShowTable@ is a helper function which takes a grid of text and prints it
-- as a table, with padding so that cells are lined in columns, and a bordered
-- header for the first row
pShowTable :: Bool -> [[DT.Text]] -> DT.Text
pShowTable b xs
    = DT.intercalate "\n"
    $ (if b then addHeaderBorder else id)
    $ fmap (DT.intercalate "   ")
    $ DL.transpose
    $ zip lengths xs' <&> \(n, column) -> column <&> \cell -> padWidth n cell
  where

    addHeaderBorder :: [DT.Text] -> [DT.Text]
    addHeaderBorder [] = []
    addHeaderBorder (y : ys) = y : DT.replicate (textWidth y) "-" : ys

    xs' :: [[DT.Text]]
    xs' = DL.transpose xs

    lengths :: [Int]
    lengths = fmap (maximum . fmap textWidth) $ xs'

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

