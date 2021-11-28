
{-# LANGUAGE OverloadedStrings #-}

module AblyPrelude.Development
    ( module X
    , undefined
    , deadCode
    , notImplemented

    -- , Control.Lens._Show
    -- , Prelude.show
    , pPrint
    ) where

import Prelude hiding (undefined)
import qualified Prelude 

--import qualified Text.Pretty.Simple
--import qualified System.Console.Terminal.Size as Size
import Debug.Trace as X
import qualified GHC.Stack as GS

import Text.Show.Pretty (pPrint)

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

-- _Show :: (Show a, Read a) => Prism' [Char] a
-- _Show = AblyPrelude.Lens._Show

-- printAsTable :: [[Text]] -> IO ()
-- printAsTable = traverse_ printLn . List.transpose . fmap go . List.transpose
--   where
--     printLn :: [Text] -> IO ()
--     printLn row = traverse_ putStr row >> putStrLn ""

--     go :: [Text] -> [Text]
--     go xs = Text.justifyLeft (1 + maxLength xs) ' ' <$> xs

--     maxLength :: [Text] -> Int
--     maxLength = maximum1Of (folded . to Text.length)

-- pPrint :: (Show a) => a -> IO ()
-- pPrint obj = size >>= \case
--     Nothing -> Text.Pretty.Simple.pPrint obj
--     Just n  -> Text.Pretty.Simple.pShow obj & _ & lines & fmap (clampLine n)
--         & collapseEmptyLines & set (mapped . _Right . ix n) '…'
--         & fmap (foldLine n) & take 300 & unlines & putStrLn

--   where
--     foldLine :: Int -> Either Int String -> String
--     foldLine n (Left a) = take n $
--         "+-- " <> show a <> " lines hidden " <> repeat '-'
--     foldLine _ (Right a) = a

--     clampLine :: Int -> String -> Maybe String
--     clampLine n x =
--         let x' = take n x
--         in fmap_ x' (guard $ any (/= ' ') x')
--         & set (_Just . ix n) '…'

--     collapseEmptyLines :: [Maybe a] -> [Either Int a]
--     collapseEmptyLines = foldr go []
--       where
--         go Nothing (Left x : xs) = Left (x + 1) : xs
--         go Nothing xs            = Left 1       : xs
--         go (Just x) xs           = Right x      : xs

--     size :: IO (Maybe Int)
--     size = over (mapped . mapped) Size.width Size.size

