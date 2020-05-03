
{-# LANGUAGE OverloadedStrings #-}

module AblyPrelude.Development
    ( module X
    , AblyPrelude.Development.undefined
    -- , pPrint
    , printAsTable
    ) where

import qualified Prelude 
import AblyPrelude
import AblyPrelude.Lens

import qualified Text.Pretty.Simple
import qualified System.Console.Terminal.Size as Size
import Debug.Trace as X

import qualified Data.List as List (transpose)
import qualified Data.Text as Text

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = Prelude.undefined

printAsTable :: [[Text]] -> IO ()
printAsTable = traverse_ printLn . List.transpose . fmap go . List.transpose
  where
    printLn :: [Text] -> IO ()
    printLn row = traverse_ putStr row >> putStrLn ""

    go :: [Text] -> [Text]
    go xs = Text.justifyLeft (1 + maxLength xs) ' ' <$> xs

    maxLength :: [Text] -> Int
    maxLength = maximum1Of (folded . to Text.length)

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

