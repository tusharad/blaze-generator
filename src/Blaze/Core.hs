{-# LANGUAGE OverloadedStrings #-}
module Blaze.Core where

import           HTML.Common.Types
import           HTML.Core
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import           System.FilePath (isValid)
import           System.Directory (doesFileExist)
import           System.Environment as Sys
import           Text.Megaparsec

toAttrs :: Text -> Text
toAttrs "class" = "class_"
toAttrs "type" = "type_"
toAttrs k = k

attributesToString :: Map.Map Text Text -> Text
attributesToString attrs = T.unwords $ map (\(k, v) -> "! " <> (toAttrs k) <> " \"" <> v <> "\"") (Map.toList attrs)

elementToString :: Int -> HTMLElement -> Text
elementToString _ (TextNode text) = "\"" <> text <> "\""
elementToString _ (CommentNode _) = ""
elementToString _ (SelfClosingTag tag attrs) = tag <> " " <> (attributesToString attrs)
elementToString n (HTMLElement tag attrs children) = tag <> (if Map.null attrs then "" else " " <> attributesToString attrs) <> " $ do\n" <> (T.intercalate "\n" (map (((T.replicate n "  ") <>) . elementToString (n+1)) children))

processFile_ :: FilePath -> IO ()
processFile_ filePath
  | not (isValid filePath) = putStrLn "Error: Invalid file path."
  | otherwise = do
      fileExists <- doesFileExist filePath
      if fileExists
        then do
          content <- TI.readFile filePath
          parseTest htmlDocument content
        else putStrLn "Error: File does not exist."

processFile :: FilePath -> IO ()
processFile filePath
  | not (isValid filePath) = putStrLn "Error: Invalid file path."
  | otherwise = do
      fileExists <- doesFileExist filePath
      if fileExists
        then do
          content <- TI.readFile filePath
          case toHTMLElement content of
            Left e -> print e
            Right r -> TI.putStrLn $ elementToString 1 $ head r
        else putStrLn "Error: File does not exist."

main' :: IO ()
main' = do
  args <- Sys.getArgs
  print args
  if null args then putStrLn "Please provide html file as command line argument"
  else processFile $ head args
