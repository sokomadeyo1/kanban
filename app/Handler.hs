{-# LANGUAGE OverloadedStrings #-}

module Handler (Handler (..), handle) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PrettyPrint
import Domain.Entry
import Domain.Tag
import Usecase.GetTaggedEntries
import Usecase.MoveEntry
import Usecase.NewColumn
import Usecase.NewEntry
import Usecase.GetColumns
import Usecase.NewTag
import Usecase.GetTags
import Usecase.GetEntriesByTag
import Usecase.TagEntry
import Usecase.UntagEntry
import Usecase.DeleteTag

data Handler
  = GetEntries
  | NewEntry T.Text T.Text
  | MoveEntry EntryID T.Text
  | NewColumn T.Text
  | GetColumns
  | NewTag T.Text
  | GetTags
  | EntriesByTag T.Text
  | TagEntry EntryID T.Text
  | UntagEntry EntryID T.Text
  | DeleteTag T.Text

handle :: Handler -> IO ()
handle GetEntries = do
  result <- getTaggedEntries
  case result of
    Left e -> TIO.putStrLn e
    Right r -> TIO.putStr $ pretty r
handle (NewEntry title desc) = do
  result <- newEntry title desc
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Created a new entry:", title]
handle (MoveEntry entryID colName) = do
  result <- moveEntry entryID colName
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Moved entry", T.show entryID, "to the", colName, "column"]
handle (NewColumn colName) = do
  result <- newColumn colName
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Created a new column:", colName]
handle GetColumns = do
  result <- getColumns
  case result of
    Left e -> TIO.putStrLn e
    Right r -> TIO.putStr $ pretty r
handle (NewTag tagName) = do
  result <- newTag tagName
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Created a new tag:", tagName]
handle GetTags = do
  result <- getTags
  case result of
    Left e -> TIO.putStrLn e
    Right tags -> TIO.putStr $ pretty tags
handle (EntriesByTag tagname) = do
  result <- getEntriesByTag tagname
  case result of
    Left e -> TIO.putStrLn e
    Right entries -> TIO.putStr $ T.unlines [T.unwords ["Entries with", pretty $ Tag (TagID 0) tagname, ":"], pretty entries]
handle (TagEntry entryid tagname) = do
  result <- tagEntry entryid tagname
  case result of
    Left e -> TIO.putStrLn e
    Right args -> TIO.putStrLn $ T.unwords ["Added", pretty $ Tag (TagID 0) tagname, "to entry", T.show entryid]
handle (UntagEntry entryid tagname) = do
  result <- untagEntry entryid tagname
  case result of
    Left e -> TIO.putStrLn e
    Right args -> TIO.putStrLn $ T.unwords ["Removed", pretty $ Tag (TagID 0) tagname, "from entry", T.show entryid]
handle (DeleteTag tagname) = do
  result <- deleteTag tagname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Deleted tag:", tagname]
