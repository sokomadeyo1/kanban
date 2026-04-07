{-# LANGUAGE OverloadedStrings #-}

module Handler (Handler (..), handle) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Domain.Entry
import Domain.Tag
import PrettyPrint
import Usecase.DeleteColumn
import Usecase.DeleteEntry
import Usecase.DeleteTag
import Usecase.EditEntry
import Usecase.GetColumns
import Usecase.GetEntriesByTag
import Usecase.GetTaggedEntries
import Usecase.GetTags
import Usecase.MoveEntry
import Usecase.NewColumn
import Usecase.NewEntry
import Usecase.NewTag
import Usecase.RenameColumn
import Usecase.RenameEntry
import Usecase.RenameTag
import Usecase.ShowColumn
import Usecase.TagEntry
import Usecase.UntagEntry

data Handler
  = GetEntries
  | NewEntry T.Text T.Text
  | MoveEntry EntryID T.Text
  | RenameEntry EntryID T.Text
  | EditEntry EntryID T.Text
  | DeleteEntry EntryID
  | NewColumn T.Text
  | RenameColumn T.Text T.Text
  | GetColumns
  | ShowColumn T.Text
  | DeleteColumn T.Text
  | NewTag T.Text
  | RenameTag T.Text T.Text
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
handle (MoveEntry entryid colname) = do
  result <- moveEntry entryid colname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Moved entry", pretty entryid, "to the", colname, "column"]
handle (RenameEntry entryid newname) = do
  result <- renameEntry entryid newname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Renamed entry", pretty entryid]
handle (EditEntry entryid newname) = do
  result <- editEntry entryid newname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Changed the description of entry", pretty entryid]
handle (DeleteEntry entryid) = do
  result <- deleteEntry entryid
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Deleted entry", pretty entryid]
handle (NewColumn colname) = do
  result <- newColumn colname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Created a new column:", colname]
handle (RenameColumn oldname newname) = do
  result <- renameColumn oldname newname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Renamed column", oldname, "to", newname]
handle GetColumns = do
  result <- getColumns
  case result of
    Left e -> TIO.putStrLn e
    Right r -> TIO.putStr $ pretty r
handle (ShowColumn colname) = do
  result <- showColumn colname
  case result of
    Left e -> TIO.putStrLn e
    Right r -> TIO.putStr $ pretty r
handle (DeleteColumn colname) = do
  result <- deleteColumn colname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Deleted column", colname]
handle (NewTag tagname) = do
  result <- newTag tagname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Created a new tag:", tagname]
handle (RenameTag oldname newname) = do
  result <- renameTag oldname newname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords
      [ "Renamed tag"
      , pretty $ Tag (TagID 0) $ oldname
      , "to"
      , pretty $ Tag (TagID 0) $ newname
      ]
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
    Right _ -> TIO.putStrLn $ T.unwords ["Added", pretty $ Tag (TagID 0) tagname, "to entry", pretty entryid]
handle (UntagEntry entryid tagname) = do
  result <- untagEntry entryid tagname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Removed", pretty $ Tag (TagID 0) tagname, "from entry", pretty entryid]
handle (DeleteTag tagname) = do
  result <- deleteTag tagname
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Deleted tag:", tagname]
