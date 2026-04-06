{-# LANGUAGE OverloadedStrings #-}

module Handler (Handler (..), handle) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PrettyPrint
import Domain.Entry
import Domain.Tag
import Usecase.GetEntries
import Usecase.MoveEntry
import Usecase.NewColumn
import Usecase.NewEntry
import Usecase.GetColumns
import Usecase.NewTag
import Usecase.GetTags
import Usecase.TagEntry

data Handler
  = GetEntries
  | NewEntry T.Text T.Text
  | MoveEntry EntryID T.Text
  | NewColumn T.Text
  | GetColumns
  | NewTag T.Text
  | GetTags
  | TagEntry EntryID T.Text

handle :: Handler -> IO ()
handle GetEntries = do
  result <- getEntries
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
handle (TagEntry entryid tagname) = do
  result <- tagEntry entryid tagname
  case result of
    Left e -> TIO.putStrLn e
    Right args -> TIO.putStrLn $ T.unwords ["Added", pretty $ Tag (TagID 0) tagname, "to entry", T.show entryid]
