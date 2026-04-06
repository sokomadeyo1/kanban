{-# LANGUAGE OverloadedStrings #-}

module Persistence.Sqlite (
  addEntry,
  moveEntry,
  getEntries,
  getOneEntry,
  addColumn,
  getColumns,
  newTag,
  getTags,
  tagEntry,
  getEntriesTags,
) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Domain.Column
import Domain.Entry
import Domain.Tag

db :: String
db = "data/dev.db"

addEntry :: T.Text -> T.Text -> T.Text -> IO (Either T.Text ())
addEntry title desc colName = do
  conn <- open db
  cols <- query conn
    "SELECT columnID FROM Column WHERE (columnTitle = ?)"
    (Only colName) :: IO [Only Int]
  case cols of
    [Only i] -> do
      result <- execute conn
        "INSERT INTO Entry (entryTitle, entryDesc, entryColumn) values (?, ?, ?)"
        (title, desc, i)
      return $ Right result
    _ -> return $ Left $ T.unwords ["No column named", colName, "found"]

getEntries :: IO (Either T.Text [Entry])
getEntries = do
  conn <- open db
  result <- query_ conn
    "SELECT entryID, entryTitle, entryDesc, columnTitle FROM Entry JOIN Column ON entryColumn=columnID"
  return $ Right result

getOneEntry :: EntryID -> IO (Either T.Text Entry)
getOneEntry eid = do
  conn <- open db
  result <- query conn
    "SELECT entryID, entryTitle, entryDesc, columnTitle FROM Entry JOIN Column ON entryColumn=columnID WHERE (entryID = ?)"
    (Only eid)
  case result of
    [e] -> return $ Right e
    _ -> return $ Left "Entry not found"

moveEntry :: EntryID -> T.Text -> IO (Either T.Text ())
moveEntry entryID colName = do
  conn <- open db
  cols <- query conn
    "SELECT columnID FROM Column WHERE (columnTitle = ?)"
    (Only colName) :: IO [Only ColumnID]
  case cols of
    [Only col] -> do
      result <- execute conn
        "UPDATE Entry SET entryColumn = ? WHERE (entryID = ?)"
        (col, entryID)
      return $ Right result
    _ -> return $ Left $ T.unwords ["No column named", colName, "found"]

addColumn :: T.Text -> IO (Either T.Text ())
addColumn colName = do
  conn <- open db
  cols <- query conn
    "SELECT columnID FROM Column WHERE (columnTitle = ?)"
    (Only colName) :: IO [Only Int]
  case cols of
    [] -> do
      result <- execute conn "INSERT INTO Column (columnTitle) VALUES (?)" (Only colName)
      return $ Right result
    _ -> return $ Left $ T.unwords ["Column", colName, "already exists"]

getColumns :: IO (Either T.Text [Column])
getColumns = do
  conn <- open db
  cols <- query_ conn
    "SELECT * FROM Column"
  return $ Right cols

newTag :: T.Text -> IO (Either T.Text ())
newTag tagName = do
  conn <- open db
  result <- execute conn
    "INSERT INTO Tag (tagName) VALUES (?)"
    (Only tagName)
  return $ Right result

getTags :: IO (Either T.Text [Tag])
getTags = do
  conn <- open db
  tags <- query_ conn
    "SELECT * FROM Tag"
  return $ Right tags

tagEntry :: EntryID -> T.Text -> IO (Either T.Text ())
tagEntry entryid tagname = do
  conn <- open db
  tags <- query conn
    "SELECT tagID FROM Tag WHERE tagName = ?"
    (Only tagname) :: IO [Only TagID]
  case tags of
    [Only tag] -> do
      result <- execute conn
        "INSERT INTO EntriesTags (tagID, entryID) VALUES (?, ?)"
        (tag, entryid)
      return $ Right result
    _ -> return $ Left $ T.unwords ["No tag named", tagname, "found"]

getEntriesTags :: EntryID -> IO (Either T.Text [Tag])
getEntriesTags entryid = do
  conn <- open db
  tags <- query conn
    "SELECT Tag.tagID, tagName FROM Tag NATURAL JOIN EntriesTags WHERE entryID = ?"
    (Only entryid)
  return $ Right tags
