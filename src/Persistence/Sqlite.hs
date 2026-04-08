{-# LANGUAGE OverloadedStrings #-}

module Persistence.Sqlite (
  addEntry,
  moveEntry,
  renameEntry,
  editEntry,
  deleteEntry,
  getEntries,
  getOneEntry,
  addColumn,
  renameColumn,
  getColumns,
  getColumnEntries,
  getOneColumn,
  deleteColumn,
  checkRestrict,
  newTag,
  renameTag,
  getTags,
  getOneTag,
  tagEntry,
  untagEntry,
  getEntriesTags,
  getEntriesByTag,
  getEntriesByColumn,
  deleteTag,
  deleteTagInstances,
) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Domain.Column
import Domain.Entry
import Domain.Constraint
import Domain.Tag

db :: String
db = "data/dev.db"

addEntry :: T.Text -> T.Text -> ColumnID -> IO (Either T.Text ())
addEntry title desc colid = do
  conn <- open db
  result <- execute conn
    "INSERT INTO Entry (entryTitle, entryDesc, entryColumn) values (?, ?, ?)"
    (title, desc, colid)
  return $ Right result

getEntries :: IO (Either T.Text [Entry])
getEntries = do
  conn <- open db
  result <- query_ conn "SELECT entryID, entryTitle, entryDesc, columnTitle FROM Entry JOIN Column ON entryColumn=columnID ORDER BY columnID"
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

moveEntry :: EntryID -> ColumnID -> IO (Either T.Text ())
moveEntry entryid colid = do
  conn <- open db
  result <- execute conn
    "UPDATE Entry SET entryColumn = ? WHERE (entryID = ?)"
    (colid, entryid)
  return $ Right result

renameEntry :: EntryID -> T.Text -> IO (Either T.Text ())
renameEntry entryid newname = do
  conn <- open db
  result <- execute conn
    "UPDATE Entry SET entryTitle = ? WHERE (entryID = ?)"
    (newname, entryid)
  return $ Right result

editEntry :: EntryID -> T.Text -> IO (Either T.Text ())
editEntry entryid newname = do
  conn <- open db
  result <- execute conn
    "UPDATE Entry SET entryDesc = ? WHERE (entryID = ?)"
    (newname, entryid)
  return $ Right result

deleteEntry :: EntryID -> IO (Either T.Text ())
deleteEntry entryid = do
  conn <- open db
  execute conn "PRAGMA foreign_keys = ON;" ()
  result <- execute conn
    "DELETE FROM Entry WHERE entryID = ?"
    (Only entryid)
  return $ Right result

addColumn :: T.Text -> IO (Either T.Text ())
addColumn colName = do
  conn <- open db
  result <- execute conn "INSERT INTO Column (columnTitle) VALUES (?)" (Only colName)
  return $ Right result

renameColumn :: ColumnID -> T.Text -> IO (Either T.Text ())
renameColumn columnid newname = do
  conn <- open db
  result <- execute conn
    "UPDATE Column SET columnTitle = ? WHERE (columnID = ?)"
    (newname, columnid)
  return $ Right result

getColumns :: IO (Either T.Text [Column])
getColumns = do
  conn <- open db
  cols <- query_ conn
    "SELECT * FROM Column"
  return $ Right cols

getColumnEntries :: ColumnID -> IO (Either T.Text [Entry])
getColumnEntries columnid = do
  conn <- open db
  cols <- query conn
    "SELECT entryID, entryTitle, entryDesc, columnTitle FROM Entry JOIN Column ON entryColumn = columnID WHERE columnID = ?"
    (Only columnid)
  return $ Right cols

getOneColumn :: T.Text -> IO (Either T.Text Column)
getOneColumn colName = do
  conn <- open db
  cols <- query conn
    "SELECT * FROM Column WHERE columnTitle = ?"
    (Only colName)
  case cols of
    [c] -> return $ Right c
    _ -> return $ Left $ T.unwords ["No column named", colName, "found"]

deleteColumn :: ColumnID -> IO (Either T.Text ())
deleteColumn columnid = do
  conn <- open db
  execute conn "PRAGMA foreign_keys = ON;" ()
  result <- execute conn
    "DELETE FROM Column WHERE columnID = ?"
    (Only columnid)
  return $ Right result

checkRestrict :: ColumnID -> ColumnID -> IO (Either T.Text Bool)
checkRestrict fromcol tocol = do
  conn <- open db
  result <- query conn
    "SELECT * FROM Restrict WHERE (fromColumn = ? AND toColumn = ?)"
    (fromcol, tocol) :: IO [Constraint]
  case result of
    [] -> return $ Right False
    _ -> return $ Right True

newTag :: T.Text -> IO (Either T.Text ())
newTag tagname = do
  conn <- open db
  result <- execute conn
    "INSERT INTO Tag (tagName) VALUES (?)"
    (Only tagname)
  return $ Right result

renameTag :: TagID -> T.Text -> IO (Either T.Text ())
renameTag tagid newname = do
  conn <- open db
  result <- execute conn
    "UPDATE Tag SET tagName = ? WHERE (tagID = ?)"
    (newname, tagid)
  return $ Right result

getTags :: IO (Either T.Text [Tag])
getTags = do
  conn <- open db
  tags <- query_ conn
    "SELECT * FROM Tag"
  return $ Right tags

getOneTag :: T.Text -> IO (Either T.Text Tag)
getOneTag tagname = do
  conn <- open db
  tags <- query conn
    "SELECT * FROM Tag WHERE tagName = ?"
    (Only tagname)
  case tags of
    [tag] -> return $ Right tag
    _ -> return $ Left "Tag not found"

tagEntry :: EntryID -> TagID -> IO (Either T.Text ())
tagEntry entryid tagid = do
  conn <- open db
  result <- execute conn
    "INSERT INTO EntriesTags (tagID, entryID) VALUES (?, ?)"
    (tagid, entryid)
  return $ Right result

untagEntry :: EntryID -> TagID -> IO (Either T.Text ())
untagEntry entryid tagid = do
  conn <- open db
  _ <- execute conn "PRAGMA foreign_keys = ON;" ()
  result <- execute conn
    "DELETE FROM EntriesTags WHERE (tagID = ? AND entryID = ?)"
    (tagid, entryid)
  return $ Right result

getEntriesTags :: EntryID -> IO (Either T.Text [Tag])
getEntriesTags entryid = do
  conn <- open db
  tags <- query conn
    "SELECT Tag.tagID, tagName FROM Tag NATURAL JOIN EntriesTags WHERE entryID = ?"
    (Only entryid)
  return $ Right tags

getEntriesByTag :: TagID -> IO (Either T.Text [Entry])
getEntriesByTag tagid = do
  conn <- open db
  entries <- query conn
    "SELECT entryID, entryTitle, entryDesc, columnTitle FROM Entry JOIN Column ON columnID = entryColumn NATURAL JOIN EntriesTags WHERE EntriesTags.tagID = ?"
    (Only tagid)
  return $ Right entries

getEntriesByColumn :: ColumnID -> IO (Either T.Text [Entry])
getEntriesByColumn colid = do
  conn <- open db
  entries <- query conn
    "SELECT entryID, entryTitle, entryDesc, columnTitle FROM Entry JOIN Column ON columnID = entryColumn WHERE columnID = ?"
    (Only colid)
  return $ Right entries

deleteTag :: TagID -> IO (Either T.Text ())
deleteTag tagid = do
  conn <- open db
  _ <- execute conn "PRAGMA foreign_keys = ON;" ()
  result <- execute conn
    "DELETE FROM Tag WHERE tagID = ?"
    (Only tagid)
  return $ Right result

deleteTagInstances :: TagID -> IO (Either T.Text ())
deleteTagInstances tagid = do
  conn <- open db
  _ <- execute conn "PRAGMA foreign_keys = ON;" ()
  result <- execute conn
    "DELETE FROM EntriesTags WHERE tagID = ?"
    (Only tagid)
  return $ Right result
