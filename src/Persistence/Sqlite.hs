{-# LANGUAGE OverloadedStrings #-}

module Persistence.Sqlite (
  addEntry,
  moveEntry,
  getEntries,
  getOneEntry,
  addColumn,
) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Domain.Entry

-- TODO: Implement reading db file path in a more appropriate place
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
  result <- query_ conn "SELECT entryID, entryTitle, entryDesc, columnTitle FROM Entry JOIN Column ON entryColumn=columnID"
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
  cols <- query conn "SELECT columnID FROM Column WHERE (columnTitle = ?)" (Only colName) :: IO [Only Int]
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
      result <- execute conn "INSERT INTO Column (columnTitle) values (?)" (Only colName)
      return $ Right result
    _ -> return $ Left $ T.unwords ["Column", colName, "already exists"]
