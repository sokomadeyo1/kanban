{-# LANGUAGE OverloadedStrings #-}

module Persistence.Sqlite (
  addEntry,
  moveEntry,
  getEntries,
  addColumn,
) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Domain.Entry

-- TODO: Implement reading db file path in a more appropriate place
db :: String
db = "data/dev.db"

-- TODO: Decide when to use Text and when to use String
addEntry :: T.Text -> T.Text -> T.Text -> IO (Either String ())
addEntry title desc colName = do
  conn <- open db
  cols <- query conn "SELECT columnID FROM Column WHERE (columnTitle = ?)" (Only colName) :: IO [Only Int]
  case cols of
    [Only i] -> do
      result <-
        execute
          conn
          "INSERT INTO Entry (entryTitle, entryDesc, entryColumn) values (?, ?, ?)"
          (title, desc, i)
      return $ Right result
    _ -> return $ Left $ "No column named " ++ (T.unpack colName) ++ " found"

getEntries :: IO (Either String [Entry])
getEntries = do
  conn <- open db
  result <- query_ conn "SELECT entryID, entryTitle, entryDesc, columnTitle FROM Entry JOIN Column ON entryColumn=columnID"
  return $ Right result

moveEntry :: Int -> T.Text -> IO (Either String ())
moveEntry entryID colName = do
  conn <- open db
  cols <- query conn "SELECT columnID FROM Column WHERE (columnTitle = ?)" (Only colName) :: IO [Only Int]
  case cols of
    [Only col] -> do
      result <-
        execute
          conn "UPDATE Entry SET entryColumn = ? WHERE (entryID = ?)" (col, entryID)
      return $ Right result
    _ -> return $ Left $ "No column named " ++ (T.unpack colName) ++ " found"

addColumn :: T.Text -> IO (Either String ())
addColumn colName = do
  conn <- open db
  cols <- query conn "SELECT columnID FROM Column WHERE (columnTitle = ?)" (Only colName) :: IO [Only Int]
  case cols of
    [] -> do
      result <- execute conn "INSERT INTO Column (columnTitle) values (?)" (Only colName)
      return $ Right result
    _ -> return $ Left $ "Column " ++ (T.unpack colName) ++ " already exists"
