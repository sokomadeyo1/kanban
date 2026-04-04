{-# LANGUAGE OverloadedStrings #-}

module Persistence.Sqlite (
  EntryField,
  ColumnField,
  addEntry,
  moveEntry,
  getEntries,
  addColumn,
) where

import qualified Data.Text as T
import Database.SQLite.Simple

-- TODO: Implement reading db file path in a more appropriate place
db :: String
db = "data/dev.db"

-- TODO: Import data types from Domain module
data EntryField = EntryField Int T.Text T.Text Int deriving (Show)
data ColumnField = ColumnField Int T.Text deriving (Show)

instance FromRow EntryField where
  fromRow = EntryField <$> field <*> field <*> field <*> field

instance FromRow ColumnField where
  fromRow = ColumnField <$> field <*> field

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

getEntries :: IO [EntryField]
getEntries = do
  conn <- open db
  query_ conn "SELECT * FROM Entry"

moveEntry :: Int -> T.Text -> IO (Either String ())
moveEntry entryID colName = do
  conn <- open db
  cols <- query conn "SELECT columnID FROM Column WHERE (columnTitle = ?)" (Only colName) :: IO [Only Int]
  case cols of
    [Only col] -> do
      result <-
        execute
          conn "UPDATE (SELECT * FROM Entry WHERE (entryID = ?)) SET entryColumn = ?" (entryID, col)
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
