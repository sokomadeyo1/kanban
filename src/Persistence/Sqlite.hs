{-# LANGUAGE OverloadedStrings #-}

module Persistence.Sqlite (
  addEntry,
  getEntries,
  EntryField,
) where

import qualified Data.Text as T
import Database.SQLite.Simple

-- TODO: Implement reading db file path in a more appropriate place
db :: String
db = "data/dev.db"

-- TODO: Import data types from Model module
data EntryField = EntryField Int T.Text T.Text deriving (Show)

instance FromRow EntryField where
  fromRow = EntryField <$> field <*> field <*> field

addEntry :: T.Text -> T.Text -> IO ()
addEntry title desc = do
  conn <- open db
  execute
    conn
    "INSERT INTO Entry (entryTitle, entryDesc) values (?, ?)"
    (title, desc)

getEntries :: IO [EntryField]
getEntries = do
  conn <- open db
  query_ conn "SELECT * FROM Entry"
