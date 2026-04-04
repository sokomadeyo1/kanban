{-# LANGUAGE OverloadedStrings #-}

module Usecase.NewEntry (newEntry) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence

defaultColumn :: T.Text
defaultColumn = "Backlog" :: T.Text

newEntry :: T.Text -> T.Text -> IO (Either String ())
newEntry title desc = do
  Persistence.addEntry title desc defaultColumn
