{-# LANGUAGE OverloadedStrings #-}

module Usecase.NewEntry (newEntry) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence

defaultColumn :: T.Text
defaultColumn = "Backlog" :: T.Text

newEntry :: T.Text -> T.Text -> IO (Either T.Text ())
newEntry title desc = do
  let t = T.strip title
  if T.length t == 0
    then return $ Left "Error: empty entry name"
    else Persistence.addEntry title desc defaultColumn
