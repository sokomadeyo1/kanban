{-# LANGUAGE OverloadedStrings #-}

module Usecase.TagEntry (tagEntry) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence
import Domain.Entry

tagEntry :: EntryID -> T.Text -> IO (Either T.Text ())
tagEntry entryid tagname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right _ -> Persistence.tagEntry entryid tagname
