{-# LANGUAGE OverloadedStrings #-}

module Usecase.TagEntry (tagEntry) where

import qualified Data.Text as T
import Domain.Entry
import qualified Persistence.Sqlite as Persistence

tagEntry :: EntryID -> T.Text -> IO (Either T.Text ())
tagEntry entryid tagname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right _ -> do
      result <- Persistence.getOneTag tagname
      _ <- case result of
        Left _ -> Persistence.newTag tagname
        Right _ -> return $ Right () -- pass
      Persistence.tagEntry entryid tagname
