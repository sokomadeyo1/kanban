{-# LANGUAGE OverloadedStrings #-}

module Usecase.UntagEntry (untagEntry) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence

untagEntry :: EntryID -> T.Text -> IO (Either T.Text ())
untagEntry entryid tagname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right _ -> do
      result <- Persistence.getOneTag tagname
      case result of
        Left err -> return $ Left err
        Right (Tag tagid _) -> Persistence.untagEntry entryid tagid
