{-# LANGUAGE OverloadedStrings #-}

module Usecase.TagEntry (tagEntry) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence

tagEntry :: EntryID -> T.Text -> IO (Either T.Text ())
tagEntry entryid tagname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right _ -> do
      checktag <- Persistence.getOneTag tagname
      case checktag of
        Right (Tag tagid _) -> Persistence.tagEntry entryid tagid
        Left _ -> do
          _ <- Persistence.newTag tagname
          newtag <- Persistence.getOneTag tagname
          case newtag of
            Left err -> return $ Left err
            Right (Tag tagid _) -> Persistence.tagEntry entryid tagid
