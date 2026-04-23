{-# LANGUAGE OverloadedStrings #-}

module Usecase.TagEntry (tagEntry) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence
import Util.Class (dummy)
import Util.PrettyPrint

tagEntry :: EntryID -> T.Text -> IO (Either T.Text ())
tagEntry entryid tagname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right _ -> do
      checktag <- Persistence.getOneTag tagname
      case checktag of
        Left _ -> do
          _ <- Persistence.newTag tagname
          newtag <- Persistence.getOneTag tagname
          case newtag of
            Left err -> return $ Left err
            Right (Tag tagid _) -> Persistence.tagEntry entryid tagid
        Right (Tag tagid _) -> do
          checktagged <- Persistence.getEntriesTags entryid
          tags <- case checktagged of
            Right tags -> return tags
            _ -> return []
          if elem tagname (map tagName tags)
            then return $ Left $ T.unwords [pretty entryid, "is already tagged with", pretty (dummy tagname :: Tag)]
            else Persistence.tagEntry entryid tagid
