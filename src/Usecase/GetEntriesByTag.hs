{-# LANGUAGE OverloadedStrings #-}

module Usecase.GetEntriesByTag (getEntriesByTag) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence

getEntriesByTag :: T.Text -> IO (Either T.Text [(Entry, [Tag])])
getEntriesByTag tagname = do
  getTagID <- Persistence.getOneTag tagname
  case getTagID of
    Left err -> return $ Left err
    Right (Tag tagid _) -> Persistence.getEntriesByTag tagid
