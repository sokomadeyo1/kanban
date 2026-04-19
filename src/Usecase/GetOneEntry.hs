{-# LANGUAGE OverloadedStrings #-}

module Usecase.GetOneEntry (getOneEntry) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence

getOneEntry :: EntryID -> IO (Either T.Text (Entry, [Tag]))
getOneEntry i = do
  res <- Persistence.getOneEntry i
  case res of
    Left _ -> return $ Left "Entry not found."
    Right entry -> do
      checktags <- Persistence.getEntriesTags i
      case checktags of
        Left _ -> return $ Left "Couldn't fetch tags."
        Right tags -> return $ Right (entry, tags)
