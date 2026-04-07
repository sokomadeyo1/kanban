{-# LANGUAGE OverloadedStrings #-}

module Usecase.DeleteEntry (deleteEntry) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence
import Domain.Entry

deleteEntry :: EntryID -> IO (Either T.Text ())
deleteEntry entryid = do
  result <- Persistence.getOneEntry entryid
  case result of
    Left err -> return $ Left err
    Right _ -> do
      _ <- Persistence.deleteEntry entryid
      check <- Persistence.getOneEntry entryid
      case check of
        Right _ -> return $ Left "Could not delete entry"
        Left _ -> return $ Right ()
