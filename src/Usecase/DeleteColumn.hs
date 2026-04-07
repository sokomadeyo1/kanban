{-# LANGUAGE OverloadedStrings #-}

module Usecase.DeleteColumn (deleteColumn) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence
import Domain.Column

deleteColumn :: T.Text -> IO (Either T.Text ())
deleteColumn colname = do
  result <- Persistence.getOneColumn colname
  case result of
    Left err -> return $ Left err
    Right (Column colid _) -> do
      entries <- Persistence.getColumnEntries colid
      case entries of
        Right [] -> do
          _ <- Persistence.deleteColumn colid
          check <- Persistence.getOneColumn colname
          case check of
            Right _ -> return $ Left "Could not delete column"
            Left _ -> return $ Right ()
        _ -> return $ Left "Could not delete column. There are still entries left"
