{-# LANGUAGE OverloadedStrings #-}

module Usecase.ShowColumn (showColumn) where

import qualified Data.Text as T
import Domain.Column
import Domain.Entry
import qualified Persistence.Sqlite as Persistence

showColumn :: T.Text -> IO (Either T.Text [Entry])
showColumn colname = do
  check <- Persistence.getOneColumn colname
  case check of
    Left err -> return $ Left err
    Right (Column colid _) -> Persistence.getEntriesByColumn colid
