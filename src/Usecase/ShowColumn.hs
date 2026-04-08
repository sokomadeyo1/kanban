{-# LANGUAGE OverloadedStrings #-}

module Usecase.ShowColumn (showColumn) where

import qualified Data.Text as T
import Domain.Column
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence
import Util.PrettyPrint

showColumn :: T.Text -> IO (Either T.Text [(Entry, [Tag])])
showColumn colname = do
  check <- Persistence.getOneColumn colname
  case check of
    Left err -> return $ Left err
    Right (Column colid _) -> do
      result <- Persistence.getEntriesByColumn colid
      case result of
        Right [] -> return $ Left $ T.unwords [pretty $ Column colid colname, "is empty"]
        _ -> return result
