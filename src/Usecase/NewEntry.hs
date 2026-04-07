{-# LANGUAGE OverloadedStrings #-}

module Usecase.NewEntry (newEntry) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence
import Domain.Column
import Usecase.NewColumn

defaultColumn :: T.Text
defaultColumn = "Backlog" :: T.Text

newEntry :: T.Text -> T.Text -> IO (Either T.Text ())
newEntry title desc = do
  let t = T.strip title
  if T.length t == 0
    then return $ Left "Error: empty entry name"
    else do
      checkcol <- Persistence.getOneColumn defaultColumn
      case checkcol of
        Right (Column colid _) -> Persistence.addEntry title desc colid
        Left _ -> do
          _ <- newColumn defaultColumn
          newcol <- Persistence.getOneColumn defaultColumn
          case newcol of
            Left err -> return $ Left err
            Right (Column colid_new _) -> Persistence.addEntry title desc colid_new
