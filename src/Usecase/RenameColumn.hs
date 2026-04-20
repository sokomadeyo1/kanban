{-# LANGUAGE OverloadedStrings #-}

module Usecase.RenameColumn (renameColumn) where

import qualified Data.Text as T
import Domain.Column
import qualified Persistence.Sqlite as Persistence
import Util.Class (dummy)
import Util.PrettyPrint

renameColumn :: T.Text -> T.Text -> IO (Either T.Text ())
renameColumn oldname newname = do
  checkCollision <- Persistence.getOneColumn newname
  case checkCollision of
    Right _ -> return $ Left $ T.unwords ["Column", pretty (dummy newname :: Column), "already exists"]
    Left _ -> do
      col <- Persistence.getOneColumn oldname
      case col of
        Left err -> return $ Left err
        Right (Column colid _) -> Persistence.renameColumn colid newname
