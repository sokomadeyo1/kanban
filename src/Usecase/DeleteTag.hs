{-# LANGUAGE OverloadedStrings #-}

module Usecase.DeleteTag (deleteTag) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence
import Domain.Tag

deleteTag :: T.Text -> IO (Either T.Text ())
deleteTag tagname = do
  result <- Persistence.getOneTag tagname
  case result of
    Left err -> return $ Left err
    Right (Tag tagid _) -> do
      _ <- Persistence.deleteTagInstances tagid
      _ <- Persistence.deleteTag tagid
      return $ Right ()
