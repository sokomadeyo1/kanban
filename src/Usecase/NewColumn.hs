{-# LANGUAGE OverloadedStrings #-}

module Usecase.NewColumn (newColumn) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence

newColumn :: T.Text -> IO (Either T.Text ())
newColumn colname = do
  let s = T.strip colname
  if T.length s == 0
    then return $ Left "Error: empty column name"
    else do
      col <- Persistence.getOneColumn colname
      case col of
        Left _ -> Persistence.addColumn colname
        Right _ -> return $ Left $ T.unwords ["Column", colname, "already exists"]
