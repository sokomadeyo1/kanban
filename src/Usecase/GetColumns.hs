{-# LANGUAGE OverloadedStrings #-}

module Usecase.GetColumns (getColumns) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence
import Domain.Column

getColumns :: IO (Either T.Text [Column])
getColumns = do
  columns <- Persistence.getColumns
  case columns of
    Right [] -> return $ Left "There are no columns. Try using \"NewColumn\""
    _ -> return columns
