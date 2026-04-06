{-# LANGUAGE OverloadedStrings #-}

module Usecase.NewColumn (newColumn) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence

newColumn :: T.Text -> IO (Either T.Text ())
newColumn colName = do
  let s = T.strip colName
  if T.length s == 0
    then return $ Left "Error: empty column name"
    else Persistence.addColumn colName
