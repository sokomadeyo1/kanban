{-# LANGUAGE OverloadedStrings #-}

module Usecase.GetEntries (getEntries) where

import qualified Data.Text as T
import Domain.Entry
import qualified Persistence.Sqlite as Persistence

getEntries :: IO (Either T.Text [Entry])
getEntries = do
  entries <- Persistence.getEntries
  case entries of
    Right [] -> return $ Left "The board is still empty. Try using \"AddEntry\""
    _ -> return $ entries
