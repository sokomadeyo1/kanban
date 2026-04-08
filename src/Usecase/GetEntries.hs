{-# LANGUAGE OverloadedStrings #-}

module Usecase.GetEntries (getEntries) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence

getEntries :: IO (Either T.Text [(Entry, [Tag])])
getEntries = do
  entries <- Persistence.getTaggedEntries
  case entries of
    Right [] -> return $ Left "The board is still empty. Try using \"AddEntry\""
    _ -> return entries
