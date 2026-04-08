{-# LANGUAGE OverloadedStrings #-}

module Usecase.GetTaggedEntries (getTaggedEntries) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence

getTaggedEntries :: IO (Either T.Text [(Entry, [Tag])])
getTaggedEntries = do
  entries <- Persistence.getTaggedEntries
  case entries of
    Right [] -> return $ Left "The board is still empty. Try using \"AddEntry\""
    _ -> return entries
