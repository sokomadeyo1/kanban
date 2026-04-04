module Usecase.GetEntries (getEntries) where

import qualified Persistence.Sqlite as Persistence

getEntries :: IO [Persistence.EntryField]
getEntries = do
  Persistence.getEntries
