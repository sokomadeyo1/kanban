module Usecase.GetEntries (getEntries) where

import qualified Persistence.Sqlite as Persistence
import Domain.Entry

getEntries :: IO (Either String [Entry])
getEntries = do
  Persistence.getEntries
