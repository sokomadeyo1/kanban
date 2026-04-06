module Usecase.GetEntries (getEntries) where

import qualified Data.Text as T
import Domain.Entry
import qualified Persistence.Sqlite as Persistence

getEntries :: IO (Either T.Text [Entry])
getEntries = do
  Persistence.getEntries
