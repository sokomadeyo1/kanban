module Usecase.NewEntry (newEntry) where

import Persistence.Sqlite (addEntry)
import qualified Data.Text as T

newEntry :: T.Text -> T.Text -> IO ()
newEntry title desc = do
  addEntry title desc
