module Usecase.NewColumn (newColumn) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence

newColumn :: T.Text -> IO (Either T.Text ())
newColumn colName = do
  Persistence.addColumn colName
