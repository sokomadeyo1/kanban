module Usecase.GetConstraints (getConstraints) where

import qualified Persistence.Sqlite as Persistence
import qualified Data.Text as T

getConstraints :: IO (Either T.Text [(T.Text, T.Text)])
getConstraints = do
  res <- Persistence.getConstraints
  case res of
    Left e -> return $ Left e
    Right constraints -> return $ Right constraints
