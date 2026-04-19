module Usecase.GetConstraints (getConstraints) where

import qualified Persistence.Sqlite as Persistence
import qualified Data.Text as T
import Domain.Column
import Util.Class (dummy)

getConstraints :: IO (Either T.Text [(Column, Column)])
getConstraints = do
  res <- Persistence.getConstraints
  case res of
    Left e -> return $ Left e
    Right constraints -> return $ Right $ map (\(x, y) -> (dummy x, dummy y)) constraints
