module Domain.Constraint (
  Constraint (..),
) where

import Database.SQLite.Simple.FromRow
import Domain.Column (ColumnID (..))

data Constraint = Restrict ColumnID ColumnID deriving (Eq, Show)

instance FromRow Constraint where
  fromRow =
    Restrict
      <$> (fmap ColumnID field)
      <*> (fmap ColumnID field)
