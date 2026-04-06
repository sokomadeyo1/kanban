module Domain.Constraint (Constraint) where

import Database.SQLite.Simple.FromRow

data Constraint = Restrict ColumnID ColumnID deriving (Eq, Show)

newtype ColumnID = ColumnID Int deriving (Eq, Show)

instance FromRow Constraint where
  fromRow =
    Restrict
      <$> (fmap ColumnID field)
      <*> (fmap ColumnID field)
