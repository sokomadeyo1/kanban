module Domain.Constraint (Constraint) where

import Database.SQLite.Simple.FromRow

data Constraint = Restrict ColumnID ColumnID deriving (Eq, Show)

newtype ColumnID = ColumnID Int deriving (Eq)
instance Show ColumnID where
  show (ColumnID i) = show i

instance FromRow Constraint where
  fromRow =
    Restrict
      <$> (fmap ColumnID field)
      <*> (fmap ColumnID field)
