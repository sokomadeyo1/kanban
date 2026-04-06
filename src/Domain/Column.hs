module Domain.Column (Column, defaultColumnNames) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromRow

defaultColumnNames :: [String]
defaultColumnNames = ["Backlog", "Ready", "In progress", "Review", "Done"]

data Column = Column
  { columnID :: ColumnID
  , -- , columnBoard :: BoardID
    columnTitle :: T.Text
  }
  deriving (Show)

newtype ColumnID = ColumnID Int deriving (Eq, Show)
instance Eq Column where
  c1 == c2 = columnID c1 == columnID c2

instance FromRow Column where
  fromRow =
    Column
      <$> (fmap ColumnID field)
      <*> field
