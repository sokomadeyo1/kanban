{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Domain.Column (
  Column (..),
  ColumnID (..),
  defaultColumnNames,
) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

defaultColumnNames :: [T.Text]
defaultColumnNames = ["Backlog", "Ready", "In progress", "Review", "Done"]

data Column = Column
  { columnID :: ColumnID
  -- , columnBoard :: BoardID
  , columnTitle :: T.Text
  }
  deriving (Show)

newtype ColumnID = ColumnID Int
  deriving (Eq)
  deriving (FromField) via Int
instance Show ColumnID where
  show (ColumnID i) = show i
instance Eq Column where
  c1 == c2 = columnID c1 == columnID c2

instance FromRow Column where
  fromRow =
    Column
      <$> (fmap ColumnID field)
      <*> field

instance ToField ColumnID where
  toField (ColumnID i) = toField i
