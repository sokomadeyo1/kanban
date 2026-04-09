{-# LANGUAGE OverloadedStrings #-}

module Domain.Column (
  Column (..),
  ColumnID (..),
  defaultColumnNames,
) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Util.Class

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
instance Show ColumnID where
  show (ColumnID i) = show i
instance ToField ColumnID where
  toField (ColumnID i) = toField i
instance FromField ColumnID where
  fromField fmap_ = fmap ColumnID $ fromField fmap_

instance Eq Column where
  c1 == c2 = columnID c1 == columnID c2
instance Dummy Column where
  dummy s = Column (ColumnID 0) s
instance FromRow Column where
  fromRow =
    Column
      <$> (fmap ColumnID field)
      <*> field
