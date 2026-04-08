{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Domain.Tag (
  Tag (..),
  TagID (..),
) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Text.Read

data Tag = Tag
  { tagID :: TagID
  , tagName :: T.Text
  }
  deriving (Show)

newtype TagID = TagID Int
  deriving (Eq)
  deriving (FromField) via Int
instance Show TagID where
  show (TagID i) = show i
instance Read TagID where
  readPrec =
    let i = readPrec :: ReadPrec Int
     in fmap TagID i
instance Eq Tag where
  t1 == t2 = tagID t1 == tagID t2

instance FromRow Tag where
  fromRow =
    Tag
      <$> (fmap TagID field)
      <*> field

instance ToField TagID where
  toField (TagID i) = toField i
