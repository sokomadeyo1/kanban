module Domain.Tag (Tag) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromRow

data Tag = Tag
  { tagID :: TagID
  , tagName :: T.Text
  }
  deriving (Show)

newtype TagID = TagID Int deriving (Eq, Show)
instance Eq Tag where
  t1 == t2 = tagID t1 == tagID t2

instance FromRow Tag where
  fromRow =
    Tag
      <$> (fmap TagID field)
      <*> field
