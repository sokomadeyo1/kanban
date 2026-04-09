module Domain.Tag (
  Tag (..),
  TagID (..),
) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Text.Read
import Util.Class

data Tag = Tag
  { tagID :: TagID
  , tagName :: T.Text
  }
  deriving (Show)

newtype TagID = TagID Int
  deriving (Eq)
instance Show TagID where
  show (TagID i) = show i
instance Read TagID where
  readPrec =
    let i = readPrec :: ReadPrec Int
     in fmap TagID i
instance ToField TagID where
  toField (TagID i) = toField i
instance FromField TagID where
  fromField field_ = fmap TagID $ fromField field_

instance Eq Tag where
  t1 == t2 = tagID t1 == tagID t2
instance Dummy Tag where
  dummy s = Tag (TagID 0) s
instance FromRow Tag where
  fromRow =
    Tag
      <$> (fmap TagID field)
      <*> field
