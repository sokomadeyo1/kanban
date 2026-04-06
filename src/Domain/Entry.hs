module Domain.Entry (Entry (..)) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromRow

data Entry = Entry
  { entryID :: EntryID
  , entryTitle :: T.Text
  , entryDesc :: T.Text
  , entryColName :: T.Text
  -- , entryTags :: [TagID]
  -- , entryAssignee :: [UserID]
  }
  deriving (Show)

newtype EntryID = EntryID Int deriving (Eq)
instance Show EntryID where
  show (EntryID i) = show i
instance Eq Entry where
  e1 == e2 = entryID e1 == entryID e2

instance FromRow Entry where
  fromRow =
    Entry
      <$> (fmap EntryID field)
      <*> field
      <*> field
      <*> field
