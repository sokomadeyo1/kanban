module Domain.User (
  User (..),
  UserID (..),
) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow

data User = User
  { userID :: UserID
  , userName :: T.Text
  -- , userBoards :: [BoardID]
  -- , userEntries :: [EntryID]
  }
  deriving (Show)

newtype UserID = UserID Int
  deriving (Eq)
instance Show UserID where
  show (UserID i) = show i
instance FromField UserID where
  fromField field_ = fmap UserID $ fromField field_

instance Eq User where
  u1 == u2 = userID u1 == userID u2
instance FromRow User where
  fromRow =
    User
      <$> (fmap UserID field)
      <*> field
