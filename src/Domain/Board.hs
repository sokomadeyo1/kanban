module Domain.Board (
  Board (..),
  BoardID (..),
) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField

data Board = Board
  { boardID :: BoardID
  , boardName :: T.Text
  , boardDesc :: T.Text
  -- , defaultCol :: ColumnID
  -- , boardUsers :: [UserID]
  }

newtype BoardID = BoardID Int
  deriving (Eq)
instance Show BoardID where
  show (BoardID i) = show i
instance ToField BoardID where
  toField (BoardID i) = toField i
instance FromField BoardID where
  fromField field_ = fmap BoardID $ fromField field_

instance Eq Board where
  b1 == b2 = boardID b1 == boardID b2
instance FromRow Board where
  fromRow =
    Board
      <$> (fmap BoardID field)
      <*> field
      <*> field
