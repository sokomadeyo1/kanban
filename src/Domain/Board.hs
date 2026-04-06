module Domain.Board (Board) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromRow

data Board = Board
  { boardID :: BoardID
  , boardName :: T.Text
  , boardDesc :: T.Text
  -- , defaultCol :: ColumnID
  -- , boardUsers :: [UserID]
  }

newtype BoardID = BoardID Int deriving (Eq, Show)
instance Eq Board where
  b1 == b2 = boardID b1 == boardID b2

instance FromRow Board where
  fromRow =
    Board
      <$> (fmap BoardID field)
      <*> field
      <*> field
