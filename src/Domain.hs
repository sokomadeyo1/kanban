module Domain where

import qualified Data.Text as T
import Database.SQLite.Simple.FromRow

data Entry = Entry
  { entryID :: EntryID
  , entryTitle :: T.Text
  , entryDesc :: T.Text
  , entryCol :: ColumnID
  , entryColName :: T.Text
  -- , entryTags :: [TagID]
  -- , entryAssignee :: [UserID]
  }
  deriving (Show)

-- data Tag = Tag
--   { tagID :: TagID
--   , tagName :: T.Text
--   }
--   deriving (Show)

-- data User = User
--   { userID :: UserID
--   , userName :: T.Text
--   , userBoards :: [BoardID]
--   , userEntries :: [EntryID]
--   }
--   deriving (Show)

data Column = Column
  { columnID :: ColumnID
  , -- , columnBoard :: BoardID
    columnTitle :: T.Text
  }
  deriving (Show)

defaultColumnNames :: [String]
defaultColumnNames = ["Backlog", "Ready", "In progress", "Review", "Done"]

-- data Board = Board
--   { boardID :: BoardID
--   , boardName :: T.Text
--   , boardDesc :: Maybe T.Text
--   , defaultCol :: ColumnID
--   , boardConstraints :: [Constraint]
--   , boardUsers :: [UserID]
--   }

-- data Constraint = Restrict ColumnID ColumnID deriving (Eq)

newtype EntryID = EntryID Int deriving (Eq, Show)
-- newtype TagID = TagID Int deriving (Eq, Show)
-- newtype UserID = UserID Int deriving (Eq, Show)
newtype ColumnID = ColumnID Int deriving (Eq, Show)
-- newtype BoardID = BoardID Int deriving (Eq, Show)

instance Eq Entry where
  e1 == e2 = entryID e1 == entryID e2
-- instance Eq Tag where
--   t1 == t2 = tagID t1 == tagID t2
-- instance Eq User where
--   u1 == u2 = userID u1 == userID u2
instance Eq Column where
  c1 == c2 = columnID c1 == columnID c2
-- instance Eq Board where
--   b1 == b2 = boardID b1 == boardID b2

instance FromRow Entry where
  fromRow =
    Entry
      <$> (fmap EntryID field)
      <*> field
      <*> field
      <*> (fmap ColumnID field)
      <*> field

instance FromRow Column where
  fromRow =
    Column
      <$> (fmap ColumnID field)
      <*> field

-- -- * Creating
--
-- createEntry :: String -> String -> IO Entry
-- createEntry name desc = undefined
--
-- createTag :: String -> IO Tag
-- createTag name = undefined
--
-- createUser :: String -> IO User
-- createUser name = undefined
--
-- createCol :: String -> Board -> IO Column
-- createCol name board = undefined
--
-- createBoard :: String -> String -> IO Board
-- createBoard name desc = undefined
--
-- -- * Entry actions
--
-- renameEntry :: Entry -> String -> String -> Entry
-- renameEntry entry newName "" = undefined
-- renameEntry entry newName newDesc = undefined
--
-- assign :: Entry -> User -> Entry
-- assign entry assignee = undefined
--
-- unassign :: Entry -> Entry
-- unassign entry = undefined
--
-- moveEntry :: Entry -> Column -> Entry
-- moveEntry entry to = undefined
--
-- removeEntry :: Entry -> Entry
-- removeEntry entry = undefined
--
-- -- * Tag actions
--
-- addTag :: Tag -> Entry -> Entry
-- addTag tag entry = undefined
--
-- renameTag :: Tag -> String -> Tag
-- renameTag tag newName = undefined
--
-- removeTag :: Tag -> Entry -> Entry
-- removeTag tag entry = undefined
--
-- -- * Users actions
--
-- renameUser :: User -> String -> User
-- renameUser user newName = undefined
--
-- -- * Column actions
--
-- renameColumn :: Column -> String -> Column
-- renameColumn col newName = undefined
--
-- -- * Board actions
--
-- setDefaultCol :: Board -> Column -> Board
-- setDefaultCol board col = undefined
--
-- addUser :: Board -> User -> Board
-- addUser board user = undefined
--
-- removeUser :: Board -> User -> Board
-- removeUser board user = undefined
--
-- reorderColumns :: Board -> Int -> Int -> Board
-- reorderColumns board i j = undefined
--
