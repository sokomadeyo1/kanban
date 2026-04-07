{-# LANGUAGE OverloadedStrings #-}

module Repl (run) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Domain.Entry
import qualified Handler
import qualified ShellWords
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

prompt :: T.Text
prompt = "==> "
argErrStr :: T.Text
argErrStr = "Insufficient number of arguments"
typeErrStr :: T.Text
typeErrStr = "Incorrect arguments. Use help to see command usage"
cmdNotFound :: T.Text
cmdNotFound = "Command not found"
cmdsAll :: [CmdString]
cmdsAll =
  [ GetBoard
  , AddEntry
  , RenameEntry
  , EditEntry
  , MoveEntry
  , DelEntry
  , AddColumn
  , RenameColumn
  , ShowColumn
  , GetColumns
  , DelColumn
  , NewTag
  , RenameTag
  , GetTags
  , ByTag
  , TagEntry
  , UntagEntry
  , DeleteTag
  , Help
  ]

data CmdString
  = GetBoard
  | AddEntry
  | RenameEntry
  | EditEntry
  | MoveEntry
  | DelEntry
  | AddColumn
  | RenameColumn
  | ShowColumn
  | GetColumns
  | NewTag
  | RenameTag
  | GetTags
  | ByTag
  | TagEntry
  | UntagEntry
  | DeleteTag
  | DelColumn
  | Help
  | Other T.Text
  deriving (Read, Show)
newtype Argv = Argv [T.Text]
data UnparsedCall = UnparsedCall CmdString Argv

parse :: UnparsedCall -> Either T.Text Handler.Handler
parse (UnparsedCall cmdstr (Argv argv)) = case cmdstr of
  GetBoard -> Right Handler.GetEntries
  AddEntry ->
    if (length argv >= 2)
      then Right $ Handler.NewEntry (argv !! 0) (argv !! 1)
      else if (length argv >= 1)
        then Right $ Handler.NewEntry (argv !! 0) ""
        else Left $ argErrStr
  RenameEntry ->
    if (length argv >= 2)
      then case (readMaybe $ T.unpack (argv !! 0) :: Maybe EntryID) of
        Nothing -> Left $ typeErrStr
        Just i -> Right $ Handler.RenameEntry i (argv !! 1)
      else Left $ argErrStr
  EditEntry ->
    if (length argv >= 2)
      then case (readMaybe $ T.unpack (argv !! 0) :: Maybe EntryID) of
        Nothing -> Left $ typeErrStr
        Just i -> Right $ Handler.EditEntry i (argv !! 1)
      else Left $ argErrStr
  MoveEntry ->
    if (length argv >= 2)
      then case (readMaybe $ T.unpack (argv !! 0) :: Maybe EntryID) of
        Nothing -> Left $ typeErrStr
        Just i -> Right $ Handler.MoveEntry i (argv !! 1)
      else Left $ argErrStr
  DelEntry ->
    if (length argv >= 1)
      then case (readMaybe $ T.unpack (argv !! 0) :: Maybe EntryID) of
        Nothing -> Left $ typeErrStr
        Just i -> Right $ Handler.DeleteEntry i
      else Left $ argErrStr
  AddColumn ->
    if (length argv >= 1)
      then Right $ Handler.NewColumn (argv !! 0)
      else Left $ argErrStr
  RenameColumn ->
    if (length argv >= 2)
      then Right $ Handler.RenameColumn (argv !! 0) (argv !! 1)
      else Left $ argErrStr
  GetColumns -> Right Handler.GetColumns
  ShowColumn ->
    if (length argv >= 1)
      then Right $ Handler.ShowColumn (argv !! 0)
      else Left $ argErrStr
  DelColumn ->
    if (length argv >= 1)
      then Right $ Handler.DeleteColumn (argv !! 0)
      else Left $ argErrStr
  NewTag ->
    if (length argv >= 1)
      then Right $ Handler.NewTag (argv !! 0)
      else Left $ argErrStr
  RenameTag ->
    if (length argv >= 2)
      then Right $ Handler.RenameTag (argv !! 0) (argv !! 1)
      else Left $ argErrStr
  GetTags -> Right Handler.GetTags
  ByTag ->
    if (length argv >= 1)
      then Right $ Handler.EntriesByTag (argv !! 0)
      else Left $ argErrStr
  TagEntry ->
    if (length argv >= 2)
      then case (readMaybe $ T.unpack (argv !! 0) :: Maybe EntryID) of
        Nothing -> Left $ typeErrStr
        Just i -> Right $ Handler.TagEntry i (argv !! 1)
      else Left $ argErrStr
  UntagEntry ->
    if (length argv >= 2)
      then case (readMaybe $ T.unpack (argv !! 0) :: Maybe EntryID) of
        Nothing -> Left $ typeErrStr
        Just i -> Right $ Handler.UntagEntry i (argv !! 1)
      else Left $ argErrStr
  DeleteTag ->
    if (length argv >= 1)
      then Right $ Handler.DeleteTag (argv !! 0)
      else Left $ argErrStr
  Help ->
    if (length argv >= 1)
      then Left $ usage $ readMaybe $ T.unpack (argv !! 0)
      else Left $ help
  Other err -> Left err

run :: IO ()
run = do
  TIO.putStr prompt
  hFlush stdout
  cmdline <- getLine
  if (length cmdline) <= 1
    then return ()
    else do
      let unparsed = cmdSplit cmdline
      let parsed = parse unparsed
      case parsed of
        Left s -> TIO.putStrLn s
        Right handler -> Handler.handle handler

cmdSplit :: String -> UnparsedCall
cmdSplit cmdline =
  case (readMaybe cmd :: Maybe CmdString) of
    Nothing -> UnparsedCall (Other bad) (Argv [])
    Just x -> case argv of
      Left err -> UnparsedCall x $ Argv [T.pack err]
      Right args -> UnparsedCall x (Argv $ map T.pack args)
 where
  cmd = takeWhile (/= ' ') cmdline
  argv1 = dropWhile (== ' ') $ dropWhile (/= ' ') cmdline
  argv = ShellWords.parse argv1
  bad = badCmd $ T.pack cmd

help :: T.Text
help = T.unlines $ map helpCmd cmdsAll

helpCmd :: CmdString -> T.Text
helpCmd GetBoard     = "GetBoard     -- show current board's contents"
helpCmd AddEntry     = "AddEntry     -- create a new entry"
helpCmd RenameEntry  = "RenameEntry  -- change the title of an entry"
helpCmd EditEntry    = "EditEntry    -- change description of an entry"
helpCmd MoveEntry    = "MoveEntry    -- move an entry to another column"
helpCmd DelEntry     = "DelEntry     -- delete an entry"
helpCmd AddColumn    = "AddColumn    -- create a new column"
helpCmd RenameColumn = "RenameColumn -- change the name of a column"
helpCmd ShowColumn   = "ShowColumn   -- list entries from one column"
helpCmd GetColumns   = "GetColumns   -- show a list of all columns"
helpCmd DelColumn    = "DelColumn    -- delete a column"
helpCmd NewTag       = "NewTag       -- create a new tag"
helpCmd RenameTag    = "RenameTag    -- change the name of a tag"
helpCmd GetTags      = "GetTags      -- show a list of all tags"
helpCmd ByTag        = "ByTag        -- get all entries with specified tag"
helpCmd TagEntry     = "TagEntry     -- add a tag to the entry"
helpCmd UntagEntry   = "UntagEntry   -- remove a tag from the entry"
helpCmd DeleteTag    = "DeleteTag    -- delete the specified tag"
helpCmd Help         = "Help         -- show this message. Use help <cmd> for more details"
helpCmd (Other _)    = ""

usage :: Maybe CmdString -> T.Text
usage (Just GetBoard)     = "usage: GetBoard"
usage (Just AddEntry)     = "usage: AddEntry <entry title> [<entry description>]"
usage (Just RenameEntry)  = "usage: RenameEntry <entry id> <new entry title>"
usage (Just EditEntry)    = "usage: EditEntry <entry id> <new entry description>"
usage (Just MoveEntry)    = "usage: MoveEntry <entry id> <column name>"
usage (Just DelEntry)     = "usage: DelEntry <entry id>"
usage (Just AddColumn)    = "usage: AddColumn <column name>"
usage (Just RenameColumn) = "usage: RenameColumn <old column name> <new column name>"
usage (Just ShowColumn)   = "usage: ShowColumn <column name>"
usage (Just GetColumns)   = "usage: GetColumns"
usage (Just DelColumn)    = "usage: DelColumn <column name>"
usage (Just NewTag)       = "usage: NewTag <tag name>"
usage (Just RenameTag)    = "usage: RenameTag <old tag name> <new tag name>"
usage (Just GetTags)      = "usage: GetTags"
usage (Just ByTag)        = "usage: ByTag <tag name>"
usage (Just TagEntry)     = "usage: TagEntry <entry id> <tag name>"
usage (Just UntagEntry)   = "usage: UntagEntry <entry id> <tag name>"
usage (Just DeleteTag)    = "usage: DeleteTag <tag name>"
usage (Just Help)         = "usage: help [<cmd>]"
usage Nothing             = help
usage (Just (Other _))    = help

badCmd :: T.Text -> T.Text
badCmd = T.concat . ([cmdNotFound, ": "] ++) . (: [])
