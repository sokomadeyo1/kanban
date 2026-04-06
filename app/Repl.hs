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
cmdNotFound :: T.Text
cmdNotFound = "Command not found"
cmdsAll :: [CmdString]
cmdsAll =
  [ GetBoard
  , AddEntry
  , MoveEntry
  , AddColumn
  , GetColumns
  , NewTag
  , GetTags
  , TagEntry
  , Help
  ]

data CmdString
  = GetBoard
  | AddEntry
  | MoveEntry
  | AddColumn
  | GetColumns
  | NewTag
  | GetTags
  | TagEntry
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
      else Left $ argErrStr
  MoveEntry ->
    if (length argv >= 2)
      then Right $ Handler.MoveEntry (read $ T.unpack (argv !! 0) :: EntryID) (argv !! 1)
      else Left $ argErrStr
  AddColumn ->
    if (length argv >= 1)
      then Right $ Handler.NewColumn (argv !! 0)
      else Left $ argErrStr
  GetColumns -> Right Handler.GetColumns
  NewTag ->
    if (length argv >= 1)
      then Right $ Handler.NewTag (argv !! 0)
      else Left $ argErrStr
  GetTags -> Right Handler.GetTags
  TagEntry ->
    if (length argv >= 2)
      then Right $ Handler.TagEntry (read $ T.unpack (argv !! 0) :: EntryID) (argv !! 1)
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
helpCmd GetBoard   = "GetBoard   -- show current board's contents"
helpCmd AddEntry   = "AddEntry   -- create a new entry"
helpCmd MoveEntry  = "MoveEntry  -- move an entry to another column"
helpCmd AddColumn  = "AddColumn  -- create a new column"
helpCmd GetColumns = "GetColumns -- show a list of all columns"
helpCmd NewTag     = "NewTag     -- create a new tag"
helpCmd GetTags    = "GetTags    -- show a list of all tags"
helpCmd TagEntry   = "TagEntry   -- add a tag to the entry"
helpCmd Help       = "Help       -- show this message. Use help <cmd> for more details"
helpCmd (Other _)  = ""

usage :: Maybe CmdString -> T.Text
usage (Just GetBoard)    = "usage: GetBoard"
usage (Just AddEntry)    = "usage: AddEntry <entry title> [<entry description>]"
usage (Just MoveEntry)   = "usage: MoveEntry <entry id> <column name>"
usage (Just AddColumn)   = "usage: AddColumn <column name>"
usage (Just GetColumns)  = "usage: GetColumns"
usage (Just NewTag)      = "usage: NewTag <tag name>"
usage (Just GetTags)     = "usage: GetTags"
usage (Just TagEntry)    = "usage: TagEntry <entry id> <tag name>"
usage (Just Help)        = "usage: help [<cmd>]"
usage Nothing            = help
usage (Just (Other cmd)) = help

badCmd :: T.Text -> T.Text
badCmd = T.concat . ([cmdNotFound, ": "] ++) . (: [])
