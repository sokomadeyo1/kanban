{-# LANGUAGE OverloadedStrings #-}

module Repl (run) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Handler
import qualified ShellWords
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Domain.Entry

prompt :: T.Text
prompt = "==> "
argErrStr :: T.Text
argErrStr = "Insufficient number of arguments"
cmdNotFound :: T.Text
cmdNotFound = "Command not found"

data CmdString
  = GetBoard
  | AddEntry
  | MoveEntry
  | AddColumn
  | GetColumns
  | Help
  | Other T.Text
  deriving (Read, Show)
newtype Argv = Argv [T.Text]
data UnparsedCall = UnparsedCall CmdString Argv

cmdAll = [GetBoard, AddEntry, MoveEntry, AddColumn, GetColumns, Help]

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
help = T.unlines $ map helpCmd cmdAll

helpCmd :: CmdString -> T.Text
helpCmd GetBoard   = "GetBoard   -- show current board's contents"
helpCmd AddEntry   = "AddEntry   -- create a new entry"
helpCmd MoveEntry  = "MoveEntry  -- move an entry to another column"
helpCmd AddColumn  = "AddColumn  -- create a new column"
helpCmd GetColumns = "GetColumns -- show a list of new columns"
helpCmd Help       = "Help       -- show this message. Use help <cmd> for more details"
helpCmd (Other _)  = ""

usage :: Maybe CmdString -> T.Text
usage (Just GetBoard)    = "usage: GetBoard"
usage (Just AddEntry)    = "usage: AddEntry <entry title> [<entry description>]"
usage (Just MoveEntry)   = "usage: MoveEntry <entry id> <column name>"
usage (Just AddColumn)   = "usage: AddColumn <column name>"
usage (Just GetColumns)  = "usage: GetColumns"
usage (Just Help)        = "usage: help [<cmd>]"
usage Nothing            = help
usage (Just (Other cmd)) = cmdNotFound

badCmd :: T.Text -> T.Text
badCmd = T.concat . ([cmdNotFound, ": "] ++) . (: [])
