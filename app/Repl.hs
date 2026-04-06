{-# LANGUAGE OverloadedStrings #-}

module Repl (run) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

data CmdString
  = GetBoard
  | AddEntry
  | MoveEntry
  | AddColumn
  | Help
  deriving (Read, Show)
newtype Argv = Argv [T.Text]
data UnparsedCall = UnparsedCall (Maybe CmdString) Argv

parse :: UnparsedCall -> Either T.Text Handler.Handler
parse (UnparsedCall cmd argv) =
  case cmd of
    Just cmdstr -> checkArgv argv cmdstr
    Nothing -> Left $ cmdNotFound

checkArgv :: Argv -> CmdString -> Either T.Text Handler.Handler
checkArgv (Argv argv) cmdstr = case cmdstr of
  GetBoard -> Right Handler.GetEntries
  AddEntry ->
    if (length argv >= 2)
      then Right $ Handler.NewEntry (argv !! 0) (argv !! 1)
      else Left $ argErrStr
  MoveEntry ->
    if (length argv >= 2)
      then Right $ Handler.MoveEntry (read $ T.unpack (argv !! 0) :: Int) (argv !! 1)
      else Left $ argErrStr
  AddColumn ->
    if (length argv >= 1)
      then Right $ Handler.NewColumn (argv !! 0)
      else Left $ argErrStr
  Help ->
    if (length argv >= 1)
      then Left $ usage $ readMaybe $ T.unpack (argv !! 0)
      else Left $ help

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
        Left s -> TIO.putStr s
        Right handler -> Handler.handle handler

cmdSplit :: String -> UnparsedCall
cmdSplit cmdline = UnparsedCall (readMaybe $ cmd) (Argv $ map T.pack $ argv)
 where
  (cmd, argv1) = (takeWhile (/= ' ') cmdline, dropWhile (== ' ') $ dropWhile (/= ' ') cmdline)
  argv = case ShellWords.parse argv1 of
    Right arg -> arg
    Left s -> [s]

help :: T.Text
help = T.unlines $ map helpCmd [GetBoard, AddEntry, MoveEntry, AddColumn, Help]

helpCmd :: CmdString -> T.Text
helpCmd GetBoard  = "GetBoard  -- show current board's contents"
helpCmd AddEntry  = "AddEntry  -- create a new entry"
helpCmd MoveEntry = "MoveEntry -- move an entry to another column"
helpCmd AddColumn = "AddColumn -- create a new column"
helpCmd Help      = "Help      -- show this message. Use help <cmd> for more details"

usage :: Maybe CmdString -> T.Text
usage (Just GetBoard)  = "usage: GetBoard"
usage (Just AddEntry)  = "usage: AddEntry <entry title> [<entry description>]"
usage (Just MoveEntry) = "usage: MoveEntry <entry id> <column name>"
usage (Just AddColumn) = "usage: AddColumn <column name>"
usage (Just Help)      = "usage: help [<cmd>]"
usage Nothing          = help
