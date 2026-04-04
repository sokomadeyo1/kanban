module Repl (run) where

import GHC.Utils.Misc (split)
import qualified Handler
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified ShellWords
import System.IO (hFlush, stdout)

prompt = "==> "
argErrStr = "Insufficient number of arguments"
cmdNotFound = "Command not found: "

data CmdString
  = GetBoard
  | AddEntry
  | MoveEntry
  | AddColumn
  | Help
  deriving (Read, Show)
newtype Argv = Argv [T.Text]
data UnparsedCall = UnparsedCall (Maybe CmdString) Argv

parse :: UnparsedCall -> Either String Handler.Handler
parse (UnparsedCall cmd argv)
  = case cmd of
    Just cmdstr -> checkArgv argv cmdstr
    Nothing -> Left $ cmdNotFound ++ show cmd

checkArgv :: Argv -> CmdString -> Either String Handler.Handler
checkArgv (Argv argv) cmdstr = case cmdstr of
  GetBoard -> Right Handler.GetEntries
  AddEntry -> if (length argv >= 2)
    then Right $ Handler.NewEntry (argv !! 0) (argv !! 1)
    else Left $ argErrStr
  MoveEntry -> if (length argv >= 2)
    then Right $ Handler.MoveEntry (read $ T.unpack (argv !! 0) :: Int) (argv !! 1)
    else Left $ argErrStr
  AddColumn -> if (length argv >= 1)
    then Right $ Handler.NewColumn (argv !! 0)
    else Left $ argErrStr
  Help -> if (length argv >= 1)
    then Left $ usage $ readMaybe $ T.unpack (argv !! 0)
    else Left $ help

run :: IO ()
run = do
  putStr prompt
  hFlush stdout
  cmdline <- getLine
  if (length cmdline) <= 1
    then return ()
    else do
      let unparsed = cmdSplit cmdline
      let parsed = parse unparsed
      case parsed of
        Left s -> print s
        Right handler -> Handler.handle handler

cmdSplit :: String -> UnparsedCall
cmdSplit cmdline = UnparsedCall (readMaybe $ cmd) (Argv $ map T.pack $ argv)
 where
  (cmd, argv1) = (takeWhile (/= ' ') cmdline, dropWhile (== ' ') $ dropWhile (/= ' ') cmdline)
  argv = case ShellWords.parse argv1 of
    Right arg -> arg
    Left s -> [s]

help :: String
help = unlines $ map helpCmd [GetBoard, AddEntry, MoveEntry, AddColumn, Help]

helpCmd :: CmdString -> String
helpCmd GetBoard  = "GetBoard  -- show current board's contents"
helpCmd AddEntry  = "AddEntry  -- create a new entry"
helpCmd MoveEntry = "MoveEntry -- move an entry to another column"
helpCmd AddColumn = "AddColumn -- create a new column"
helpCmd Help      = "Help      -- show this message. Use help <cmd> for more details"

usage :: Maybe CmdString -> String
usage (Just GetBoard)  = "usage: GetBoard"
usage (Just AddEntry)  = "usage: AddEntry <entry title> [<entry description>]"
usage (Just MoveEntry) = "usage: MoveEntry <entry id> <column name>"
usage (Just AddColumn) = "usage: AddColumn <column name>"
usage (Just Help)      = "usage: help [<cmd>]"
usage Nothing          = help
