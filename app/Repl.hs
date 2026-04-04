module Repl (run) where

import GHC.Utils.Misc (split)
import qualified Handler
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified ShellWords

prompt = "==> "
argErrStr = "Insufficient number of arguments"
cmdNotFound = "Command not found: "

data CmdString
  = GetBoard
  | AddEntry
  | MoveEntry
  | AddColumn
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

run :: IO ()
run = do
  putStr prompt
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
  Right argv = ShellWords.parse argv1
