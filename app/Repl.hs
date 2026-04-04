module Repl (run) where

import GHC.Utils.Misc (split)
import qualified Handler (Handler (..), handle)
import Text.Read (readMaybe)

prompt = "==> "
argErrStr = "Insufficient number of arguments"
cmdNotFound = "Command not found: "

newtype CmdString
  = GetBoard
  | AddEntry
  | MoveEntry
  | AddColumn
  deriving (Read)
newtype Argv = Argv [String]
data UnparsedCall = UnparsedCall (Maybe CmdString) Argv

parse :: UnparsedCall -> Either String Handler
parse (UnparsedCall cmd argv)
  = case cmd of
    Just cmdstr -> checkArgv Argv cmdstr
    Nothing -> Left $ cmdNotFound ++ cmd

checkArgv :: Argv -> CmdString -> Either String Handler
checkArgv (Argv argv) cmdstr = case cmdstr of
  GetBoard -> Right Handler.GetEntries
  AddEntry -> if (length argv >= 2)
    then Right $ Handler.NewEntry (argv !! 0) (argv !! 1)
    else Left $ argErrStr
  MoveEntry -> if (length argv >= 2)
    then Right $ Handler.MoveEntry (read argv !! 0 :: Int) (argv !! 1)
    else Left $ argErrStr
  AddColumnt -> if (length argv >= 1)
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
        Right handler -> handle handler

-- TODO: write smarter cmdSplit
cmdSplit :: String -> UnparsedCall
cmdSplit cmdline = UnparsedCall (readMaybe $ argv !! 0) (Argv $ [concat . tail argv])
 where
  argv = split ' ' cmdline
