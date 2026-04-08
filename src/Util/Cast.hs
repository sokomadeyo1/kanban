module Util.Cast (castTaggedEntry) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag

castTaggedEntry :: (EntryID, T.Text, T.Text, T.Text, String, String) -> (Entry, [Tag])
castTaggedEntry (eid, title, desc, col, tagids_, tagnames_) =
  ((Entry eid title desc col), zipWith Tag tagids tagnames)
 where
  tagids = case tagids_ of
    "[null]" -> []
    _ -> read tagids_
  tagnames = case tagnames_ of
    "[null]" -> []
    _ -> read tagnames_
