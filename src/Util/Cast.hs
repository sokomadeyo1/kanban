module Util.Cast (castTaggedEntry) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag

castTaggedEntry :: (EntryID, T.Text, T.Text, T.Text, String, String) -> (Entry, [Tag])
castTaggedEntry (eid, title, desc, col, tagids_, tagnames_) =
  ((Entry eid title desc col), zipWith Tag tagids tagnames)
 where
  tagids = read tagids_
  tagnames = read tagnames_
