module Util.Cast (
  castTaggedEntry,
  getid,
  maybeToMonoid,
) where

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

maybeToMonoid :: (Monoid m) => Maybe m -> m
maybeToMonoid Nothing = mempty
maybeToMonoid (Just x) = x

getid :: Entry -> Int
getid = (\(EntryID i) -> i) . entryID
