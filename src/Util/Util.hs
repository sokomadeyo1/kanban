module Util.Util (columnEntries) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag

columnEntries :: T.Text -> [(Entry, [Tag])] -> [(Entry, [Tag])]
columnEntries colname = filter (\(Entry _ _ _ entrycol, _) -> entrycol == colname)
