{-# LANGUAGE OverloadedStrings #-}

module Util.PrettyPrint (Pretty, pretty) where

import qualified Data.Text as T
import Domain.Column
import Domain.Entry
import Domain.Tag

class Pretty a where
  pretty :: a -> T.Text

instance Pretty Entry where
  pretty (Entry eID title "" column) =
    T.unwords [T.show eID, T.concat ["[", column, "]"], title, "\n"]
  pretty (Entry eID title desc column) =
    T.unwords [T.show eID, T.concat ["[", column, "]"], title, "\n\t", desc, "\n"]

instance Pretty Column where
  pretty (Column _ name) = T.concat ["[", name, "]"]

instance Pretty Tag where
  pretty (Tag _ name) = T.concat ["\t<", name, ">"]

instance Pretty EntryID where
  pretty (EntryID i) = T.concat ["#", T.show i]

instance (Pretty a) => Pretty [a] where
  pretty = T.unlines . map pretty

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x, y) = T.unwords [pretty x, pretty y]
