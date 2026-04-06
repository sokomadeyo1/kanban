{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint (Pretty, pretty) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Column

class Pretty a where
  pretty :: a -> T.Text

instance Pretty Entry where
  pretty (Entry eID title desc column) =
    T.unwords [T.show eID, T.concat ["(", column, ")"], title, desc]

instance Pretty Column where
  pretty (Column _ name) = name

instance (Pretty a) => Pretty [a] where
  pretty = T.unlines . map pretty
