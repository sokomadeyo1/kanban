module Util.Class (Dummy (..)) where

import qualified Data.Text as T

class Dummy a where
  dummy :: T.Text -> a
