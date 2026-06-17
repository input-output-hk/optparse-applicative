module Options.Applicative.Help.Ann (
  Ann(..)
  ) where

import Options.Applicative.Help.Style

data Ann
  = AnnStyle SetStyle
  deriving (Eq, Show)
