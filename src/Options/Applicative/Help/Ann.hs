{-# LANGUAGE FlexibleInstances #-}

module Options.Applicative.Help.Ann (
  Ann(..),
  CanAnnotate(..)
  ) where

import Prettyprinter (Doc, annotate)
import Options.Applicative.Help.Style

data Ann
  = AnnTrace Int String
  | AnnStyle SetStyle
  deriving (Eq, Show)

class CanAnnotate a where
  -- | Annotate trace a value
  annTrace
    :: Int      -- ^ Trace level
    -> String   -- ^ Trace message
    -> a        -- ^ Value to be traced
    -> a        -- ^ The traced value

instance CanAnnotate (Doc Ann) where
  annTrace n = annotate . AnnTrace n
