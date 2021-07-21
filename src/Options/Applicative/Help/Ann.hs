{-# LANGUAGE FlexibleInstances #-}

module Options.Applicative.Help.Ann (
  Ann(..),
  CanAnnotate(..)
  ) where

import Prettyprinter (Doc, annotate)

data Ann = AnnTrace
  Int     -- ^ Trace level
  String  -- ^ Trace message
  deriving (Eq, Show)

-- | The minimum trace level for tracing to be included
minTraceLevel :: Int
minTraceLevel = 2

class CanAnnotate a where
  annTrace :: Int -> String -> a -> a

instance CanAnnotate (Doc Ann) where
  annTrace n = if n >= minTraceLevel then annotate . AnnTrace n else const id
