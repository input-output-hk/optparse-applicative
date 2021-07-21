{-# LANGUAGE CPP #-}

module Options.Applicative.Help.Pretty
  ( module Text.PrettyPrint.ANSI.Leijen
  , (.$.)
  , groupOrNestLine
  , altSep

  , isEffectivelyEmpty
  ) where

import           Control.Applicative
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
#endif

import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), columns)
import           Text.PrettyPrint.ANSI.Leijen.Internal (Doc (..), flatten)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Prelude

(.$.) :: Doc -> Doc -> Doc
(.$.) = (PP.<$>)

-- | Apply the function if we're not at the
--   start of our nesting level.
ifNotAtRoot :: (Doc -> Doc) -> Doc -> Doc
ifNotAtRoot f doc =
  Nesting $ \i ->
    Column $ \j ->
      if i == j
        then doc
        else f doc


-- | Render flattened text on this line, or start
--   a new line before rendering any text.
--
--   This will also nest subsequent lines in the
--   group.
groupOrNestLine :: Doc -> Doc
groupOrNestLine =
  Union
    <$> flatten
    <*> ifNotAtRoot (line <>)

-- | Separate items in an alternative with a pipe.
--
--   If the first document and the pipe don't fit
--   on the line, then mandatorily flow the next entry
--   onto the following line.
--
--   The (<//>) softbreak ensures that if the document
--   does fit on the line, there is at least a space,
--   but it's possible for y to still appear on the
--   next line.
altSep :: Doc -> Doc -> Doc
altSep x y =
  group (x <+> char '|' <> line) <//> y

-- | Determine if the document is empty when rendered
isEffectivelyEmpty :: Doc -> Bool
isEffectivelyEmpty doc = case doc of
  Fail -> True
  Empty -> True
  Char _ -> False
  Text _ _ -> False
  Line -> False
  FlatAlt _ d -> isEffectivelyEmpty d
  Cat a b -> isEffectivelyEmpty a && isEffectivelyEmpty b
  Nest _ d -> isEffectivelyEmpty d
  Union _ d -> isEffectivelyEmpty d
  Column _ -> True
  Columns _ -> True
  Nesting _ -> False
  Color _ _ _ d -> isEffectivelyEmpty d
  Intensify _ d -> isEffectivelyEmpty d
  Italicize _ d -> isEffectivelyEmpty d
  Underline _ d -> isEffectivelyEmpty d
  RestoreFormat {} -> True
