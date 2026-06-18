{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Options.Applicative.Help.Pretty
  ( module PP
  , module Prettyprinter.Render.Terminal

  , Doc
  , SimpleDoc

  , (.$.)
  , (</>)

  , groupOrNestLine
  , altSep
  , hangAtIfOver

  , enclose
  , parens
  , brackets
  , hang
  , indent
  , nest

  , text
  , plain
  , string

  , (<$$>)
  , (<//>)

  , isEffectivelyEmpty

  , prettyString
  , streamToString
  ) where

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>), mempty)
#endif
import qualified Data.Text.Lazy as Lazy

import           Prettyprinter hiding ((<>), Doc, enclose, parens, brackets, hang, indent, nest)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Internal as PPI
import           Prettyprinter.Render.Terminal

import           Prelude

type Doc = PP.Doc AnsiStyle
type SimpleDoc = SimpleDocStream AnsiStyle

indent :: Int -> Doc -> Doc
indent = PP.indent

(.$.) :: Doc -> Doc -> Doc
(.$.) x y = x <> line <> y

ifNotAtRoot :: (Doc -> Doc) -> Doc -> Doc
ifNotAtRoot = ifElseAtRoot id

ifAtRoot :: (Doc -> Doc) -> Doc -> Doc
ifAtRoot = flip ifElseAtRoot id

ifElseAtRoot :: (Doc -> Doc) -> (Doc -> Doc) -> Doc -> Doc
ifElseAtRoot f g doc =
  PPI.Nesting $ \i ->
    PPI.Column $ \j ->
      if i == j
        then f doc
        else g doc

-- | Render flattened text on this line, or start a new line before rendering
--   any text, nesting subsequent lines in the group.
groupOrNestLine :: Doc -> Doc
groupOrNestLine d =
  (PPI.Union
    <$> flatten
    <*> ifNotAtRoot (line <>)) d
  where flatten :: Doc -> Doc
        flatten doc = case doc of
          PPI.FlatAlt _ y     -> flatten y
          PPI.Cat x y         -> PPI.Cat (flatten x) (flatten y)
          PPI.Nest i x        -> PPI.Nest i (flatten x)
          PPI.Line            -> PPI.Fail
          PPI.Union x _       -> flatten x
          PPI.Column f        -> PPI.Column (flatten . f)
          PPI.WithPageWidth f -> PPI.WithPageWidth (flatten . f)
          PPI.Nesting f       -> PPI.Nesting (flatten . f)
          PPI.Annotated ann x -> PPI.Annotated ann (flatten x)

          x@PPI.Fail   -> x
          x@PPI.Empty  -> x
          x@PPI.Char{} -> x
          x@PPI.Text{} -> x

-- | Separate items in an alternative with a pipe.
altSep :: Doc -> Doc -> Doc
altSep x y =
  group (x <+> PPI.Char '|' <> line) <//> y

-- | Hang at column j if we're over it, otherwise align.
hangAtIfOver :: Int -> Int -> Doc -> Doc
hangAtIfOver i j d =
  PPI.Column $ \k ->
    if k <= j then
      align d
    else
      linebreak <> ifAtRoot (indent i) d

(</>) :: Doc -> Doc -> Doc
(</>) x y = x <> softline <> y

(<$$>) :: Doc -> Doc -> Doc
(<$$>) x y = x <> linebreak <> y

(<//>) :: Doc -> Doc -> Doc
(<//>) x y = x <> softbreak <> y

linebreak :: Doc
linebreak = flatAlt line mempty

softbreak :: Doc
softbreak = group linebreak

string :: String -> Doc
string = PP.pretty

parens :: Doc -> Doc
parens = PP.parens

brackets :: Doc -> Doc
brackets = PP.brackets

enclose :: Doc -> Doc -> Doc -> Doc
enclose = PP.enclose

hang :: Int -> Doc -> Doc
hang = PP.hang

nest :: Int -> Doc -> Doc
nest = PP.nest

isEffectivelyEmpty :: Doc -> Bool
isEffectivelyEmpty doc = case doc of
  PPI.Fail          -> True
  PPI.Empty         -> True
  PPI.Char _        -> False
  PPI.Text _ _      -> False
  PPI.Line          -> False
  PPI.FlatAlt _ d   -> isEffectivelyEmpty d
  PPI.Cat a b       -> isEffectivelyEmpty a && isEffectivelyEmpty b
  PPI.Nest _ d      -> isEffectivelyEmpty d
  PPI.Union _ d     -> isEffectivelyEmpty d
  PPI.Column _      -> True
  PPI.WithPageWidth _ -> False
  PPI.Nesting _     -> False
  PPI.Annotated _ d -> isEffectivelyEmpty d

prettyString :: Double -> Int -> Doc -> String
prettyString ribbonFraction lineWidth
  = streamToString
  . layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine lineWidth ribbonFraction }

streamToString :: SimpleDoc -> String
streamToString = Lazy.unpack . Prettyprinter.Render.Terminal.renderLazy

text :: String -> Doc
text = pretty

plain :: Doc -> Doc
plain = unAnnotate