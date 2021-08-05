{-# LANGUAGE CPP #-}

module Options.Applicative.Help.Pretty
  ( module PP
  , (.$.)
  , groupOrNestLine
  , altSep
  , hangAtIfOver

  , Ann(..)
  , Doc

  , enclose
  , parens
  , brackets
  , hang
  , indent
  , nest

  , text
  , plain
  , deunderline
  , underline
  , debold
  , bold
  , ondullwhite
  , onwhite
  , ondullcyan
  , oncyan
  , ondullmagenta
  , onmagenta
  , ondullblue
  , onblue
  , ondullyellow
  , onyellow
  , ondullgreen
  , ongreen
  , ondullred
  , onred
  , ondullblack
  , onblack
  , dullwhite
  , white
  , dullcyan
  , cyan
  , dullmagenta
  , magenta
  , dullblue
  , blue
  , dullyellow
  , yellow
  , dullgreen
  , green
  , dullred
  , red
  , dullblack
  , black

  -- TODO Remove these
  -- , (<$>)
  , (</>)
  , (<$$>)
  , (<//>)
  , string

  , isEffectivelyEmpty

  , renderShowS
  ) where

import           Control.Applicative
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
#endif

import           Options.Applicative.Help.Ann
import           Prettyprinter hiding ((<>), Doc, enclose, parens, brackets, hang, indent, nest)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Internal as PPI
import           Prettyprinter.Render.String (renderShowS)
import qualified Options.Applicative.Help.Style as S

import           Prelude

type Doc = PPI.Doc Ann

(.$.) :: Doc -> Doc -> Doc
(.$.) x y = annTrace 1 "(.$.)" (x <> line <> y)

-- | Apply the function if we're not at the
--   start of our nesting level.
ifNotAtRoot :: (Doc -> Doc) -> Doc -> Doc
ifNotAtRoot =
  ifElseAtRoot id

-- | Apply the function if we're not at the
--   start of our nesting level.
ifAtRoot :: (Doc -> Doc) -> Doc -> Doc
ifAtRoot =
  flip ifElseAtRoot id

-- | Apply the function if we're not at the
--   start of our nesting level.
ifElseAtRoot :: (Doc -> Doc) -> (Doc -> Doc) -> Doc -> Doc
ifElseAtRoot f g doc =
  PPI.Nesting $ \i ->
    PPI.Column $ \j ->
      if i == j
        then f doc
        else g doc


-- | Render flattened text on this line, or start
--   a new line before rendering any text.
--
--   This will also nest subsequent lines in the
--   group.
groupOrNestLine :: Doc -> Doc
groupOrNestLine d = annTrace 1 "groupOrNestLine" $
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
  group (x <+> PPI.Char '|' <> line) <//> y

-- | Printer hacks to get nice indentation for long commands
--   and subcommands.
--
--   If we're starting this section over the desired width
--   (usually 1/3 of the ribbon), then we will make a line
--   break, indent all of the usage, and go.
--
--   The ifAtRoot is an interesting clause. If this whole
--   operation is put under a `group` then the linebreak
--   will disappear; then item d will therefore not be at
--   the starting column, and it won't be indented more.
hangAtIfOver :: Int -> Int -> Doc -> Doc
hangAtIfOver i j d =
  PPI.Column $ \k ->
    if k <= j then
      align d
    else
      linebreak <> ifAtRoot (indent i) d

-- (<$>) :: Doc -> Doc -> Doc
-- (<$>) = \x y -> x <> line <> y

(</>) :: Doc -> Doc -> Doc
(</>) x y = annTrace 1 "(</>)" $ x <> softline <> y

(<$$>) :: Doc -> Doc -> Doc
(<$$>) x y = annTrace 1 "(<$$>)" $x <> linebreak <> y

(<//>) :: Doc -> Doc -> Doc
(<//>) x y = annTrace 1 "(<//>)" $ x <> softbreak <> y

linebreak :: Doc
linebreak = annTrace 0 "linebreak" $ flatAlt line mempty

softbreak :: Doc
softbreak = annTrace 0 "softbreak" $ group linebreak

-- | Traced version of 'PP.string'.
string :: String -> Doc
string = annTrace 0 "string" . PP.pretty

-- | Traced version of 'PP.parens'.
parens :: Doc -> Doc
parens = annTrace 1 "parens" . PP.parens

-- | Traced version of 'PP.brackets'.
brackets :: Doc -> Doc
brackets = annTrace 1 "brackets" . PP.brackets

-- | Traced version of 'PP.enclose'.
enclose
    :: Doc -- ^ L
    -> Doc -- ^ R
    -> Doc -- ^ x
    -> Doc -- ^ LxR
enclose l r x = annTrace 1 "enclose" (PP.enclose l r x)

-- | Traced version of 'PP.hang'.
hang :: Int -> Doc -> Doc
hang n = annTrace 1 "hang" . PP.hang n

-- | Traced version of 'PP.nest'.
nest :: Int -> Doc -> Doc
nest n = annTrace 1 "nest" . PP.nest n

-- | Traced version of 'PP.indent'.
indent :: Int -> Doc -> Doc
indent n = annTrace 1 "indent" . PP.indent n

-- | Determine if the document is empty when rendered
isEffectivelyEmpty :: Doc -> Bool
isEffectivelyEmpty doc = case doc of
  PPI.Fail -> True
  PPI.Empty -> True
  PPI.Char _ -> False
  PPI.Text _ _ -> False
  PPI.Line -> False
  PPI.FlatAlt _ d -> isEffectivelyEmpty d
  PPI.Cat a b -> isEffectivelyEmpty a && isEffectivelyEmpty b
  PPI.Nest _ d -> isEffectivelyEmpty d
  PPI.Union _ d -> isEffectivelyEmpty d
  PPI.Column _ -> True
  PPI.WithPageWidth _ -> False
  PPI.Nesting _ -> False
  PPI.Annotated _ d -> isEffectivelyEmpty d

-- TODO: Implement properly.  This is needed for compatability.
text :: String -> Doc
text = pretty

-- TODO: Implement properly.  This is needed for compatability.
plain :: Doc -> Doc
plain = id

-- TODO: Implement properly.  This is needed for compatability.
deunderline :: Doc -> Doc
deunderline = id

-- TODO: Implement properly.  This is needed for compatability.
underline :: Doc -> Doc
underline = id

-- TODO: Implement properly.  This is needed for compatability.
debold :: Doc -> Doc
debold = id

-- TODO: Implement properly.  This is needed for compatability.
bold :: Doc -> Doc
bold = id

-- TODO: Implement properly.  This is needed for compatability.
ondullwhite :: Doc -> Doc
ondullwhite = id

-- TODO: Implement properly.  This is needed for compatability.
onwhite :: Doc -> Doc
onwhite = id

-- TODO: Implement properly.  This is needed for compatability.
ondullcyan :: Doc -> Doc
ondullcyan = id

-- TODO: Implement properly.  This is needed for compatability.
oncyan :: Doc -> Doc
oncyan = id

-- TODO: Implement properly.  This is needed for compatability.
ondullmagenta :: Doc -> Doc
ondullmagenta = id

-- TODO: Implement properly.  This is needed for compatability.
onmagenta :: Doc -> Doc
onmagenta = id

-- TODO: Implement properly.  This is needed for compatability.
ondullblue :: Doc -> Doc
ondullblue = id

-- TODO: Implement properly.  This is needed for compatability.
onblue :: Doc -> Doc
onblue = id

-- TODO: Implement properly.  This is needed for compatability.
ondullyellow :: Doc -> Doc
ondullyellow = id

-- TODO: Implement properly.  This is needed for compatability.
onyellow :: Doc -> Doc
onyellow = id

-- TODO: Implement properly.  This is needed for compatability.
ondullgreen :: Doc -> Doc
ondullgreen = id

-- TODO: Implement properly.  This is needed for compatability.
ongreen :: Doc -> Doc
ongreen = id

-- TODO: Implement properly.  This is needed for compatability.
ondullred :: Doc -> Doc
ondullred = id

-- TODO: Implement properly.  This is needed for compatability.
onred :: Doc -> Doc
onred = id

-- TODO: Implement properly.  This is needed for compatability.
ondullblack :: Doc -> Doc
ondullblack = id

-- TODO: Implement properly.  This is needed for compatability.
onblack :: Doc -> Doc
onblack = id

-- TODO: Implement properly.  This is needed for compatability.
dullwhite :: Doc -> Doc
dullwhite = id

-- TODO: Implement properly.  This is needed for compatability.
white :: Doc -> Doc
white = id

-- TODO: Implement properly.  This is needed for compatability.
dullcyan :: Doc -> Doc
dullcyan = id

-- TODO: Implement properly.  This is needed for compatability.
cyan :: Doc -> Doc
cyan = id

-- TODO: Implement properly.  This is needed for compatability.
dullmagenta :: Doc -> Doc
dullmagenta = id

-- TODO: Implement properly.  This is needed for compatability.
magenta :: Doc -> Doc
magenta = id

-- TODO: Implement properly.  This is needed for compatability.
dullblue :: Doc -> Doc
dullblue = id

-- TODO: Implement properly.  This is needed for compatability.
blue :: Doc -> Doc
blue = id

-- TODO: Implement properly.  This is needed for compatability.
dullyellow :: Doc -> Doc
dullyellow = id

-- TODO: Implement properly.  This is needed for compatability.
yellow :: Doc -> Doc
yellow = id

-- TODO: Implement properly.  This is needed for compatability.
dullgreen :: Doc -> Doc
dullgreen = id

-- TODO: Implement properly.  This is needed for compatability.
green :: Doc -> Doc
green = id

-- TODO: Implement properly.  This is needed for compatability.
dullred :: Doc -> Doc
dullred = id

red :: Doc -> Doc
red = annotate (AnnStyle (S.color S.Red))

-- TODO: Implement properly.  This is needed for compatability.
dullblack :: Doc -> Doc
dullblack = id

black :: Doc -> Doc
black = annotate (AnnStyle (S.color S.Black))
