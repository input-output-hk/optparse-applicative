{-# LANGUAGE ScopedTypeVariables #-}

module Options.ApplicativeAlt.Help.Core (
  cmdDesc,
  briefDesc,
  missingDesc,
  fullDesc,
  globalDesc,
  ParserHelp(..),
  errorHelp,
  headerHelp,
  suggestionsHelp,
  usageHelp,
  bodyHelp,
  footerHelp,
  globalsHelp,
  parserHelp,
  parserUsage,
  parserGlobals
  ) where

import Control.Applicative
import Control.Monad (guard)
import Data.Bifunctor
import Data.Function (on)
import Data.List (sort, intersperse, groupBy)
import Data.Foldable (any, foldl')
import Data.Maybe (maybeToList, catMaybes, fromMaybe)
import Data.Monoid (mempty)
import Data.Semigroup (Semigroup (..))
import Prelude hiding (any)

import Options.ApplicativeAlt.Common
import Options.ApplicativeAlt.Types
import Options.ApplicativeAlt.Help.Pretty
import Options.ApplicativeAlt.Help.Chunk

import qualified Data.Text.Prettyprint.Doc                 as New

-- | Style for rendering an option.
data OptDescStyle ann
  = OptDescStyle
      { descSep :: Doc ann,
        descHidden :: Bool,
        descGlobal :: Bool
      }

safelast :: [a] -> Maybe a
safelast = foldl' (const Just) Nothing

-- | Generate description for a single option.
optDesc :: forall ann a. ParserPrefs -> OptDescStyle ann -> ArgumentReachability -> Option ann a -> (Chunk (Doc ann), Parenthetic)
optDesc pprefs style _reachability opt =
  let names :: [OptName]
      names =
        sort . optionNames . optMain $ opt
      meta :: Chunk (Doc ann)
      meta =
        stringChunk $ optMetaVar opt
      descs :: [Doc ann]
      descs =
        map (pretty . showOption) names
      descriptions :: Chunk (Doc ann)
      descriptions =
        listToChunk (intersperse (descSep style) descs)
      desc :: Chunk (Doc ann)
      desc
        | prefHelpLongEquals pprefs && not (isEmpty meta) && any isLongName (safelast names) =
          descriptions <> stringChunk "=" <> meta
        | otherwise =
          descriptions <<+>> meta
      show_opt :: Bool
      show_opt
        | descGlobal style && not (propShowGlobal (optProps opt)) =
          False
        | optVisibility opt == Hidden =
          descHidden style
        | otherwise =
          optVisibility opt == Visible
      wrapping :: Parenthetic
      wrapping
        | null names =
          NeverRequired
        | length names == 1 =
          MaybeRequired
        | otherwise =
          AlwaysRequired
      rendered :: Chunk (Doc ann)
      rendered
        | not show_opt =
          mempty
        | otherwise =
          desc
      modified :: Chunk (Doc ann)
      modified =
        maybe id fmap (optDescMod opt) rendered
   in (modified, wrapping)

-- | Generate descriptions for commands.
cmdDesc :: Parser ann a -> [(Maybe String, Chunk (Doc ann))]
cmdDesc = mapParser desc
  where
    desc _ opt =
      case optMain opt of
        CmdReader gn cmds p ->
          (,) gn $
            tabulate
              [ (pretty cmd, align (extractChunk d))
                | cmd <- reverse cmds,
                  d <- maybeToList . fmap infoProgDesc $ p cmd
              ]
        _ -> mempty

-- | Generate a brief help text for a parser.
briefDesc :: Bool -> ParserPrefs -> Parser ann a -> Chunk (Doc ann)
briefDesc compact = briefDesc' compact True

-- | Generate a brief help text for a parser, only including mandatory
--   options and arguments.
missingDesc :: ParserPrefs -> Parser ann a -> Chunk (Doc ann)
missingDesc = briefDesc' False False

-- | Generate a brief help text for a parser, allowing the specification
--   of if optional arguments are show.
briefDesc' :: Bool -> Bool -> ParserPrefs -> Parser ann a -> Chunk (Doc ann)
briefDesc' compact showOptional pprefs = id
    . wrapOver compact NoDefault MaybeRequired
    . foldTree compact pprefs style
    . mfilterOptional
    . treeMapParser (optDesc pprefs style)
  where
    mfilterOptional
      | showOptional =
        id
      | otherwise =
        filterOptional
    style = OptDescStyle
      { descSep = pretty "|",
        descHidden = False,
        descGlobal = False
      }

enclose2
    :: Bool
    -> Doc ann -- ^ L
    -> Doc ann -- ^ R
    -> Doc ann -- ^ x
    -> Doc ann -- ^ LxR
enclose2 compact l r x = l <> s <> x <> nl <> r <> nl
  where nl = if compact then mempty else line
        s = if compact then mempty else pretty " "

parens2 :: Bool -> Doc ann -> Doc ann
parens2 compact = enclose2 compact lparen rparen

brackets2 :: Bool -> Doc ann -> Doc ann
brackets2 compact = enclose2 compact lbracket rbracket

braces2 :: Bool -> Doc ann -> Doc ann
braces2 compact = enclose2 compact lbrace rbrace

-- | Wrap a doc in parentheses or brackets if required.
wrapOver :: Bool -> AltNodeType -> Parenthetic -> (Chunk (Doc ann), Parenthetic) -> Chunk (Doc ann)
wrapOver compact altnode mustWrapBeyond (chunk, wrapping)
  | altnode == MarkDefault =
    fmap (\c -> group (flatAlt (brackets2 compact c) (brackets2 True c))) chunk
  | wrapping > mustWrapBeyond =
    fmap (\c -> group (flatAlt (parens2 compact c) (parens2 True c))) chunk
  | otherwise =
    chunk

maybeNest :: Bool -> Doc ann -> Doc ann
maybeNest compact d = group (flatAlt (line <> pretty "  " <> nest 2 d) d)

-- Fold a tree of option docs into a single doc with fully marked
-- optional areas and groups.
foldTree :: Bool -> ParserPrefs -> OptDescStyle ann -> OptTree (Chunk (Doc ann), Parenthetic) -> (Chunk (Doc ann), Parenthetic)
foldTree compact _ _ (Leaf x) =
  x
foldTree compact prefs s (MultNode xs) =
  let go = id
        . (<</>>)
        . fmap (maybeNest compact)
        . wrapOver compact NoDefault MaybeRequired
        . foldTree compact prefs s
      x =
        foldr go mempty xs
      wrapLevel =
        mult_wrap xs
   in (x, wrapLevel)
  where
    mult_wrap [_] = NeverRequired
    mult_wrap _ = MaybeRequired
foldTree compact prefs s (AltNode b xs) =
  (\x -> (x, NeverRequired))
    -- . fmap groupOrNestLine
    -- . fmap (maybeNest compact)
    . wrapOver compact b MaybeRequired
    . alt_node
    . filter (not . isEmpty . fst)
    . map (foldTree compact prefs s)
    $ xs
  where
    alt_node :: [(Chunk (Doc ann), Parenthetic)] -> (Chunk (Doc ann), Parenthetic)
    alt_node [n] = n
    alt_node ns =
      (\y -> (y, AlwaysRequired))
        . foldr (chunked (altSep compact) . wrapOver compact NoDefault MaybeRequired) mempty
        $ ns
foldTree compact prefs s (BindNode x) =
  let rendered =
        wrapOver compact NoDefault NeverRequired (foldTree compact prefs s x)

      -- We always want to display the rendered option
      -- if it exists, and only attach the suffix then.
      withSuffix =
        rendered >>= (\r -> pure r <> stringChunk (prefMultiSuffix prefs))
   in (withSuffix, NeverRequired)

-- | Generate a full help text for a parser
fullDesc :: ParserPrefs -> Parser ann a -> Chunk (Doc ann)
fullDesc = optionsDesc False

-- | Generate a help text for the parser, showing
--   only what is relevant in the "Global options: section"
globalDesc :: ParserPrefs -> Parser ann a -> Chunk (Doc ann)
globalDesc = optionsDesc True

-- | Common generator for full descriptions and globals
optionsDesc :: Bool -> ParserPrefs -> Parser ann a -> Chunk (Doc ann)
optionsDesc global pprefs = tabulate . catMaybes . mapParser doc
  where
    doc info opt = do
      guard . not . isEmpty $ n
      guard . not . isEmpty $ h
      return (extractChunk n, align . extractChunk $ h <</>> hdef)
      where
        n = fst $ optDesc pprefs style info opt
        h = optHelp opt
        hdef = Chunk . fmap show_def . optShowDefault $ opt
        show_def s = parens (pretty "default:" <+> pretty s)
    style = OptDescStyle
      { descSep = pretty ",",
        descHidden = True,
        descGlobal = global
      }

errorHelp :: Chunk (Doc ann) -> ParserHelp ann
errorHelp chunk = mempty { helpError = chunk }

headerHelp :: Chunk (Doc ann) -> ParserHelp ann
headerHelp chunk = mempty { helpHeader = chunk }

suggestionsHelp :: Chunk (Doc ann) -> ParserHelp ann
suggestionsHelp chunk = mempty { helpSuggestions = chunk }

globalsHelp :: Chunk (Doc ann) -> ParserHelp ann
globalsHelp chunk = mempty { helpGlobals = chunk }

usageHelp :: Chunk (Doc ann) -> ParserHelp ann
usageHelp chunk = mempty { helpUsage = chunk }

bodyHelp :: Chunk (Doc ann) -> ParserHelp ann
bodyHelp chunk = mempty { helpBody = chunk }

footerHelp :: Chunk (Doc ann) -> ParserHelp ann
footerHelp chunk = mempty { helpFooter = chunk }

-- | Generate the help text for a program.
parserHelp :: ParserPrefs -> Parser ann a -> ParserHelp ann
parserHelp pprefs p =
  bodyHelp . vsepChunks $
    with_title "Available options:" (fullDesc pprefs p)
      : (group_title <$> cs)
  where
    def = "Available commands:"
    cs = groupBy ((==) `on` fst) $ cmdDesc p

    group_title a@((n, _) : _) =
      with_title (fromMaybe def n) $
        vcatChunks (snd <$> a)
    group_title _ = mempty

    with_title :: String -> Chunk (Doc ann) -> Chunk (Doc ann)
    with_title title = fmap (pretty title .$.)


parserGlobals :: ParserPrefs -> Parser ann a -> ParserHelp ann
parserGlobals pprefs p =
  globalsHelp $
    (.$.) <$> stringChunk "Global options:"
          <*> globalDesc pprefs p



-- | Generate option summary.
parserUsage :: ParserPrefs -> Parser ann a -> String -> Doc ann
parserUsage pprefs p progn =
  case prefUsageOverflow pprefs of
    UsageOverflowAlign ->
      hsep
        [ pretty "Usage:",
          pretty progn,
          align (extractChunk (briefDesc False pprefs p))
        ]
    UsageOverflowHang level ->
      hang level $
        hsep
          [ pretty "Usage:",
            pretty progn,
            extractChunk (briefDesc False pprefs p)
          ]

-- | Peek at the structure of the rendered tree within.
--
--   For example, if a child is an option with multiple
--   alternatives, such as -a or -b, we need to know this
--   when wrapping it. For example, whether it's optional:
--   we don't want to have [(-a|-b)], rather [-a|-b] or
--   (-a|-b).
data Parenthetic
  = NeverRequired
  -- ^ Parenthesis are not required.
  | MaybeRequired
  -- ^ Parenthesis should be used if this group can be repeated
  | AlwaysRequired
  -- ^ Parenthesis should always be used.
  deriving (Eq, Ord, Show)
