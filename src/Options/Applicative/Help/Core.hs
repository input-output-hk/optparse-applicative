{-# LANGUAGE CPP #-}
module Options.Applicative.Help.Core (
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
  descriptionHelp,
  bodyHelp,
  footerHelp,
  globalsHelp,
  parserHelp,
  parserUsage,
  parserGlobals
  ) where

import Control.Applicative
import Control.Monad (guard, MonadPlus)
import Data.Bifunctor (Bifunctor(first))
import Data.Function (on)
import Data.List (sort, intersperse, groupBy)
import Data.Foldable (any, foldl')
import Data.Maybe (maybeToList, catMaybes, fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import Prelude hiding (any)

import Options.Applicative.Common
import Options.Applicative.Types
import Options.Applicative.Help.Ann
import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Pretty

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Redundant $" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use tuple-section" -}

-- | Style for rendering an option.
data OptDescStyle
  = OptDescStyle
      { descSep :: Doc,
        descHidden :: Bool,
        descGlobal :: Bool
      }

safelast :: [a] -> Maybe a
safelast = foldl' (const Just) Nothing

-- | Generate description for a single option.
optDesc :: ParserPrefs -> OptDescStyle -> ArgumentReachability -> Option a -> (Chunk Doc, Parenthetic)
optDesc pprefs style _reachability opt = first (annTrace 2 "optDesc") $
  let names =
        sort . optionNames . optMain $ opt
      meta =
        stringChunk $ optMetaVar opt
      descs =
        map (string . showOption) names
      descriptions =
        listToChunk (intersperse (descSep style) descs)
      desc
        | prefHelpLongEquals pprefs && not (isEmpty meta) && any isLongName (safelast names) =
          descriptions <> stringChunk "=" <> meta
        | otherwise =
          descriptions <<+>> meta
      show_opt
        | descGlobal style && not (propShowGlobal (optProps opt)) =
          False
        | optVisibility opt == Hidden =
          descHidden style
        | otherwise =
          optVisibility opt == Visible
      wrapping
        | null names =
          NeverRequired
        | length names == 1 =
          MaybeRequired
        | otherwise =
          AlwaysRequired
      rendered
        | not show_opt =
          mempty
        | otherwise =
          desc
      modified =
        maybe id fmap (optDescMod opt) rendered
   in (modified, wrapping)

-- | Generate descriptions for commands.
cmdDesc :: ParserPrefs -> Parser a -> [(Maybe String, Chunk Doc)]
cmdDesc pprefs = fmap (fmap (annTrace 2 "cmdDesc")) <$> mapParser desc
  where
    desc _ opt =
      case optMain opt of
        CmdReader gn cmds p ->
          (,) gn $
            tabulate (prefTabulateFill pprefs)
              [ (string cmd, align (extractChunk d))
                | cmd <- reverse cmds,
                  d <- maybeToList . fmap infoProgDesc $ p cmd
              ]
        _ -> mempty

-- | Generate a brief help text for a parser.
briefDesc :: ParserPrefs -> Parser a -> Chunk Doc
briefDesc = fmap (annTrace 2 "briefDesc") . briefDesc' True

-- | Generate a brief help text for a parser, only including mandatory
--   options and arguments.
missingDesc :: ParserPrefs -> Parser a -> Chunk Doc
missingDesc = fmap (annTrace 2 "missingDesc") . briefDesc' False

-- | Generate a brief help text for a parser, allowing the specification
--   of if optional arguments are show.
briefDesc' :: Bool -> ParserPrefs -> Parser a -> Chunk Doc
briefDesc' showOptional pprefs = fmap (annTrace 2 "briefDesc'")
    . wrapOver NoDefault MaybeRequired
    . foldTree pprefs style
    . mfilterOptional
    . treeMapParser (optDesc pprefs style)
  where
    mfilterOptional
      | showOptional =
        id
      | otherwise =
        filterOptional
    style = OptDescStyle
      { descSep = string "|",
        descHidden = False,
        descGlobal = False
      }

-- | Wrap a doc in parentheses or brackets if required.
wrapOver :: AltNodeType -> Parenthetic -> (Chunk Doc, Parenthetic) -> Chunk Doc
wrapOver altnode mustWrapBeyond (chunk, wrapping)
  | chunkIsEffectivelyEmpty chunk =
    annTrace 3 "wrapOver0" <$> chunk
  | altnode == MarkDefault =
    annTrace 3 "wrapOver1" <$> fmap brackets chunk
  | wrapping > mustWrapBeyond =
    annTrace 3 "wrapOver2" <$> fmap parens chunk
  | otherwise =
    annTrace 3 "wrapOver3" chunk

-- Fold a tree of option docs into a single doc with fully marked
-- optional areas and groups.
foldTree :: ParserPrefs -> OptDescStyle -> OptTree (Chunk Doc, Parenthetic) -> (Chunk Doc, Parenthetic)
foldTree _ _ (Leaf x) = first (annTrace 3 "foldTree1")
  x
foldTree prefs s (MultNode xs) =
  ( let generous :: Chunk Doc
        generous = annTrace 3 "generous" $
            if null xs
              then mempty
              else id
                . mconcat
                . fmap (\(w, d) -> (w <>) <$> d)
                . zip leads
                $ fmap (wrapOver NoDefault MaybeRequired . first (fmap (nest 2)) . foldTree prefs s) xs
        compact :: Chunk Doc
        compact = annTrace 3 "compact" $
          foldr (chunked (</>) . wrapOver NoDefault MaybeRequired . foldTree prefs s) mempty xs
    in group <$> chunkFlatAlt generous compact
  , mult_wrap xs
  )
  where
    mult_wrap [_] = NeverRequired
    mult_wrap _ = MaybeRequired
    leads :: [Doc]
    leads = mempty:repeat (line <> pretty "  ")

foldTree prefs s (AltNode b xs) = first (annTrace 3 "foldTree2") $
  (\x -> (x, NeverRequired))
    . fmap groupOrNestLine
    . wrapOver b MaybeRequired
    . alt_node
    . fmap (first (\d -> annTrace 3 (show d) d))
    . filter (not . isEmpty . fst)
    . map (foldTree prefs s)
    $ xs
  where
    alt_node :: [(Chunk Doc, Parenthetic)] -> (Chunk Doc, Parenthetic)
    alt_node [n] = n
    alt_node ns =
      ( fmap group
        $ chunkFlatAlt
          ( if null ns
              then mempty
              else
                ( mconcat
                . fmap (\(w, d) -> (w <>) <$> d)
                . zip leads
                $ fmap (wrapOver NoDefault MaybeRequired) ns
                ) <> pure line
          )

          ( foldr (chunked altSep . wrapOver NoDefault MaybeRequired) mempty
          $ ns
          )
      , AlwaysRequired
      )
    leads :: [Doc]
    leads = pretty " ":repeat (line <> pretty "| ")

foldTree prefs s (BindNode x) = first (annTrace 3 "foldTree3") $
  let rendered = annTrace 3 "rendered" $
        wrapOver NoDefault NeverRequired (foldTree prefs s x)

      -- We always want to display the rendered option
      -- if it exists, and only attach the suffix then.
      withSuffix =
        rendered >>= (\r -> pure r <> stringChunk (prefMultiSuffix prefs))
   in (withSuffix, NeverRequired)

-- | Generate a full help text for a parser
fullDesc :: ParserPrefs -> Parser a -> Chunk Doc
fullDesc = fmap (annTrace 2 "fullDesc") <$> optionsDesc False

-- | Generate a help text for the parser, showing
--   only what is relevant in the "Global options: section"
globalDesc :: ParserPrefs -> Parser a -> Chunk Doc
globalDesc = fmap (annTrace 2 "globalDesc") <$> optionsDesc True

-- | Common generator for full descriptions and globals
optionsDesc :: Bool -> ParserPrefs -> Parser a -> Chunk Doc
optionsDesc global pprefs = fmap (annTrace 2 "optionsDesc")
    . tabulate (prefTabulateFill pprefs)
    . catMaybes
    . mapParser doc
  where
    doc :: MonadPlus m => ArgumentReachability -> Option a -> m (Doc, Doc)
    doc info opt = do
      guard . not . isEmpty $ n
      guard . not . isEmpty $ h
      return (extractChunk n, align . extractChunk $ h <</>> hdef)
      where
        n = fst $ optDesc pprefs style info opt
        h = optHelp opt
        hdef = Chunk . fmap show_def . optShowDefault $ opt
        show_def s = parens (string "default:" <+> string s)
    style = OptDescStyle
      { descSep = string ",",
        descHidden = True,
        descGlobal = global
      }

errorHelp :: Chunk Doc -> ParserHelp
errorHelp chunk = mempty { helpError = chunk }

headerHelp :: Chunk Doc -> ParserHelp
headerHelp chunk = mempty { helpHeader = chunk }

suggestionsHelp :: Chunk Doc -> ParserHelp
suggestionsHelp chunk = mempty { helpSuggestions = chunk }

globalsHelp :: Chunk Doc -> ParserHelp
globalsHelp chunk = mempty { helpGlobals = chunk }

usageHelp :: Chunk Doc -> ParserHelp
usageHelp chunk = mempty { helpUsage = chunk }

descriptionHelp :: Chunk Doc -> ParserHelp
descriptionHelp chunk = mempty { helpDescription = chunk }

bodyHelp :: Chunk Doc -> ParserHelp
bodyHelp chunk = mempty { helpBody = chunk }

footerHelp :: Chunk Doc -> ParserHelp
footerHelp chunk = mempty { helpFooter = chunk }

-- | Generate the help text for a program.
parserHelp :: ParserPrefs -> Parser a -> ParserHelp
parserHelp pprefs p =
  bodyHelp . vsepChunks $
    with_title "Available options:" (fullDesc pprefs p)
      : (group_title <$> cs)
  where
    def = "Available commands:"
    cs = groupBy ((==) `on` fst) $ cmdDesc pprefs p

    group_title a@((n, _) : _) =
      with_title (fromMaybe def n) $
        vcatChunks (snd <$> a)
    group_title _ = mempty

    with_title title = annTrace 1 "with_title" . fmap (string title .$.)


parserGlobals :: ParserPrefs -> Parser a -> ParserHelp
parserGlobals pprefs p =
  globalsHelp $
    (.$.) <$> stringChunk "Global options:"
          <*> globalDesc pprefs p



-- | Generate option summary.
parserUsage :: ParserPrefs -> Parser a -> String -> Doc
parserUsage pprefs p progn = annTrace 2 "parserUsage" $
  case prefUsageOverflow pprefs of
    UsageOverflowAlign ->
      hsep
        [ string "Usage:",
          string progn,
          align (extractChunk (briefDesc pprefs p))
        ]
    UsageOverflowHang level ->
      hang level $
        hsep
          [ string "Usage:",
            string progn,
            extractChunk (briefDesc pprefs p)
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
