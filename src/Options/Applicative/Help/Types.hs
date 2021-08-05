{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Options.Applicative.Help.Types (
    ParserHelp (..)
  , renderHelp
  , helpText
  ) where

import Data.Semigroup
import Data.Text (Text)
import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Style (styleToRawText)
import Prelude
import Prettyprinter.Internal (textSpaces)
import Prettyprinter.Render.Util.Panic
import Prettyprinter.Render.Util.StackMachine

import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

data ParserHelp = ParserHelp
  { helpError :: Chunk Doc
  , helpSuggestions :: Chunk Doc
  , helpHeader :: Chunk Doc
  , helpUsage :: Chunk Doc
  , helpDescription :: Chunk Doc
  , helpBody :: Chunk Doc
  , helpGlobals :: Chunk Doc
  , helpFooter :: Chunk Doc
  }

instance Show ParserHelp where
  showsPrec _ h = showString (renderHelp 80 h)

instance Monoid ParserHelp where
  mempty = ParserHelp mempty mempty mempty mempty mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup ParserHelp where
  (ParserHelp e1 s1 h1 u1 d1 b1 g1 f1) <> (ParserHelp e2 s2 h2 u2 d2 b2 g2 f2)
    = ParserHelp (mappend e1 e2) (mappend s1 s2)
                 (mappend h1 h2) (mappend u1 u2)
                 (mappend d1 d2) (mappend b1 b2)
                 (mappend g1 g2) (mappend f1 f2)

helpText :: ParserHelp -> Doc
helpText (ParserHelp e s h u d b g f) =
  extractChunk $
    vsepChunks [e, s, h, u, fmap (indent 2) d, b, g, f]

-- | Convert a help text to 'String'.
renderHelp :: Int -> ParserHelp -> String
renderHelp cols
  = LT.unpack
  . B.toLazyText
  . renderAnsi
  . layoutPretty (LayoutOptions (AvailablePerLine cols 1.0))
  . helpText

textToShowS :: Text -> B.Builder
textToShowS = B.fromText

renderAnsi :: SimpleDocStream Ann -> B.Builder
renderAnsi
  = renderSimplyDecorated B.fromText renderPush renderPop
  where
    renderPush :: Ann -> B.Builder
    renderPush ann = case ann of
      AnnTrace _ _ -> ""
      AnnStyle setStyle -> B.fromString (styleToRawText setStyle)
    renderPop :: Ann -> B.Builder
    renderPop ann = case ann of
      AnnTrace _ _ -> ""
      AnnStyle setStyle -> B.fromString (styleToRawText setStyle)
