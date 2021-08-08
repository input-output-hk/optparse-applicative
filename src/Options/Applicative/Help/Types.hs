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
import Options.Applicative.Help.Style (SetStyle (..), styleToRawText, defaultStyle)
import Prelude
import Prettyprinter.Internal (textSpaces)
import Prettyprinter.Render.Util.Panic

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

renderAnsi :: SimpleDocStream Ann -> B.Builder
renderAnsi
  = renderCtxDecorated defaultStyle B.fromText renderPush renderPop
  . alterAnnotationsS alter
  where
    alter :: Ann -> Maybe SetStyle
    alter (AnnStyle setStyle) = Just setStyle
    renderPush :: SetStyle -> SetStyle -> B.Builder
    renderPush _ setStyle = B.fromString (styleToRawText setStyle)
    renderPop :: SetStyle -> SetStyle -> B.Builder
    renderPop setStyle _ = B.fromString (styleToRawText setStyle)

renderCtxDecorated
    :: Monoid ann
    => ann
    -> (Text -> B.Builder) -- ^ Render plain 'Text'
    -> (ann -> ann -> B.Builder)  -- ^ How to render an annotation
    -> (ann -> ann -> B.Builder)  -- ^ How to render the removed annotation
    -> SimpleDocStream ann
    -> B.Builder
renderCtxDecorated topAnn toText push pop = go [topAnn]
  where
    go _                      SFail               = panicUncaughtFail
    go []                     SEmpty              = mempty
    go (_:_:_)                SEmpty              = panicInputNotFullyConsumed
    go (_:_)                  SEmpty              = mempty
    go stack                  (SChar c rest)      = toText (T.singleton c) <> go stack rest
    go stack                  (SText _l t rest)   = toText t <> go stack rest
    go stack                  (SLine i rest)      = toText (T.singleton '\n') <> toText (textSpaces i) <> go stack rest
    go stack@(ctxAnn:_)       (SAnnPush ann rest) = push ctxAnn ann <> go ((ctxAnn <> ann) : stack) rest
    go (ann:stack@(ctxAnn:_)) (SAnnPop rest)      = pop ctxAnn ann <> go stack rest
    go []                     SAnnPop{}           = panicUnpairedPop
    go _                      (SAnnPush _ _)      = error "Unpaired push"
    go _                      (SAnnPop _)         = error "Unpaired pop"
{-# INLINE renderCtxDecorated #-}
