module Options.Applicative.Help.Style
  ( SetStyle (..)
  , Intensity (..)
  , Layer (..)
  , Bold (..)
  , Underlined (..)
  , Italicized (..)
  , Color (..)
  , color
  , bgColor
  , colorDull
  , bgColorDull
  , bold
  , italicized
  , styleToRawText
  ) where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)

import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified System.Console.ANSI    as ANSI

data SetStyle = SetStyle
  { ansiForeground  :: Maybe (Intensity, Color) -- ^ Set the foreground color, or keep the old one.
  , ansiBackground  :: Maybe (Intensity, Color) -- ^ Set the background color, or keep the old one.
  , ansiBold        :: Maybe Bold               -- ^ Switch on boldness, or don’t do anything.
  , ansiItalics     :: Maybe Italicized         -- ^ Switch on italics, or don’t do anything.
  , ansiUnderlining :: Maybe Underlined         -- ^ Switch on underlining, or don’t do anything.
  } deriving (Eq, Ord, Show)

instance Monoid SetStyle where
    mempty = SetStyle Nothing Nothing Nothing Nothing Nothing
    mappend = (<>)

styleToRawText :: SetStyle -> String
styleToRawText = ANSI.setSGRCode . stylesToSgrs
  where
    stylesToSgrs :: SetStyle -> [ANSI.SGR]
    stylesToSgrs (SetStyle fg bg b i u) = catMaybes
        [ Just ANSI.Reset
        , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Foreground (convertIntensity intensity) (convertColor c)) fg
        , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Background (convertIntensity intensity) (convertColor c)) bg
        , fmap (\_              -> ANSI.SetConsoleIntensity ANSI.BoldIntensity) b
        , fmap (\_              -> ANSI.SetItalicized True) i
        , fmap (\_              -> ANSI.SetUnderlining ANSI.SingleUnderline) u
        ]

    convertIntensity :: Intensity -> ANSI.ColorIntensity
    convertIntensity = \i -> case i of
        Vivid -> ANSI.Vivid
        Dull  -> ANSI.Dull

    convertColor :: Color -> ANSI.Color
    convertColor = \c -> case c of
        Black   -> ANSI.Black
        Red     -> ANSI.Red
        Green   -> ANSI.Green
        Yellow  -> ANSI.Yellow
        Blue    -> ANSI.Blue
        Magenta -> ANSI.Magenta
        Cyan    -> ANSI.Cyan
        White   -> ANSI.White

data Intensity = Vivid | Dull
    deriving (Eq, Ord, Show)

data Layer = Foreground | Background
    deriving (Eq, Ord, Show)

data Bold       = Bold       deriving (Eq, Ord, Show)
data Underlined = Underlined deriving (Eq, Ord, Show)
data Italicized = Italicized deriving (Eq, Ord, Show)

instance Semigroup SetStyle where
  cs1 <> cs2 = SetStyle
    { ansiForeground  = ansiForeground  cs1 <|> ansiForeground  cs2
    , ansiBackground  = ansiBackground  cs1 <|> ansiBackground  cs2
    , ansiBold        = ansiBold        cs1 <|> ansiBold        cs2
    , ansiItalics     = ansiItalics     cs1 <|> ansiItalics     cs2
    , ansiUnderlining = ansiUnderlining cs1 <|> ansiUnderlining cs2 }

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Eq, Ord, Show)

-- | Style the foreground with a vivid color.
color :: Color -> SetStyle
color c = mempty { ansiForeground = Just (Vivid, c) }

-- | Style the background with a vivid color.
bgColor :: Color -> SetStyle
bgColor c =  mempty { ansiBackground = Just (Vivid, c) }

-- | Style the foreground with a dull color.
colorDull :: Color -> SetStyle
colorDull c =  mempty { ansiForeground = Just (Dull, c) }

-- | Style the background with a dull color.
bgColorDull :: Color -> SetStyle
bgColorDull c =  mempty { ansiBackground = Just (Dull, c) }

-- | Render in __bold__.
bold :: SetStyle
bold = mempty { ansiBold = Just Bold }

-- | Render in /italics/.
italicized :: SetStyle
italicized = mempty { ansiItalics = Just Italicized }

-- | Render underlined.
underlined :: SetStyle
underlined = mempty { ansiUnderlining = Just Underlined }
