{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall            #-}

module MusicCompiler (musicReader) where

import Control.Monad.Catch
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.List (isInfixOf)
import Data.List.Utils (replace, split)
import Data.Maybe (listToMaybe, fromJust)
import Data.Monoid (First (..))
import Generics.SYB
import Music.Types (engrave, Command (..), Note, ChordV, Roman, Interval)
import Text.InterpolatedString.Perl6 (qc)
import Text.Pandoc hiding (Inline (Note))


musicReader :: String -> IO String
musicReader s = do
    mkPandocReaderWith
        readMarkdown
        (fmap (fmap $ everywhere $ mkT inline) $ everywhereM $ mkM tt)
        pandocToHTML s
  where
    inline (Math InlineMath z) =
      Str $ fromJust $ getFirst $ mconcat
        [ readMaybe @Command z
        , readMaybe @(ChordV Note) z
        , readMaybe @Note z
        , readMaybe @Roman z
        , readMaybe @Interval z
        ]
    inline x = x

    tt (Para [Math DisplayMath z]) =
      pure . Para . pure . RawInline (Format "html") $ [qc|
<div class="vex-tabdiv music">
{escape z}
</div>
      |]

    tt (CodeBlock (_, ["chords"], _) doc) = do
      let chords = fmap (split "|") $ lines doc
      pure . Para . pure . RawInline (Format "html") $ mconcat
        [ [qc|<table class="chord-diagram">|]
        , flip foldMap chords $ \row -> mconcat
            [ [qc|<tr>|]
            , flip foldMap row $ \chord ->
              [qc|<td>{chord}</td>|]
            , [qc|</tr>
              |]
            ]
        , [qc|</table>|]
        ]

    tt x = pure x


readMaybe :: forall a. (Read a, Show a) => String -> First String
readMaybe = fmap (show @a) . coerce . fmap fst . listToMaybe . reads @a


doCommand :: String -> String
doCommand ('!' : c) = engrave $ read @Command c
doCommand z = z



escape :: String -> String
escape = replace "\\n" "\n"
       . replace "!!!!!" "\\nnotes "
       . ("options space=15 font-size=18\\n" ++)
       . addPreamble
       . replace "\n" " "
       . unlines
       . fmap doCommand
       . lines
       . dropWhile isSpace


addPreamble :: String -> String
addPreamble s
  | "!!!!!" `isInfixOf` s = s
  | otherwise = "tabstave notation=true tablature=false clef=treble!!!!!" ++ s


mkPandocReaderWith
    :: (ReaderOptions -> String -> Either PandocError Pandoc)
    -> (Pandoc -> IO Pandoc)
    -> (Pandoc -> String)
    -> String
    -> IO String
mkPandocReaderWith pReader transformer writer content =
  fmap writer $ transformer =<< runPandocReader (pReader def) content


pandocToHTML :: Pandoc -> String
pandocToHTML = writeHtmlString $
  def { writerHighlight = True }


runPandocReader
    :: (MonadThrow m)
    => (String -> Either PandocError Pandoc)
    -> String
    -> m Pandoc
runPandocReader panReader source =
  case panReader source of
    Left err -> throwM err
    Right pandoc -> return pandoc

