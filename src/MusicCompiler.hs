{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall            #-}

module MusicCompiler (musicReader) where

import Control.Monad.Catch
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.IORef
import Data.List.Utils (replace)
import Data.Maybe (listToMaybe, fromJust)
import Data.Monoid (First (..))
import Generics.SYB
import Music.Types (engrave, Command (..), Note, ChordV, Roman, Interval)
import Text.InterpolatedString.Perl6 (qc)
import Text.Pandoc hiding (Inline (Note))


musicReader :: String -> IO String
musicReader s = do
    ioref <- newIORef @Int 0
    mkPandocReaderWith
        readMarkdown
        (fmap (fmap $ everywhere $ mkT inline) $ everywhereM $ mkM $ tt ioref)
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

    tt ioref (Para [Math DisplayMath z]) = do
      n <- readIORef ioref
      modifyIORef ioref (+1)
      pure . Para . pure . RawInline (Format "html") $ [qc|
<canvas class="music" id="music-{n}"></canvas>
<script type="text/javascript">
var renderer = new Vex.Flow.Renderer(document.getElementById('music-{n}'), Vex.Flow.Renderer.Backends.CANVAS)
var artist = new Artist(10, 5, 400)
var vextab = new VexTab(artist)
vextab.parse("\ntabstave clef=treble notation=true tablature=false\nnotes {escape z}");
artist.render(renderer);
</script>
      |]
    tt _ x = pure x


readMaybe :: forall a. (Read a, Show a) => String -> First String
readMaybe = fmap (show @a) . coerce . fmap fst . listToMaybe . reads @a


doCommand :: String -> String
doCommand ('!' : c) = engrave $ read @Command c
doCommand z = z



escape :: String -> String
escape = replace "\n" " " . unlines . fmap doCommand . lines . dropWhile isSpace


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

