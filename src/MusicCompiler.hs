{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall         #-}

module MusicCompiler (musicReader) where

import Music.Types (engrave, Command (..))
import Control.Monad.Catch
import Data.Char (isSpace)
import Data.IORef
import Data.List.Utils (replace)
import Generics.SYB
import Text.InterpolatedString.Perl6 (qc)
import Text.Pandoc


musicReader :: String -> IO String
musicReader s = do
    ioref <- newIORef @Int 0
    mkPandocReaderWith readMarkdown (everywhereM $ mkM $ tt ioref) pandocToHTML s
  where
    tt ioref (Para [Math DisplayMath z]) = do
      n <- readIORef ioref
      modifyIORef ioref (+1)
      pure $ RawBlock (Format "html") $ [qc|
<canvas class="music" id="music-{n}"></canvas>
<script type="text/javascript">
var renderer = new Vex.Flow.Renderer(document.getElementById('music-{n}'), Vex.Flow.Renderer.Backends.CANVAS)
var artist = new Artist(10, 5, 200)
var vextab = new VexTab(artist)
vextab.parse("\ntabstave clef=treble notation=true tablature=false\nnotes {escape z}");
artist.render(renderer);
</script>
      |]
    tt _ x = pure x


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

