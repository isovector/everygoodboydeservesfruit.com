{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall            #-}

module MusicCompiler (musicReader) where

import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.IORef
import Data.List.Utils (split)
import Data.Maybe (listToMaybe, fromJust)
import Data.Monoid (First (..))
import Data.Text (pack)
import Generics.SYB
import Language.Music.DSL
import Language.Music.Types
import SitePipe.Readers
import Text.InterpolatedString.Perl6 (qc)
import Text.Pandoc hiding (Inline (Note))


musicReader :: String -> IO String
musicReader s = do
    ref <- newIORef $ id @Int 0
    mkPandocReaderWith
        (fmap (. pack) readMarkdown)
        (fmap (fmap $ everywhere $ mkT inline) $ everywhereM $ mkM $ tt ref)
        pandocToHTML s
  where
    inline (Math InlineMath z) =
      Str $ fromJust $ getFirst $ mconcat
        [ readMaybe @(ChordV Note) z
        , readMaybe @Note z
        , readMaybe @Roman z
        , readMaybe @Interval z
        ]
    inline x = x

    tt _ (CodeBlock (_, ["chords"], _) doc) = do
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

    tt ref (CodeBlock (_, ["music"], _) doc) = do
      name <- liftIO $ readIORef ref <* modifyIORef ref (+1)
      let divName = "music-" ++ show name
      pure . Para . pure . RawInline (Format "html") $ mconcat
        [ [qc|<div id="{divName}" class="music"></div>|]
        , [qc|
<script type="text/javascript">
{either (error . show) id $ musicParser divName doc}
</script>
        |]
        ]

    tt _ x = pure x


readMaybe :: forall a. (Read a, Show a) => String -> First String
readMaybe = fmap (show @a) . coerce . fmap fst . listToMaybe . reads @a

