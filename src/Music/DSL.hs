{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall            #-}

module Music.DSL where

import           BasePrelude hiding (Category (..), Min (..))
import           Control.Lens hiding (elementsOf)
import           Control.Monad.State
import           Data.Generics.Product
import qualified Data.Map as M
import           Generics.SYB hiding (Generic)
import           Music.Types
import           Text.InterpolatedString.Perl6 (qc)


newtype JS t = JS
  { getJS :: String
  }
  deriving (Eq, Ord, Read, Data)

instance Show (JS t) where
  show = getJS

data Stave
  = Stave (Maybe Clef) (Maybe Note) (Maybe (Int, Int)) Element
  | SGroup [Stave]
  deriving (Eq, Ord, Show, Read, Data, Typeable)


data Line deriving (Typeable)
data Voice deriving (Typeable)
data StaveNote deriving (Typeable)
data Formatter deriving (Typeable)
data Bar deriving (Typeable)
data Beam deriving (Typeable)

instance Semigroup Stave where
  SGroup s1 <> SGroup s2 = SGroup $ s1 <> s2
  SGroup s1 <> s2        = SGroup $ s1 ++ [s2]
  s1 <> SGroup s2        = SGroup $ s1 : s2
  s1 <> s2               = SGroup [s1, s2]

data Element
  = EChord (ChordV Note) Int Inversion [([Int], Dur)]
  | ENote Note Int Dur
  | ERest Dur
  | EGroup [Element]
  -- ETranspose
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Dur
  = D1
  | D2
  | D4
  | D8
  | D16
  | D32
  | Dotted Dur
  deriving (Eq, Ord, Read, Data, Typeable)

type DSQ = Int

byDSQ :: Dur -> DSQ
byDSQ (Dotted D32) = error "too fine grained"
byDSQ (Dotted d) =
  let z = byDSQ d
   in div z 2 + z
byDSQ D32 = 1
byDSQ D16 = 2
byDSQ D8 = 4
byDSQ D4 = 8
byDSQ D2 = 16
byDSQ D1 = 32


instance Show Dur where
  show D1 = "w"
  show D2 = "h"
  show D4 = "4"
  show D8 = "8"
  show D16 = "16"
  show D32 = "32"
  show (Dotted d) = show d ++ "d"

instance Semigroup Element where
  EGroup s1 <> EGroup s2 = EGroup $ s1 <> s2
  EGroup s1 <> s2        = EGroup $ s1 ++ [s2]
  s1 <> EGroup s2        = EGroup $ s1 : s2
  s1 <> s2               = EGroup [s1, s2]

instance Monoid Element where
  mempty = EGroup []


withClef :: Clef -> Stave -> Stave
withClef c = everywhere $ mkT $ const $ Just c


withKey :: Note -> Stave -> Stave
withKey k = everywhere $ mkT $ \(Stave c _ t e) -> Stave c (Just k) t e


withTimeSig :: Int -> Int -> Stave -> Stave
withTimeSig t b = everywhere $ mkT $ const $ Just (t, b)


chord :: ChordV Note -> Int -> Inversion -> Element
chord c o i = EChord c o i $ pure $ (, D4) $ [0 .. length (notesOf c) - 1]


note :: Note -> Int -> Element
note n o = ENote n o D4


rest :: Element
rest = ERest D4


dur :: Dur -> Element -> Element
dur d = everywhere $ mkT $ const d


annotate :: String -> Element -> Element
annotate = undefined


bars :: Element -> Stave
bars = Stave Nothing Nothing Nothing


type JSM = State JSState

data JSState = JSState
  { jssContent     :: String
  , jssDrawQueue   :: [String]
  , jssFresh       :: Int
  , jssAccidentals :: M.Map Note Int
  }
  deriving (Eq, Show, Ord, Read, Generic)


runJSM :: JSM a -> String
runJSM js =
  let jss = execState js $ JSState "" [] 0 mempty
   in jssContent jss ++ intercalate "\n" (jssDrawQueue jss)


freshName :: forall a. Typeable a => JSM (JS a)
freshName = do
  i <- gets jssFresh
  modify $ field @"jssFresh" %~ (+1)
  pure $ JS $ '_' : typeName @a ++ show i


typeName :: forall a. Typeable a => String
typeName = fmap (\x -> bool '_' x $ isAlphaNum x)
         . show
         . typeRep
         $ Proxy @a


context :: JS ()
context = JS "ctx"


emit :: String -> JSM ()
emit e = modify $ field @"jssContent" <>~ e <> "\n"


emitDraw :: JS a -> JSM ()
emitDraw (JS name) =
  modify $ field @"jssDrawQueue" <>~ [[qc|{name}.setContext({context}).draw();|]]


emitQueue :: String -> JSM ()
emitQueue e = modify $ field @"jssDrawQueue" <>~ [e]


drawClef :: JS Bar -> Clef -> JSM ()
drawClef s c = emit [qc|{s}.addClef("{show c & _head %~ toLower}");|]


drawTimeSig :: JS Bar -> (Int, Int) -> JSM ()
drawTimeSig s (t, b) = emit [qc|{s}.addTimeSignature("{t}/{b}");|]


drawElement :: Element -> JSM [JS StaveNote]
drawElement (ENote n o d) = fmap pure $ drawNotes False [(n, o)] d
drawElement (EChord c o i [(ns, d)]) =
  let chordNotes = inOctaves o $ invert i $ notesOf c
      notes = fmap (chordNotes !!) ns
   in fmap pure $ drawNotes False notes d
drawElement (EChord c o i ns) =
  fmap join . traverse drawElement $ fmap (\n -> EChord c o i [n]) ns
drawElement (ERest d) = fmap pure $ drawNotes True [(B, 4)] d
drawElement (EGroup es) = fmap join $ traverse drawElement es


drawNotes :: Bool -> [(Note, Int)] -> Dur -> JSM (JS StaveNote)
drawNotes isRest notes d = do
  v <- freshName
  let jsNotes = intercalate "\",\""
              $ fmap (\(n, o) -> [qc|{uglyShowNote n}/{o}|]) notes
  emit $ mconcat
    [ [qc|var {v} = new VF.StaveNote(|]
    , "{"
    , [qc|keys: ["{jsNotes}"], duration: "{d}{bool "" "r" isRest}"|]
    , "});"
    ]
  doAccidentals v $ fmap fst notes
  case d of
    Dotted _ -> emit [qc|{v}.addDotToAll();|]
    _ -> pure ()

  pure v


drawVoice :: Float -> JS Bar -> [JS StaveNote] -> JSM (JS Voice)
drawVoice w stave sns = do
  vv <- freshName @[StaveNote]
  v <- freshName @Voice
  let sns' = intercalate "," $ fmap show sns
  emit [qc|var {vv} = [{sns'}];|]
  emit [qc|var {v} = new VF.Voice();|]
  emit [qc|{v}.addTickables({vv});|]

  fm <- freshName @Formatter
  emitQueue [qc|var {fm} = new VF.Formatter().joinVoices([{v}]).format([{v}], {w} - {stave}.getNoteStartX());|]
  emitQueue [qc|{v}.draw({context}, {stave});|]

  beam <- freshName @Beam
  emit [qc|var {beam} = VF.Beam.generateBeams({vv});|]
  emitQueue [qc|{beam}.forEach(beam => beam.setContext({context}).draw());|]

  pure v


doAccidentals :: JS StaveNote -> [Note] -> JSM ()
doAccidentals v notes = do
  accs <- gets jssAccidentals
  for_ (zip [0..] notes) $ \(i :: Int, n) -> do
    unless (agrees accs n) $ do
      let adj = adjustment n
      emit [qc|{v}.addAccidental({i}, new VF.Accidental("{showAdjustment adj}"));|]
      modify $ field @"jssAccidentals" %~ M.insert (baseNote n) adj


agrees :: M.Map Note Int -> Note -> Bool
agrees m n =
  adjustment n == fromMaybe 0 (M.lookup (baseNote n) m)


accidentalsForKey :: Note -> M.Map Note Int
accidentalsForKey c =
  let notes = interval c <$> [Unison, Maj2, Maj3, Perf4, Perf5, Maj6, Maj7]
   in M.fromList
    . fmap (\x -> ( baseNote x
                  , adjustment x
                  )
           )
    $ filter ((/= 0) . adjustment) notes


getActivity :: Element -> [Dur]
getActivity (EChord _ _ _ d) = fmap snd d
getActivity (ENote _ _ d)    = pure d
getActivity (ERest d)        = pure d
getActivity (EGroup g)       = g >>= getActivity


getDuration :: Element -> Sum Int
getDuration = foldMap (Sum . byDSQ) . getActivity


elementsOf :: Element -> [Element]
elementsOf (EGroup e) = e
elementsOf e          = [e]


splitMono :: Monoid m => (m -> Bool) -> (a -> m) -> [a] -> ([a], [a])
splitMono f t as = join (***) (fmap fst)
                 . span (f . snd)
                 . fmap (second $ foldMap t)
                 . zip as
                 . tail
                 $ inits as


groupMono :: Monoid m => (m -> Bool) -> (a -> m) -> [a] -> [[a]]
groupMono _ _ [] = []
groupMono f t as =
  let (a, b) = splitMono f t as
   in a : groupMono f t b


drawStave :: Stave -> JSM ()
drawStave (SGroup _) = undefined
drawStave (Stave clef key timesig e) = do
  bs <- drawBars 32 e
  let v = head bs

  for_ clef    $ drawClef v
  for_ timesig $ drawTimeSig v
  for_ key     $ \k -> do
    modify $ field @"jssAccidentals" .~ accidentalsForKey k
    -- drawKey v k


------------------------------------------------------------------------------
-- | How big we assume each activity is, in pixels
activitySize :: Int
activitySize = 35

------------------------------------------------------------------------------
-- | How wide we assume each system is, in pixels
systemWidth :: Int
systemWidth = 600


buildSystems :: Int -> DSQ -> Element -> [[(Element, Int)]]
buildSystems w dsq e =
  let bs   = splitBars dsq e
      acts = fmap (\x -> (x, (* activitySize) . length $ getActivity x)) bs
   in groupMono
        ((<= w) . getSum)
        (Sum . snd)
        acts


drawBars :: DSQ -> Element -> JSM [JS Bar]
drawBars dsq e = do
  let systems = buildSystems systemWidth dsq e
  fmap join $ for (zip [0..] systems) $ \(y :: Int, row) -> do
    let totalSize = getSum . foldMap Sum $ fmap snd row
        getWidth relSize = fromIntegral @_ @Float systemWidth
                         * fromIntegral relSize
                         / fromIntegral totalSize
    let widths = fmap (getWidth . snd) row
        startXs = fmap (getSum . foldMap Sum) $ inits widths
        row' = zip (fmap fst row) $ zip startXs widths
    for row' $ \(bar, (x, w)) -> do
      -- TODO(sandy): reset key on each bar
      v <- freshName
      emit [qc|var {v} = new VF.Stave({10 + x}, {40 + y * 110}, {w});|]
      emitDraw v

      sn <- drawElement bar
      void $ drawVoice w v sn
      pure v



main :: IO ()
main = writeFile "yes.js"
     . runJSM
     . drawStave
     . withTimeSig 4 4
     . withKey C
     . withClef Treble
     . bars
     . dur D16
     . mconcat
     . replicate 16
     $ chord (Maj D) 4 Second <> chord (Min D) 4 Third <> rest <> rest



splitBars :: DSQ -> Element -> [Element]
splitBars _ e
  | elementsOf e == [] = []
splitBars dsqs e =
  let (fit, dont) = splitMono ((<= dsqs) . getSum) getDuration $ elementsOf e
      fitDur = getSum $ foldMap getDuration fit
   in case (compare fitDur dsqs, listToMaybe dont) of
        (LT, Just _n) -> error "not enough"
          -- let (fill, remaining) = breakElement (dsqs - fitDur) n
          --  in (EGroup $ fit ++ [fill])
          --   : splitBars dsqs (EGroup $ remaining : dont)
        (EQ, _) -> EGroup fit : splitBars dsqs (EGroup dont)
        _ -> error "aghigdsi"

