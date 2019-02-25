{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Music.DSL where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Bool
import           Data.Char
import           Data.Data
import           Data.Foldable
import           Data.Generics.Product
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Proxy
import           Data.Semigroup
import           Data.Typeable
import           GHC.Generics
import           Generics.SYB hiding (Generic)
import           Music.Types
import           Text.InterpolatedString.Perl6 (q, qc)


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



withClef :: Clef -> Stave -> Stave
withClef c = everywhere $ mkT $ const $ Just c

withKey :: Note -> Stave -> Stave
withKey k = everywhere $ mkT $ \(Stave c _ t e) -> Stave c (Just k) t e

withTimeSig :: Int -> Int -> Stave -> Stave
withTimeSig t b = everywhere $ mkT $ const $ Just (t, b)

chord :: ChordV Note -> Int -> Inversion -> Element
chord c o i = EChord c o i $ pure $ (, D4) $ [0 .. length $ notesOf c]

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
   in jssContent jss ++ join (jssDrawQueue jss)

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


emit :: String -> JSM ()
emit e = modify $ field @"jssContent" <>~ e <> "\n"


emitDraw :: JS a -> JSM ()
emitDraw (JS name) =
  modify $ field @"jssDrawQueue" <>~ [[qc|{name}.setContext("ctx").draw();|]]

drawClef :: JS Stave -> Clef -> JSM ()
drawClef s c = emit [qc|{s}.addClef("{show c & _head %~ toLower}");|]

drawTimeSig :: JS Stave -> (Int, Int) -> JSM ()
drawTimeSig s (t, b) = emit [qc|{s}.addTimeSignature("{t}/{b}");|]


drawElement :: Element -> JSM (JS StaveNote)
drawElement (ENote n o d) = drawNotes [(n, o)] d

drawNotes :: [(Note, Int)] -> Dur -> JSM (JS StaveNote)
drawNotes notes d = do
  v <- freshName
  let jsNotes = intercalate "\",\""
              $ fmap (\(n, o) -> [qc|{uglyShowNote n}/{o}|]) notes
  emit $ mconcat
    [ [qc|var {v} = new VF.StaveNote(|]
    , "{"
    , [qc|keys: ["{jsNotes}"], duration: "{d}"|]
    , "});"
    ]
  doAccidentals v $ fmap fst notes
  case d of
    Dotted _ -> emit [qc|{v}.addDotToAll();|]
    _ -> pure ()
  pure v


doAccidentals :: JS StaveNote -> [Note] -> JSM ()
doAccidentals v notes = do
  accs <- gets jssAccidentals
  for_ (zip [0..] notes) $ \(i, n) -> do
    unless (agrees accs n) $ do
      let adj = adjustment n
      emit [qc|{v}.addAccidental({i}, new VF.Accidental("{showAdjustment adj}"));|]
      modify $ field @"jssAccidentals" %~ M.insert (baseNote n) adj


agrees :: M.Map Note Int -> Note -> Bool
agrees m n =
  case M.lookup (baseNote n) m of
    Just x  -> x == adjustment n
    Nothing -> True



accidentalsForKey :: Note -> M.Map Note Int
accidentalsForKey c =
  let notes = interval c <$> [Unison, Maj2, Maj3, Perf4, Perf5, Maj6, Maj7]
   in M.fromList . fmap (\x -> ( baseNote x
                               , adjustment x))
                 $ filter ((/= 0) . adjustment) notes



drawStave :: Stave -> JSM (JS Stave)
drawStave (Stave clef key timesig e) = do
  v <- freshName
  emit [qc|var {v} = new VF.Stave(10, 40, 400);|]
  for_ clef $ drawClef v
  for_ timesig $ drawTimeSig v
  for_ key $ \k -> do
    modify $ field @"jssAccidentals" .~ accidentalsForKey k
    -- drawKey v k
  emitDraw v

  drawElement e
  pure v

