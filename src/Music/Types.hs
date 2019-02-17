{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wall            #-}

module Music.Types where

import Data.Bool (bool)
import Data.List (intercalate)
import Debug.Trace
import GHC.Generics
import Data.Monoid ((<>))

class Engrave a where
  engrave :: a -> String

data Note
  = Cbb
  | Cb
  | C
  | Cs
  | Css
  | Dbb
  | Db
  | D
  | Ds
  | Dss
  | Ebb
  | Eb
  | E
  | Es
  | Ess
  | Fbb
  | Fb
  | F
  | Fs
  | Fss
  | Gbb
  | Gb
  | G
  | Gs
  | Gss
  | Abb
  | Ab
  | A
  | As
  | Ass
  | Bbb
  | Bb
  | B
  | Bs
  | Bss
  deriving (Eq, Ord, Enum, Bounded, Generic, Read)

toNoteClass :: Note -> Roman
toNoteClass C   = I
toNoteClass Cs  = I
toNoteClass Css = I
toNoteClass Cb  = I
toNoteClass Cbb = I
toNoteClass D   = II
toNoteClass Ds  = II
toNoteClass Dss = II
toNoteClass Db  = II
toNoteClass Dbb = II
toNoteClass E   = III
toNoteClass Es  = III
toNoteClass Ess = III
toNoteClass Eb  = III
toNoteClass Ebb = III
toNoteClass F   = IV
toNoteClass Fs  = IV
toNoteClass Fss = IV
toNoteClass Fb  = IV
toNoteClass Fbb = IV
toNoteClass G   = V
toNoteClass Gs  = V
toNoteClass Gss = V
toNoteClass Gb  = V
toNoteClass Gbb = VI
toNoteClass A   = VI
toNoteClass As  = VI
toNoteClass Ass = VI
toNoteClass Ab  = VI
toNoteClass Abb = VI
toNoteClass B   = VII
toNoteClass Bs  = VII
toNoteClass Bss = VII
toNoteClass Bb  = VII
toNoteClass Bbb = VII

fromNoteClass :: Roman -> Note
fromNoteClass I   = C
fromNoteClass II  = D
fromNoteClass III = E
fromNoteClass IV  = F
fromNoteClass V   = G
fromNoteClass VI  = A
fromNoteClass VII = B

instance Show Note where
  show C   = "C"
  show D   = "D"
  show E   = "E"
  show F   = "F"
  show G   = "G"
  show A   = "A"
  show B   = "B"
  show a   = foldNote (show . fromNoteClass . toNoteClass) (++ "b") (++ "#") a

-- TODO(sandy): flats are broken
instance Engrave Note where
  engrave = show

foldNote :: (Note -> a) -> (a -> a) -> (a -> a) -> Note -> a
foldNote f _ _ C   =         f C
foldNote f _ s Cs  = s     $ f C
foldNote f _ s Css = s . s $ f C
foldNote f b _ Cb  = b     $ f C
foldNote f b _ Cbb = b . b $ f C
foldNote f _ _ D   =         f D
foldNote f _ s Ds  = s     $ f D
foldNote f _ s Dss = s . s $ f D
foldNote f b _ Db  = b     $ f D
foldNote f b _ Dbb = b . b $ f D
foldNote f _ _ E   =         f E
foldNote f _ s Es  = s     $ f E
foldNote f _ s Ess = s . s $ f E
foldNote f b _ Eb  = b     $ f E
foldNote f b _ Ebb = b . b $ f E
foldNote f _ _ F   =         f F
foldNote f _ s Fs  = s     $ f F
foldNote f _ s Fss = s . s $ f F
foldNote f b _ Fb  = b     $ f F
foldNote f b _ Fbb = b . b $ f F
foldNote f _ _ G   =         f G
foldNote f _ s Gs  = s     $ f G
foldNote f _ s Gss = s . s $ f G
foldNote f b _ Gb  = b     $ f G
foldNote f b _ Gbb = b . b $ f G
foldNote f _ _ A   =         f A
foldNote f _ s As  = s     $ f A
foldNote f _ s Ass = s . s $ f A
foldNote f b _ Ab  = b     $ f A
foldNote f b _ Abb = b . b $ f A
foldNote f _ _ B   =         f B
foldNote f _ s Bs  = s     $ f B
foldNote f _ s Bss = s . s $ f B
foldNote f b _ Bb  = b     $ f B
foldNote f b _ Bbb = b . b $ f B

pitchClass :: Note -> Int
pitchClass =
  modular 12 . foldNote (semitonesAwayFromC . toNoteClass) (subtract 1) (+1)

modular :: Int -> Int -> Int
modular z n
  | n < 0     = n + z
  | n >= z    = n - z
  | otherwise = n


semitonesAwayFromC :: Roman -> Int
semitonesAwayFromC I   = 0
semitonesAwayFromC II  = 2
semitonesAwayFromC III = 4
semitonesAwayFromC IV  = 5
semitonesAwayFromC V   = 7
semitonesAwayFromC VI  = 9
semitonesAwayFromC VII = 11


data Accidental = Flat | Natural | Sharp
  deriving (Eq, Ord, Enum, Bounded, Generic, Read)

instance Show Accidental where
  show Flat    = "♭"
  show Natural = "♮"
  show Sharp   = "♯"

instance Monoid Accidental where
  mempty = Natural
  mappend Flat    Flat    = error "i told you we needed double flats"
  mappend Flat    Sharp   = Natural
  mappend Sharp   Flat    = Natural
  mappend Sharp   Sharp   = error "i told you we needed double sharps"
  mappend Natural a       = a
  mappend a       Natural = a


data Degree
  = Four
  | Five
  | Six
  | Seven
  | Nine
  | Eleven
  | Thirteen
  | Fifteen
  deriving (Eq, Ord, Enum, Bounded, Read)

instance Show Degree where
  show Four     = "4"
  show Five     = "5"
  show Six      = "6"
  show Seven    = "7"
  show Nine     = "9"
  show Eleven   = "11"
  show Thirteen = "13"
  show Fifteen  = "15"


data Command
  = Chord (ChordV Note) Int Inversion
  | Chord' (ChordV Note)
  | Note Note
  deriving (Eq, Ord, Read)

instance Show Command where
  show (Chord' c) = show c
  show (Chord c _ _) = show c
  show (Note n)      = show n


data ChordV a
  = Maj a
  | Maj7C a
  | Min7C a
  | Dom7 a
  -- | MinMaj a
  -- | Sus a
  | Min a
  | Dim a
  -- | Alt (ChordV a)
  -- | Mod (Modified Degree) (ChordV a)
  -- | Slash (ChordV a) a
  deriving (Eq, Ord, Functor, Generic, Read)


data Modified a = Modified Accidental a
  deriving (Eq, Ord, Functor, Generic, Read)

instance Show a => Show (Modified a) where
  show (Modified fs d) = show fs ++ show d

instance Applicative Modified where
  pure = Modified Natural
  Modified fx fa <*> Modified x a = Modified (fx <> x) $ fa a

instance Show a => Show (ChordV a) where
  show (Maj a)     = show a
  show (Maj7C a)    = show a ++ "Δ"
  show (Min7C a)    = show a ++ "-7"
  -- show (MinMaj a)  = show a ++ "Δ-"
  show (Dom7 a)    = show a ++ "7"
  -- show (Sus a)     = show a ++ "sus"
  show (Min a)     = show a ++ "-"
  show (Dim a)     = show a ++ "∅"
  -- show (Mod x c)   = show c ++ " " ++ show x
  -- show (Slash c a) = show c ++ "/" ++ show a
  -- show (Alt c)     = show c ++ "alt"


-- data Mode
--   = Ionian
--   | Dorian
--   | Phrygian
--   | Lydian
--   | Mixolydian
--   | Aeolian
--   | Locrian
--   deriving (Eq, Ord, Enum, Show, Generic)

-- data Harmony
--   = MajorScale
--   | MelodicMinorScale
--   deriving (Eq, Ord, Enum, Show, Generic)

data Roman
  = I
  | II
  | III
  | IV
  | V
  | VI
  | VII
  deriving (Eq, Ord, Enum, Show, Generic, Bounded)

data Inversion = First | Second | Third | Fourth
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Read)

data Interval
  = Unison
  | Min2
  | Maj2
  | Min3
  | Maj3
  | Perf4
  | Tritone
  | Perf5
  | Min6
  | Maj6
  | Min7
  | Maj7
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Read)

intervalSize :: Interval -> Roman
intervalSize Unison  = I
intervalSize Min2    = II
intervalSize Maj2    = II
intervalSize Min3    = III
intervalSize Maj3    = III
intervalSize Perf4   = IV
intervalSize Tritone = IV
intervalSize Perf5   = V
intervalSize Min6    = VI
intervalSize Maj6    = VI
intervalSize Min7    = VII
intervalSize Maj7    = VII

add :: forall a. (Enum a, Bounded a) => a -> a -> a
add a b = toEnum . modular (fromEnum (maxBound @a) + 1) $ fromEnum a + fromEnum b


adjustment :: Note -> Int
adjustment n = fromEnum n - fromEnum (fromNoteClass $ toNoteClass n)

adjust :: Note -> Int -> Note
adjust n a = toEnum $ fromEnum n + a

showTrace :: Show a => a -> a
showTrace = trace =<< show

interval :: Note -> Interval -> Note
interval n i =
  let nname = fromNoteClass $ add (toNoteClass n) $ intervalSize i
      adj = modular 12 (pitchClass n + fromEnum i) - pitchClass nname
   in adjust nname adj

notesOf :: ChordV Note -> [Note]
notesOf (Maj c)   = interval c <$> [Unison, Maj3, Perf5]
notesOf (Min c)   = interval c <$> [Unison, Min3, Perf5]
notesOf (Maj7C c) = interval c <$> [Unison, Maj3, Perf5, Maj7]
notesOf (Min7C c) = interval c <$> [Unison, Min3, Perf5, Min7]
notesOf (Dom7 c)  = interval c <$> [Unison, Maj3, Perf5, Min7]
notesOf (Dim c)   = interval c <$> [Unison, Min3, Tritone]

inOctaves :: Int -> [Note] -> [(Note, Int)]
inOctaves _ [] = []
inOctaves o ns@((fromEnum -> n) : _) = fmap (\z -> (z, bool o (o+1) $ fromEnum z < n)) ns

rotate :: [a] -> [a]
rotate [] = []
rotate (a : as) = as ++ [a]

invert :: Inversion -> [a] -> [a]
invert i n = iterate rotate n !! fromEnum i


instance Engrave (Note, Int) where
  engrave (n, i) = engrave n ++ "/" ++ show i

instance Engrave Command where
  engrave (Chord' c) = engrave $ Chord c 4 First
  engrave (Chord c o i)
      = ('(' :)
      . (++ ")")
      . intercalate "."
      . fmap engrave
      . inOctaves o
      . invert i
      $ notesOf c
  engrave (Note n) = engrave (n, 4 :: Int)


