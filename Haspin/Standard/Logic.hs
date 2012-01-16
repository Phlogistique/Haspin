-- | 'Haspin.Standard.Logic' implements the pattern matching bits that
-- transform a parse tree to a combo data structure, and manipulates the
-- data structure.
-- 
-- This file includes:
-- * data which does not pertain to parsing; for exemple equivalences between
--   tricks, and default rotation and timeslots;
-- * algorithms for transforming syntax trees obtained from parsers to combos
--   with defined values
-- * miscelaneous algorithms that manipulate breakdowns

module Haspin.Standard.Logic where
import Haspin.Standard.Data
import Haspin.Standard.StrictParser
import Control.Monad
import Data.List
import Data.DList (singleton, snoc, toList)
import Data.Maybe

-- | Data about the default rotation number and fingerslots for each trick; it
-- is encoded as a breakdown and parsed (hopefully) early at runtime for
-- concision. This is, though, unsafe and ugly; it would be better if we
-- could somehow get Template Haskell to work with breakdowns without
-- rewriting all the parsers.
defaults = map trickWithDefaults $! parseDefaults
parseDefaults = unsafeCombo
    "Thumbaround normal 1.0 T12-T1 > \
    \Sonic normal 1.0 23-12 > \
    \Pinkyswivel normal 1.0 12-23"

nameEquivalences = [("Swivel", "Pinkyswivel")]
trickEquivalence =
    [("Antigravity", unsafeTrick "Fingerless Indexaround Reverse 11-11")]

-- | Transforms a ParsedTrick with possibly unknown components to a Trick where
-- the unknown components have been replaced with either defaults found in
-- 'defaults', or general purpose default values (Normal 1.0 12-12) in
-- compliance with the convention.
trickWithDefaults (ParsedTrick name dir rot start stop)  =
    Trick name dir' rot' start' stop'
  where trickDefaults = find (\t -> name == trickName t) defaults
        start' = findJust [start, liftM trickStart trickDefaults] $
            Slot [Index, Middle]
        stop' = findJust [stop, start, liftM trickStop trickDefaults] start'
        dir' = findJust [dir, liftM trickDir trickDefaults] Normal
        rot' = findJust [rot, liftM trickRot trickDefaults] $ Rotation 2
        findJust :: [Maybe a] -> a -> a
        findJust l d = fromMaybe d $ msum l

extCombo :: ParsedExtCombo -> ExtCombo
extCombo = toList . extCombo' Nothing 
  where 
    extCombo' s1 (ParsedExtCombo c s2 t) = extCombo' (Just s2) c `snoc` extTrick s1 t (Just s2)
    extCombo' s1 (ParsedExtComboTrick t) = singleton $ extTrick s1 t Nothing

extTrick :: Maybe Separator -> ParsedExtTrick -> Maybe Separator -> ExtTrick
extTrick s1 (ParsedExtTrick t p s c) s2 = ExtTrick t' p' s' c'
  where
    t' = trickWithDefaults t
    p' = case s1 of
           Just SepCont  -> notouch p
           Just SepCatch -> notouch p
           _             -> touch p
    c' = case s2 of
           Just SepCont  -> notouch c
           Just SepPush  -> notouch c
           _             -> touch c
    s' = case s of
           Just n  -> n
           Nothing -> trickRot t'

    touch Nothing          = Just $ Slot []
    touch (Just Nothing)   = Just $ Slot []
    touch (Just something) = something
    
    notouch Nothing          = Nothing
    notouch (Just Nothing)   = Just $ Slot []
    notouch (Just something) = something

