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
{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Haspin.Standard.Logic where
import Haspin.Standard.Data
import Haspin.Standard.StrictParser
import Control.Monad
import Data.List
import Data.DList (singleton, snoc, toList)
import Data.Maybe
import Data.Function

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
trickWithDefaults (ParsedTrick name dir rot start stop _)  =
    Trick name dir' rot' start' stop' [] 
  where trickDefaults = find (\t -> name == trickName t) defaults
        start' = findJust [start, liftM trickStart trickDefaults] $
            Slot [Index, Middle]
        stop' = findJust [stop, start, liftM trickStop trickDefaults] start'
        dir' = findJust [dir, liftM trickDir trickDefaults] Normal
        rot' = findJust [rot, liftM trickRot trickDefaults] $ Rotation 2
        findJust :: [Maybe a] -> a -> a
        findJust l d = fromMaybe d $ msum l

extCombo :: ParsedExtCombo -> ExtCombo
extCombo c = toList $ extCombo' Nothing c Nothing
  where 
    extCombo' s1 (ParsedExtCombo c s' t) s2 = extCombo' s1 c (Just s') `snoc` extTrick (Just s') t s2
    extCombo' s1 (ParsedExtComboTrick t) s2 = singleton $ extTrick s1 t s2

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

dumbify :: ExtCombo -> ExtCombo
dumbify (e1@(ExtTrick {extTrickTrick = t1})
        :e2@(ExtTrick {extTrickTrick = t2})
        :xs)

    | Nothing <- extTrickCatch e1
    , Nothing <- extTrickPush e2
    , on (==) extTrickTrick e1 e2
    = (e1 { extTrickSpin = on (+) extTrickSpin e1 e2
          , extTrickCatch = extTrickCatch e2
          }
      ):xs
dumbify c = c
    
isInterrupted :: ExtTrick -> Boolean
isInterrupted (ExtTrick { extTrickPush = Just []
                        , extTrickCatch = Just []
                        , extTrickTrick = Trick { trickRot = r }
                        , extTrickSpin = r'
                        }
    = r == r'
isInterrupted _ = False

instance Ord Zone where
    compare z1 z2 | z1s < z2s && z1e <= z2e = LT
                  | z1s <= z2s && z1e < z2e = LT
                  | z1s > z2s && z1e >= z2e = GT
                  | z1s >= z2s && z1e > z2e = GT
                  | otherwise               = EQ

      where z1s = minimum z1
            z1e = maximum z1
            z2s = minimum z2
            z2e = maximum z2

baseTrick t@(trickRot -> r) = ExtTrick t (Just []) r (Just [])
uninterrupted = baseTrick . extTrickTrick

extTrickStop = trickStop . extTrickTrick
extTrickStart = trickStart . extTrickTrick
extTrickRot = trickRot . extTrickTrick

-- This tries to find which portion exactly of a trick is executed in an
-- interrupted trick. It is actually ambiguous and non-trivial.
portion :: ExtTrick -> ExtTrick -> ExtTrick -> Maybe (Rotation, Rotation)
portion t before after = portion' t (extTrickStop before) (extTrickStart after)

portion' :: ExtTrick -> Zone -> Zone -> Maybe (Rotation, Rotation)
portion' (portion1 -> p@(Just _)) _ _ = p
portion' _ _ _ = Nothing -- The default case is for when we really don't know.
                         -- Shouldn't I use the List/non-determinist monad here instead?

portion1 :: ExtTrick -> Maybe (Rotation, Rotation)
portion1 (ExtTrick (trickRot -> rot) (Just _) s Nothing ) _ _ = Just (Rotation 0, spin)
portion1 (ExtTrick (trickRot -> rot) Nothing  s (Just _)) _ _ = Just (rot - spin, rot)


{-- This assumption is incorrect. For now, let's settle for just considering that a push means it may start here
mayStartAtBeginning (ExtTrick { extTrickPush = Just [] }) = True
mayStartAtBeginning (ExtTrick { extTrickPush = Just z'
                              , extTrickTrick = Trick { trickStart = z }}) | z' `subset` z = True 
  where subset z' z = all (`elem` z) z'
mayStartAtBeginning _ = False
--}

takeComboPart :: Rotation -> Rotation -> [ExtTrick] -> [ExtTrick]
takeComboPart 0 0 _ = []
takeComboPart 0 stop (x@(extTrickSpin -> spin):xs) | stop >= spin = x : takeComboPart 0 (stop - spin) xs
                                                   | otherwise    = x { extTrickSpin = stop }
takeComboPart start stop (x@(extTrickSpin -> spin):xs) | start >= spin = takeComboPart (start - spin) (stop - spin) xs
                                                       | otherwise     = x { extTrickPush = Nothing
                                                                           , extTrickSpin = spin - start}
                                                                       : takeComboPart 0 (stop - (spin - start)) xs
takeComboPart _ _ [] = []

sortTuple (a,b) | GT <- compare a b = (b,a)
sortTuple x = x 

aroundName start stop = aroundName' (start,stop) ++ "Around"
  where aroundName' (maximum &&& minimum . sortTuple -> (low, high)) | low == high = show start
                                                                     | otherwise   = show start ++ show stop

-- the nice version decomposes when it can, and leaves the rest as-is
decompose1 (decomposeAll1 -> Just e) = e
decompose1 e = e

-- the un-nice version decomposes until it can't do more, or returns Nothing to complain
decomposeAll1 (e@(ExtTrick {extTrickTrick = t})
    | isInterrupted e, Just (r,r') <- portion1 e
    = Just $ takeComboPart r r' $ decompose1 $ uninterrupted e

    | not (isInterrupted e) && trickName t == "Twisted Sonic"
    = Just $ [ ExtTrick { extTrickTrick = t { trickName  = "Charge"
                                            , trickRot   = Rotation 2
                                            , trickStop  = trickStart t
                                            }
                        , extTrickPush  = Just []
                        , extTrickSpin  = Rotation 2
                        , extTrickCatch = Nothing
                        }
             , ExtTrick { extTrickTrick = t { trickName = aroundName (trickStart t) (trickStop t)
                                            , trickDir   = if trickStart t <= trickStop t then Normal else Reverse
                                            , trickRot   = Rotation 1
                                            }
                        , extTrickPush  = Just []
                        , extTrickSpin  = Rotation 2
                        , extTrickCatch = Nothing
                        }
             ]
    | isFundamental e = Just e
    | otherwise = Nothing

isFundamental (extTrickName -> n) = n `elem` ["Charge", "Wiper"] || ("Around" `isSuffixOf` n) -- This is terrible, where are regexen?

