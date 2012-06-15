{-# LANGUAGE PatternGuards #-}
module Haspin.Standard.Data where

import Data.List
import Data.Function

type Combo = [Trick]
type ExtCombo = [ExtTrick]
data Trick = Trick { trickName  :: String
                   , trickDir   :: Direction
                   , trickRot   :: Rotation
                   , trickStart :: Slot
                   , trickStop  :: Slot
                   , trickNotes :: [TrickAnnotation]
                   }
    deriving (Show, Eq)

data ExtTrick = ExtTrick { extTrickTrick :: Trick
                         , extTrickPush  :: Maybe Slot
                         , extTrickSpin  :: Rotation
                         , extTrickCatch :: Maybe Slot
                         }
    deriving (Show, Eq)

data TrickAnnotation = PalmUp
                     | PalmDown 
                     | PalmSide 
                     | WeissanStyle 
                     | KoreanStyle 
                     | SwivelStyle 
                     | TrickAnnotation String
    deriving (Show, Eq)

data Separator = SepThen | SepCatch | SepPush | SepCont
  deriving (Show, Eq)
data Direction = Normal | Reverse
  deriving (Show, Eq)
newtype Rotation = Rotation {rotation :: Integer}
  deriving (Show, Eq, Ord)

instance Num Rotation where -- couldn't this be shortened?
    (Rotation r) + (Rotation r') = Rotation $ r + r'
    (Rotation r) - (Rotation r') = Rotation $ r - r'
    (Rotation r) * (Rotation r') = Rotation $ r * r'
    abs (Rotation r) = Rotation (abs r)
    signum (Rotation r) = Rotation (signum r)
    fromInteger = Rotation

data Zone = Pinky | Ring | Middle | Index | Thumb | Palm | Back
  deriving (Show, Eq, Ord)
newtype Slot = Slot [Zone]
  deriving (Show, Eq)

verboseExtCombo :: ExtCombo -> String
verboseExtCombo c = concat $ inter (map verboseExtTrick c)
                                   (intermap separator c)
  where
    separator a b = if isHybrid a b then " ~ " else " > "

    isHybrid a b | Nothing <- extTrickCatch a = True
                 | Nothing <- extTrickPush b  = True
                 | otherwise                  = False

    inter l [] = l
    inter [] l = l
    inter (x:xs) (y:ys) = x:y:inter xs ys

    -- l being empty is not an exception thanks to lazy eval
    intermap f l = zipWith f l $ tail l

verboseExtTrick (ExtTrick t p s c) =
    verboseTrick t ++ " "
    ++ bracketed "p" p
    ++ "[s " ++ showRot s ++ "]"
    ++ bracketed "c" c
  where
    bracketed _ Nothing          = ""
    bracketed l (Just (Slot [])) = "[" ++ l ++ "]"
    bracketed l (Just s)         = "[" ++ l ++ " " ++ showSlot s ++ "]"

verboseCombo = intercalate " > " . map verboseTrick

verboseTrick (Trick name dir rot start stop _) =
    name ++ " " ++
    showDir dir ++ " " ++
    showRot rot ++ " " ++
    showSlot start ++ "-" ++ showSlot stop

showRot (Rotation r) = show (fromInteger r / 2)
showDir Normal = "normal"
showDir Reverse = "reverse"
showSlot (Slot s) = map showZone s

showZone Pinky = '4'
showZone Ring = '3'
showZone Middle = '2'
showZone Index = '1'
showZone Thumb = 'T'
showZone Palm = 'P'
showZone Back = 'B'

