module Haspin.Standard.Data where

import Data.List

type Combo = [Trick]
type InterruptedCombo = [ExtendedTrick]
data Trick = Trick { trickName  :: String
                   , trickDir   :: Direction
                   , trickRot   :: Rotation
                   , trickStart :: Slot
                   , trickStop  :: Slot
                   }
    deriving Show

data ExtendedTrick = ExtendedTrick { extTrickTrick :: Trick
                                   , extTrickPush  :: Maybe Slot
                                   , extTrickSpin  :: Rotation
                                   , extTrickCatch :: Maybe Slot
                                   }
    deriving Show

data Direction = Normal | Reverse deriving Show
newtype Rotation = Rotation Integer deriving Show
data Zone = Pinky | Ring | Middle | Index | Thumb | Palm | Back deriving Show
newtype Slot = Slot [Zone] deriving Show

verboseExtendedCombo c = inter (map verboseExtendedTrick c)
                               (intermap separator c)
  where
    separator a b = if isHybrid a b then " ~ " else " > "
    isHybrid a b = extTrickCatch a == Nothing || extTrickPush b == Nothing

    inter l [] = l
    inter [] l = l
    inter (x:xs) (y:ys) = x:y:inter xs ys

    -- l being empty is not an exception thanks to lazy eval
    intermap f l = zipWith f l $ tail l

verboseExtendedTrick = const ""

verboseCombo = intercalate " > " . map verboseTrick

verboseTrick (Trick name dir rot start stop) =
    name ++ " " ++
    showDir dir ++ " " ++
    showRot rot ++ " " ++
    showSlot start ++ "-" ++ showSlot stop

showRot (Rotation r) = show (fromInteger r / 2)
showDir Normal = "normal"
showDir Reverse = "reverse"
showSlot (Slot s) = map showZone s
  where
    showZone Pinky = '4'
    showZone Ring = '3'
    showZone Middle = '2'
    showZone Index = '1'
    showZone Thumb = 'T'
    showZone Palm = 'P'
    showZone Back = 'B'

