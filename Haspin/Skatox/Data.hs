module Haspin.Skatox.Data where

data Combo = Combo Slot Direction Rotation Combo
           | ComboSlot Slot
  deriving Show

data Slot = Slot [SZone] Axe [SZone]
  deriving Show

data SZone = SZone { szZone     :: Zone
                   , szContact  :: Bool
                   , szPhalange :: Phalange }
  deriving Show

data Phalange = PhalangeX | PhalangeY | PhalangeZ | PhalangeNone deriving Show
data Axe = AxeSingle Axe' | AxeCouple Axe' Axe' | AxeNone deriving Show
data Axe' = AxeVert | AxeHoriz | AxeFacing | AxeOblique deriving Show

-- BEGIN duplicated from Haspin.Standard.Data for reasons I can't understand
data Direction = Normal | Reverse deriving Show
data Zone = Pinky | Ring | Middle | Index | Thumb | Palm | Back deriving Show
newtype Rotation = Rotation Integer deriving Show

showRot (Rotation r) = show (fromInteger r / 2)

showZone Pinky = '4'
showZone Ring = '3'
showZone Middle = '2'
showZone Index = '1'
showZone Thumb = 'T'
showZone Palm = 'P'
showZone Back = 'B'
-- END

showCombo :: Combo -> String
showCombo (Combo s d r c) = showSlot s ++ " "
                         ++ showRot' d r ++ " "
                         ++ showCombo c
showCombo (ComboSlot s) = showSlot s

showRot' Normal r = "N" ++ showRot r ++ ">"
showRot' Reverse r = "R" ++ showRot r ++ ">"
showSlot (Slot sz1 a sz2) = concatMap showSZone sz1
                         ++ showAxe a
                         ++ concatMap showSZone sz2

showAxe (AxeSingle a) = showAxe' a
showAxe (AxeCouple a b) = showAxe' a ++ showAxe' b
showAxe AxeNone = "?"

showAxe' AxeVert = "|"
showAxe' AxeHoriz = "_"
showAxe' AxeFacing = "."
showAxe' AxeOblique = "/"

showSZone (SZone s True p) = "(" ++ showSZone (SZone s False p) ++ ")"
showSZone (SZone s False p) = [showZone s] ++ showPhalange p

showPhalange PhalangeX = "x"
showPhalange PhalangeY = "y"
showPhalange PhalangeZ = "z"
showPhalange PhalangeNone = ""
