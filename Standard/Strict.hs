module Haspin.Standard.Strict where
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String
import Control.Applicative
import Control.Monad
import Data.Maybe

type Combo = [Trick]
data Trick = Trick { trickName  :: String
                   , trickDir   :: Direction
                   , trickRot   :: Rotation
                   , trickStart :: Slot
                   , trickStop  :: Slot
                   }
data Direction = Normal | Reverse
newtype Rotation = Rotation Integer
data Zone = Pinky | Ring | Middle | Index | Thumb | Palm | Back
newtype Slot = Slot [Zone]

instance Show Slot where
    show (Slot s) = map showZone s
      where
        showZone Pinky = '4'
        showZone Ring = '3'
        showZone Middle = '2'
        showZone Index = '1'
        showZone Thumb = 'T'
        showZone Palm = 'P'
        showZone Back = 'B'

instance Show Rotation where
    show (Rotation r) = show (fromInteger r / 2)

instance Show Trick where
    show (Trick name dir rot start stop) =
        name ++ " " ++
        show dir ++ " " ++
        show rot ++ " " ++
        show start ++ "-" ++ show stop

instance Show Direction where
    show Normal = "normal"
    show Reverse = "reverse"

defaults = "Thumbaround normal 1.0 12-12"

parseCombo :: Parser Combo
parseCombo = trick `sepBy` separator <* eof
separator  = whitespace >> string ">" >> whitespace
whitespace = skipMany space
trick      = do
    name <- pTrickName
    whitespace
    dir <- optional $ direction <* whitespace
    rot <- optional $ rotation <* whitespace
    startstop <- optional $ do
        start <- slot
        stop <- optional $string "-" *> slot
        return (start, stop)

    let start = maybe (Slot [Index, Middle]) fst startstop
        stop = fromMaybe start $ maybe Nothing snd startstop
        dir_ = fromMaybe Normal dir
        rot_ = fromMaybe (Rotation 2) rot
    return $ Trick name dir_ rot_ start stop

pTrickName = many1 letter
direction = (string "normal" >> return Normal)
        <|> (string "reverse" >> return Reverse)
rotation = do
    int <- some digit 
    string "."
    half <- (string "0" >> return 0)
        <|> (string "5" >> return 1)

    return $ Rotation $ (read int) * 2 + half

slot = liftM Slot $ some ((char '4' >> return Pinky)
                      <|> (char '3' >> return Ring)
                      <|> (char '2' >> return Middle)
                      <|> (char '1' >> return Index)
                      <|> (char 'T' >> return Thumb)
                      <|> (char 'P' >> return Palm)
                      <|> (char 'B' >> return Back)
                      <?> "slot")

