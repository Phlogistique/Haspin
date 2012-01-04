module Haspin.Standard.Strict where
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List

import Haspin.Standard.Data

data ParsedTrick = ParsedTrick { parsedTrickName  :: String
                               , parsedTrickDir   :: Maybe Direction
                               , parsedTrickRot   :: Maybe Rotation
                               , parsedTrickStart :: Maybe Slot
                               , parsedTrickStop  :: Maybe Slot
                               }
  deriving Show

data ParsedExtended = ParsedExtended { parExtTrick :: ParsedTrick
                                     , parExtPush  :: Maybe $ Maybe Slot
                                     , parExtSpin  :: Maybe $ Maybe Slot
                                     , parExtCatch :: Maybe $ Maybe Slot
                                     }

defaults = map trickWithDefaults $ unsafeParse
    "Thumbaround normal 1.0 T12-T1 > \
    \Sonic normal 1.0 23-12 > \
    \Pinkyswivel normal 1.0 12-23"

unsafeParse parser parsed = fromRight $! parse parser "(hardcoded)" parsed
  where fromRight (Right a) = a

nameEquivalences = [("Swivel", "Pinkyswivel")]
trickEquivalence =
    [("Antigravity", unsafeTrick "Fingerless Indexaround Reverse 11-11")]

unsafeTrick = unsafeParse trick

parseCombo :: Parser [ParsedTrick]
parseExtendedCombo :: Parser [ParsedExtended]
parseCombo = parseTrick `sepBy` separator <* eof
separator  = whitespace >> string ">" >> whitespace
whitespace = skipMany space
parseTrick = do
    name <- pTrickName
    whitespace
    dir <- optional $ direction <* whitespace
    rot <- optional $ try $ rotation <* whitespace
    (start,stop) <- nestedOptional slot (string "-" *> slot)

    return $ ParsedTrick name dir rot start stop
  where
    nestedOptional :: Parser a -> Parser a -> Parser (Maybe a, Maybe a)
    nestedOptional a b = liftM maymay $ optional $ do
        a' <- a
        b' <- optional b
        return (a', b')
    maymay :: Maybe (a, Maybe a) -> (Maybe a, Maybe a)
    maymay Nothing = (Nothing,Nothing)
    maymay (Just (a, Nothing)) = (Just a,Nothing)
    maymay (Just (a, Just b)) = (Just a,Just b)

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

pTrickName = many1 letter
direction = (string "normal" >> return Normal)
        <|> (string "reverse" >> return Reverse)
rotation = do
    int <- some digit 
    string "."
    half <- (string "0" >> return 0)
        <|> (string "5" >> return 1)

    return $ Rotation $ read int * 2 + half

slot = liftM Slot $ some ((char '4' >> return Pinky)
                      <|> (char '3' >> return Ring)
                      <|> (char '2' >> return Middle)
                      <|> (char '1' >> return Index)
                      <|> (char 'T' >> return Thumb)
                      <|> (char 'P' >> return Palm)
                      <|> (char 'B' >> return Back)
                      <?> "slot")
