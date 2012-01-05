module Haspin.Standard.StrictParser where
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

data ParsedExtTrick = ParsedExtTrick { parExtTrick :: ParsedTrick
                                     , parExtPush  :: Maybe (Maybe Slot)
                                     , parExtSpin  :: Maybe Rotation
                                     , parExtCatch :: Maybe (Maybe Slot)
                                     }
  deriving Show
data ParsedExtCombo = ParsedExtCombo { parExtComTrick  :: ParsedExtTrick,
                                       parExtComSep    :: Separator,
                                       parExtComTricks :: ParsedExtCombo }
                    | ParsedExtComboEnd ParsedExtTrick
  deriving Show

unsafeParse parser parsed = fromRight $! parse parser "(hardcoded)" parsed
  where fromRight (Right a) = a
unsafeCombo = unsafeParse parseCombo
unsafeTrick = unsafeParse parseTrick

parseExtCombo :: Parser ParsedExtCombo
parseExtCombo = (try $ do t <- parseExtTrick
                          s <- parseSeparator
                          c <- parseExtCombo
                          return $ ParsedExtCombo t s c)
            <|> (liftM ParsedExtComboEnd parseExtTrick <* eof)

parseExtTrick = do
    t <- parseTrick
    p <- optional $ bracketed "p" $ optional slot
    s <- optional $ bracketed "s" rotation
    c <- optional $ bracketed "c" $ optional slot
    return $ ParsedExtTrick t p s c
  where
    bracketed name parser = open name *> parser <* close
    open name = lexeme $ string $ "[" ++ name
    close = lexeme $ string "]"

lexeme = (<* whitespace)
whitespace = skipMany space

parseCombo = parseTrick `sepBy` lexeme (string ">") <* eof

parseSeparator = lexeme ((SepCatch <$ try (string ">~"))
                     <|> (SepPush  <$ try (string "~>"))
                     <|> (SepCont  <$ string "~")
                     <|> (SepThen  <$ string ">")
                     <?> "trick separator")


parseTrick = lexeme $ do
    name <- parseTrickName
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

parseTrickName = many1 letter

direction = (Normal <$ string "normal")
        <|> (Reverse <$ string "reverse")
rotation = do
    int <- some digit 
    string "."
    half <- (0 <$ string "0")
        <|> (1 <$ string "5")

    return $ Rotation $ read int * 2 + half

slot = liftM Slot $ some ((Pinky  <$ char '4')
                      <|> (Ring   <$ char '3')
                      <|> (Middle <$ char '2')
                      <|> (Index  <$ char '1')
                      <|> (Thumb  <$ char 'T')
                      <|> (Palm   <$ char 'P')
                      <|> (Back   <$ char 'B')
                      <?> "slot")
