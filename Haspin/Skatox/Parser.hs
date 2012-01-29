module Haspin.Skatox.Parser where
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

import Haspin.Skatox.Data

import Control.Applicative
import Control.Monad

-- BEGIN copy-paste
lexeme = (<* whitespace)
lexeme1 = (<* whitespace1)
whitespace1 = skipMany1 space
whitespace = whitespace1 <|> return ()
-- END

parseRot = do
    int <- some digit 
    string "."
    half <- (0 <$ string "0")
        <|> (1 <$ string "5")
    char '>'
    whitespace

    return $ Rotation $ read int * 2 + half
  <?> "rotation"

parseCombo :: Parser Combo
parseCombo = parseSlot >>= rest
  where
    rest :: Slot -> Parser Combo
    rest s = do d <- parseDir
                r <- parseRot
                s' <- parseSlot
                c <- rest s'
                
                return (Combo s d r c)
           <|> return (ComboSlot s)

parseDir = Normal <$ char 'N'
       <|> Reverse <$ char 'R'

parseSlot :: Parser Slot
parseSlot = lexeme $ liftM3 Slot (some parseSZone) parseAxe (some parseSZone)

parseSZone = parseZoneNoContact <|> parseZoneContact
  where
    parseZoneNoContact = do
        char '('
        sz <- parseZoneContact
        char ')'
        return sz { szContact = True }
    parseZoneContact = do
        z <- parseZone
        p <- parsePhalange
        return $ SZone z False p
        
parsePhalange = (PhalangeX <$ char 'x')
            <|> (PhalangeY <$ char 'y')
            <|> (PhalangeZ <$ char 'z')
            <|> return PhalangeNone

parseZone = Pinky  <$ char '4'
        <|> Ring   <$ char '3'
        <|> Middle <$ char '2'
        <|> Index  <$ char '1'
        <|> Thumb  <$ char 'T'
        <|> Palm   <$ char 'P'
        <|> Back   <$ char 'B'
        <?> "zone"

parseAxe =
    do { a <- parseAxe'
       ; do { char '/'
            ; b <- parseAxe'
            ; return (AxeCouple a b)
            } <|> return (AxeSingle a)
       } <|> AxeNone <$ char '?'

parseAxe' = AxeVert    <$ char '|'
        <|> AxeHoriz   <$ char '_'
        <|> AxeFacing  <$ char '.'


