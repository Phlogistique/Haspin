module Haspin.Standard.StrictParser where
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Char
import Data.List

import Haspin.Standard.Data

data ParsedTrick = ParsedTrick { parsedTrickName  :: String
                               , parsedTrickDir   :: Maybe Direction
                               , parsedTrickRot   :: Maybe Rotation
                               , parsedTrickStart :: Maybe Slot
                               , parsedTrickStop  :: Maybe Slot
                               , parsedTrickNote  :: [TrickAnnotation]
                               }
  deriving Show

data ParsedExtTrick = ParsedExtTrick { parExtTrick :: ParsedTrick
                                     , parExtPush  :: Maybe (Maybe Slot)
                                     , parExtSpin  :: Maybe Rotation
                                     , parExtCatch :: Maybe (Maybe Slot)
                                     }
  deriving Show
data ParsedExtCombo = ParsedExtCombo { parExtComLeft   :: ParsedExtCombo,
                                       parExtComSep    :: Separator,
                                       parExtComRight  :: ParsedExtTrick }
                    | ParsedExtComboTrick ParsedExtTrick
  deriving Show

unsafeParse parser parsed = fromRight $! parse parser "(hardcoded)" parsed
  where fromRight (Right a) = a

unsafeCombo :: String -> [ParsedTrick]
unsafeCombo = unsafeParse parseCombo
unsafeTrick :: String -> ParsedTrick
unsafeTrick = unsafeParse parseTrick

parseExtCombo :: Parser ParsedExtCombo
parseExtCombo = chainl1 parseExtTrick' parseSeparator'' <* eof
  where
    parseExtTrick' :: Parser ParsedExtCombo
    parseExtTrick' = liftM ParsedExtComboTrick parseExtTrick

    parseSeparator' :: Separator -> ParsedExtCombo -> ParsedExtCombo -> ParsedExtCombo
    parseSeparator' sep t1 (ParsedExtComboTrick t2) = ParsedExtCombo t1 sep t2

    parseSeparator'' :: Parser (ParsedExtCombo -> ParsedExtCombo -> ParsedExtCombo)
    parseSeparator'' = do
        sep <- parseSeparator
        return $ parseSeparator' sep
    -- parseSeparator' c@(ParsedExtCombo {}) (ParsedExtComboTrick t) =
    --     ParsedExtCombo c (liftM parseSeparator) t

parseExtTrick = lexeme $ do
    t <- parseTrick
    p <- optional $ bracketed "p" $ optional slot
    s <- optional $ bracketed "s" rotation
    c <- optional $ bracketed "c" $ optional slot
    return $ ParsedExtTrick t p s c
  where
    bracketed name parser = try (open name *> parser <* close)
    open name = lexeme $ string $ "[" ++ name
    close = lexeme $ string "]"

lexeme = (<* whitespace)
lexeme1 = (<* whitespace1)
whitespace1 = skipMany1 space
whitespace = whitespace1 <|> return ()

parseCombo = parseTrick `sepBy` lexeme (string ">") <* eof

parseSeparator :: Parser Separator
parseSeparator = lexeme ((SepCatch <$ try (string ">~"))
                     <|> (SepPush  <$ try (string "~>"))
                     <|> (SepCont  <$ string "~")
                     <|> (SepThen  <$ string ">")
                     <?> "trick separator")

parseTrick = lexeme $ do
    startAnnotations <- many parseAnnotation 
    trick <- parseTrick'
    endAnnotations <- many parseAnnotation
    return $ annotate trick (startAnnotations ++ endAnnotations)
  where
    annotate :: ParsedTrick -> [TrickAnnotation] -> ParsedTrick
    annotate trick notes = trick { parsedTrickNote = parsedTrickNote trick ++ notes }

parseTrick' = lexeme $ do
    name <- parseTrickName
    whitespace
    dir <- try $ optional $ direction <* whitespace
    rot <- try $ optional $ rotation <* whitespace
    (start,stop) <- slots

    return $ ParsedTrick name dir rot start stop []

slots = nestedOptional slot (string "-" *> slot)
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

parseTrickName = do words <- manyTill (lexeme $ many1 letter) $
                        lookAhead $ try $ do
                            skip direction
                                <|> skip rotation
                                <|> skip slots
                            notFollowedBy letter
                    return $ intercalate " " words

stringi name = walk name >> return name
  where
    walk []     = return ()
    walk (c:cs) = (caseChar c <?> msg) >> walk cs

    caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                | otherwise  = char c

    msg         = show name


skip p = do
    p
    return ()

direction = (Normal <$ stringi "normal")
        <|> (Reverse <$ stringi "reverse")

rotation = do
    int <- some digit 
    string "."
    half <- (0 <$ string "0")
        <|> (1 <$ string "5")

    return $ Rotation $ read int * 2 + half
  <?> "rotation"

parseAnnotation = lexeme $ do
    string "("
    text <- manyTill anyChar (string ")")
    string ")"

    return $ TrickAnnotation text


slot = (liftM Slot $ some ((Pinky  <$ char '4')
                       <|> (Ring   <$ char '3')
                       <|> (Middle <$ char '2')
                       <|> (Index  <$ char '1')
                       <|> (Thumb  <$ char 'T')
                       <|> (Palm   <$ char 'P')
                       <|> (Back   <$ char 'B'))) <?> "slot"

