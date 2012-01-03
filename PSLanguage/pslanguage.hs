module PSLanguage where

import Text.Parsec.String

data Finger = Index
            | Middle
            | Ring
            | Pinky
            | Thumb

data HandPart =
              Finger Finger
            | Palm
            | BackHand

type Combo = [ObjectDefinition]

data ObjectDefinition = ObjectDefinition String

data Token = Operator Operator
           | Identifier String
           | Comma
           | ParenOpen
           | ParenClose
           | CurlyOpen
           | CurlyClose

data Operator = Touching | Above

tokenize :: Parser Token
tokenize = return $ Identifier "Thumbaround"

parse :: Parser Combo
parse = return []

