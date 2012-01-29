module Haspin.Test.Standard where
import Haspin.Skatox.Data
import Haspin.Skatox.Parser

import Data.List
import System.Environment
import Text.Parsec

defaultCombo = "T?21 R1.0> T2?1"

main = do
    args <- getArgs
    let combo = if null args
                then defaultCombo
                else intercalate " " args
        parsedCombo = parse parseCombo "(test)" combo
    case parsedCombo of
      Left err  -> print combo >> print err
      Right res -> do
        putStrLn combo
        putStrLn $ showCombo res


