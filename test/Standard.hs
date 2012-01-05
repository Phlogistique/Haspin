module Haspin.Test.Standard where
import Haspin.Standard.Data
import Haspin.Standard.Logic
import Haspin.Standard.StrictParser
import System.Environment
import Text.Parsec

defaultCombo = "Shadow normal 1.5 12-12 [p][s 0.5] ~ Backhand spin normal P-P [s 0.5] ~ Shadow normal 1.5 12-12 [s 0.5][c] > Pinkyswivel 1.0 P-B [p][s 1.0] ~ Backhand spin B-12 [s 0.5][c]"

main = do
    args <- getArgs
    let combo = if null args then defaultCombo else args !! 1
        parsedCombo = parse parseExtCombo "(test)" combo
    case parsedCombo of
      Left err  -> print $ err
      Right res -> putStrLn $ verboseExtCombo $ extCombo res


