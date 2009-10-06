module CUI where

import System ( getArgs )
import Data.Char
import SRT
import Timing
import Subtitle


cui_main = do
    args <- getArgs
    if length args == 0
        then usage
        else dispatch (head args) (tail args)

usage = do
    putStrLn "usage: subsuber command [args...]"
    putStrLn "commands: "
    putStrLn "    shift inputfile.srt outputfile.srt seconds"
    putStrLn "    split inputfile.srt timing"
    putStrLn "    append inputfile1.srt imputfile2.srt outputfile.srt"


dispatch command =
    case map toLower command of
        "shift" -> executeShift
        "split" -> executeSplit
        "append" -> executeAppend

executeShift args = do
    putStr "do executeShift"

executeSplit args = do
    putStr "do executeSplit"

executeAppend args = do
    putStr "do executeAppend"
