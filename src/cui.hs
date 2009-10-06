module CUI where

import System ( getArgs )
import System.IO
import Data.Char
import Data.Maybe
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
        otherwise -> \_ -> usage

executeShift [inputFile, outputFile, seconds] = do
    subs <- readSubtitlesFile inputFile
    let intSeconds = read seconds :: Int
    let timing = convertToTiming $ 1000 * intSeconds
    let newSubs = map (flip shiftSubtitle timing) subs
    let outputContent = renderSrtFile newSubs
    writeFile outputFile outputContent


executeShift _ = do
    putStr "arguments for shift: inputfile.srt outputfile.srt seconds"

executeSplit args = do
    putStr "do executeSplit"

executeAppend args = do
    putStr "do executeAppend"

readSubtitlesFile :: FilePath -> IO Subtitles
readSubtitlesFile filename = do
    inputContent <- readFile filename
    case readSubtitlesOrError inputContent of
        Left errMsg -> do
            putStrLn $ "Parse error: " ++ errMsg
            return []
        Right subs -> return subs

