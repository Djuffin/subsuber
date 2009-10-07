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
    putStrLn "    split inputfile.srt seconds"
    putStrLn "    append inputfile1.srt imputfile2.srt outputfile.srt"


dispatch command =
    case map toLower command of
        "shift" -> executeShift
        "split" -> executeSplit
        "append" -> executeAppend
        otherwise -> \_ -> usage

executeShift [inputFile, outputFile, timingStr] = do
    subs <- readSubtitlesFile inputFile
    let timing = readTiming timingStr
    let newSubs = map (flip shiftSubtitle timing) subs
    let outputContent = renderSrtFile newSubs
    writeFile outputFile outputContent


executeShift _ = do
    putStr "arguments for shift: inputfile.srt outputfile.srt seconds"

executeSplit [inputFile, timingStr] = do
    subs <- readSubtitlesFile inputFile
    let timing = readTiming timingStr
    let part1subs = allBefore timing subs
    let part2subs = allAfter timing subs
    let part1Content = renderSrtFile part1subs
    let part2Content = renderSrtFile part2subs
    writeFile (inputFile ++ "-part1") part1Content
    writeFile (inputFile ++ "-part2") part2Content


executeSplit _ = do
    putStr "arguments for split: inputfile.srt seconds"


executeAppend args = do
    putStr "do executeAppend"

readTiming :: String -> Timing
readTiming seconds = convertToTiming $ 1000 * read seconds

readSubtitlesFile :: FilePath -> IO Subtitles
readSubtitlesFile filename = do
    inputContent <- readFile filename
    case readSubtitlesOrError inputContent of
        Left errMsg -> do
            putStrLn $ "Parse error: " ++ errMsg
            return []
        Right subs -> return subs

