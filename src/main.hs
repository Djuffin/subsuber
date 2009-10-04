module Main () where

import Control.Monad

import SRT
import Timing
import Subtitle
import Tests


f = "1\n" ++
    "00:02:26,407 --> 00:02:31,356 X1:100 X2:100 Y1:100 Y2:100\n"++
    "<font color=#00ff00>Detta handlar om min storebrors</font>\n"++
    "<b><i><u>kriminella beteende och foersvinnade.</u></i></b>\n"++
    "\n"++
    "2\n"++
    "00:02:31,567 --> 00:02:37,164\n"++ 
    "Vi talar inte laengre om Wade. Det aer\n"++ 
    "som om han aldrig hade existerat."

getSrt :: [Subtitle] 
getSrt = case readSrtFile f of
    Left err -> []
    Right s -> s --map (flip shiftSubtitle $ negative $ Timing (-1) 10 0 0) s


e = allAfter (Timing 0 2 31 561) getSrt

main = run_tests
--putStr $ renderSrtFile e
