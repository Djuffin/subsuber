{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module Tests  where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Batch

import SRT
import Timing
import Subtitle


-- Timing Tests
instance Arbitrary Timing where
    arbitrary = do
        hours <- choose (0, 60) :: Gen Int
        mins <- choose (0, 60) :: Gen Int
        secs <- choose (0, 60) :: Gen Int
        mscs <- choose (0, 1000) :: Gen Int
        return $ Timing hours mins secs mscs
    coarbitrary t = variant $ convertToMilliseconds t

prop_negative t = (hours t == - hours nt)
    && (minutes t == - minutes nt)
    && (seconds t == - seconds nt)
    && (milliseconds t == - milliseconds nt)
    where nt = negative t

prop_double_negative :: Timing -> Bool
prop_double_negative t =
    t == (negative $ negative t)

prop_eq :: Timing -> Bool
prop_eq t = t == t

prop_convert1 t =
    (convertToMilliseconds t) == (convertToMilliseconds t)

prop_convert2 t =
    (convertToTiming $ convertToMilliseconds t) == (convertToTiming $ convertToMilliseconds t)

prop_convert3 t =
    (convertToMilliseconds $ convertToTiming t) == (convertToMilliseconds $ convertToTiming t)

-- Subtitles Tests

instance Arbitrary Char where
    arbitrary = elements (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "/.,?><\';|:][}{~!@#$%^&*()_")
    coarbitrary c = variant $ fromEnum c


instance Arbitrary Subtitle where
    arbitrary = do
        i <- arbitrary
        b <- arbitrary
        e <- arbitrary
        text <- arbitrary
        return $ Subtitle i b e text
    coarbitrary s = variant $ (index s) + (convertToMilliseconds $ beginning s)

prop_ordered :: Subtitles -> Bool
prop_ordered subs = ordered $ order subs
    where   ordered [] = True
            ordered [_] = True
            ordered (x:y:xs) = x <= y && (index x + 1 == index y) && ordered (y:xs)

prop_shift :: Subtitle -> Timing -> Bool
prop_shift s t = (beginning s' == b) && (end s' == e)
    where   s' = shiftSubtitle s t
            b = addTimings (beginning s) t
            e = addTimings (end s) t

prop_allAfter :: [Subtitle] -> Timing -> Bool
prop_allAfter subs t = all check after
    where   after = allAfter t subs
            check s = beginning s > t

prop_allBefore :: [Subtitle] -> Timing -> Bool
prop_allBefore subs t = all check before
    where   before = allBefore t subs
            check s = beginning s <= t

-- SRT parsing Tests

test_subtitles_text = "1\n" ++
    "00:02:26,407 --> 00:02:31,356 X1:100 X2:100 Y1:100 Y2:100\n"++
    "<font color=#00ff00>Detta handlar om min storebrors</font>\n"++
    "<b><i><u>kriminella beteende och foersvinnade.</u></i></b>\n"++
    "\n"++
    "2\n"++
    "00:02:31,567 --> 00:02:37,164\n"++ 
    "Vi talar inte laengre om Wade. Det aer\n"++ 
    "som om han aldrig hade existerat."

expectedSub1 = Subtitle 1 (Timing 0 2 26 407) (Timing 0 2 31 356) sub1body
sub1body = " X1:100 X2:100 Y1:100 Y2:100\n"++
    "<font color=#00ff00>Detta handlar om min storebrors</font>\n"++
    "<b><i><u>kriminella beteende och foersvinnade.</u></i></b>"

expectedSub2 = Subtitle 2 (Timing 0 2 31 567) (Timing 0 2 37 164) sub2body
sub2body = "\nVi talar inte laengre om Wade. Det aer\n" ++ 
    "som om han aldrig hade existerat."


prop_parse = eq sub1 expectedSub1 && eq sub2 expectedSub2
                where
                    eq s1 s2 = (index s1 == index s2) && (beginning s1 == beginning s2) &&
                        (end s1 == end s2) && (text s1 == text s2)
                    Just [sub1,sub2] = readSubtitles test_subtitles_text

-- Run

options = TestOptions
    { no_of_tests = 200
      , length_of_tests = 1
      , debug_tests = False }

run_tests = do
    runTests "Timing" options
        [ run prop_negative
        , run prop_double_negative
        , run prop_eq
        , run prop_convert1
        , run prop_convert2
        , run prop_convert3 ]

    runTests "Subtitles" options
        [ run prop_ordered
        , run prop_shift
        , run prop_allAfter
        , run prop_allBefore
        ]

    runTests "SRT" options
        [ run prop_parse
        ]
