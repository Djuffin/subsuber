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
