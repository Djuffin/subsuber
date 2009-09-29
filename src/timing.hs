module Timing where


data Timing = Timing {
    hours :: Int,
    minutes :: Int,
    seconds :: Int,
    milliseconds :: Int
    }
		
negative :: Timing -> Timing
negative (Timing hours minutes seconds milliseconds) =
    Timing (-hours) (-minutes) (-seconds) (-milliseconds)


addTimings :: Timing -> Timing -> Timing
addTimings a b = convertToTiming $ convertToMilliseconds a + convertToMilliseconds b

convertToMilliseconds :: Timing -> Int
convertToMilliseconds (Timing hours minutes seconds milliseconds) =
    hours * 3600 * 1000 +
    minutes * 60 * 1000 +
    seconds * 1000 +
    milliseconds

convertToTiming :: Int -> Timing
convertToTiming milliseconds = Timing hours minutes seconds milliseconds'
    where
        hours = milliseconds `div` (3600 * 1000)
        minutes = (milliseconds `div` (60 * 1000)) `mod` 60
        seconds = (milliseconds `div` 1000) `mod` 60
        milliseconds' = milliseconds `mod` 1000


instance Show Timing where
	show (Timing hours minutes seconds milliseconds) =
		(show hours) ++ ":"	++ (show minutes) ++ ":" ++ (show seconds) ++ "," ++ (show milliseconds)
		

instance Eq Timing where
    x == y = (convertToMilliseconds x) == (convertToMilliseconds y)

instance Ord Timing where
    compare x y = compare (convertToMilliseconds x)(convertToMilliseconds y)

