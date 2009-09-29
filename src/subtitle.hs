module Subtitle where

import Data.List

import Timing

		
data Subtitle = Subtitle {
		index :: Int,
		beginning :: Timing,
		end :: Timing,
		text :: String		
	}



shiftSubtitle :: Subtitle -> Timing -> Subtitle
shiftSubtitle s t = Subtitle
                    (index s)
                    (addTimings (beginning s) t)
                    (addTimings (end s) t)
                    (text s)

splitByTiming :: Timing -> [Subtitle] -> ([Subtitle], [Subtitle])
splitByTiming t subs = partition ((>) t . beginning) subs

allBefore :: Timing -> [Subtitle] -> [Subtitle]
allBefore t = reIndex . fst . (splitByTiming t)

allAfter :: Timing -> [Subtitle] -> [Subtitle]
allAfter t = reIndex . snd . (splitByTiming t)


reIndex :: [Subtitle] -> [Subtitle]
reIndex subs = zipWith combine [1..] subs
                where combine n sub = Subtitle n (beginning sub) (end sub) (text sub)


instance Show Subtitle where
	show (Subtitle index beginning end text) = 
		(show index) ++ "\n" ++ (show beginning) ++ " --> " ++ (show end) ++ text ++ "\n\n"

instance Eq Subtitle where
    x == y = (beginning x) == (beginning y)

instance Ord Subtitle where
    compare x y = compare (beginning x)(beginning y)
