module Subtitle where

import Data.List

import Timing

		
data Subtitle = Subtitle {
		index :: Int,
		beginning :: Timing,
		end :: Timing,
		text :: String		
	}

type Subtitles = [Subtitle]

shiftSubtitle :: Subtitle -> Timing -> Subtitle
shiftSubtitle s t = Subtitle
                    (index s)
                    (addTimings (beginning s) t)
                    (addTimings (end s) t)
                    (text s)

splitByTiming :: Timing -> Subtitles -> (Subtitles, Subtitles)
splitByTiming t subs = partition ((>) t . beginning) subs

allBefore :: Timing -> Subtitles -> Subtitles
allBefore t = reIndex . fst . (splitByTiming t)

allAfter :: Timing -> Subtitles -> Subtitles
allAfter t = reIndex . snd . (splitByTiming t)


reIndex :: Subtitles -> Subtitles
reIndex subs = zipWith combine [1..] subs
                where combine n sub = Subtitle n (beginning sub) (end sub) (text sub)

order :: Subtitles -> Subtitles
order = reIndex . sort

append :: Subtitles -> Timing -> Subtitles -> Subtitles
append subs1 gap subs2 = order (subs1 ++ map (flip shiftSubtitle gap) subs2)



instance Show Subtitle where
	show (Subtitle index beginning end text) = 
		(show index) ++ "\n" ++ (show beginning) ++ " --> " ++ (show end) ++ text ++ "\n\n"

instance Eq Subtitle where
    x == y = (beginning x) == (beginning y)

instance Ord Subtitle where
    compare x y = compare (beginning x)(beginning y)
