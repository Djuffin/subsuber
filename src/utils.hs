module Utils where

split :: Char -> String -> [String]
split _ [] = []
split c s = split' "" s
	where 
		split' :: String -> String -> [String]
		split' head [] = if head /= ""  then [head] else []
		split' head (x:xs)  | x == c = if head /= "" 
									then head : split' "" xs
									else split' "" xs
							| otherwise = split' (head ++ [x]) xs

