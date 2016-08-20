{-
	19. Rotate a list N places to the left.
-}

module ListRotate where

{-
	1st implementation: using drop and take to split the list
-}

rotate1 :: [a] -> Int -> [a]
rotate1 xs n = let len = length xs
                   new_n = if n > 0 then n `mod` len
	                            else len - (abs n) `mod` len
	           start = drop new_n xs
	           end = take new_n xs
	        in start ++ end


{-
	2nd implementation: using drop, take and cycle
-}

rotate2 :: [a] -> Int -> [a]
rotate2 xs n = take len $ drop new_n $ cycle xs
                 where len = length xs
		       new_n = if n > 0 then n `mod` len
		                        else len - (abs n) `mod` len


{-
	3rd implementation: stack recursive
-}

rotate3 :: [a] -> Int -> [a]
rotate3 [] _ = []
rotate3 xs 0 = xs
rotate3 l@(x:xs) n | n > 0 = rotate3 (xs ++ [x]) (n - 1)
                   | otherwise = rotate3 (last l : init l) (n + 1)
