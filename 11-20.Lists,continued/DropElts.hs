{-
	16. Drop every N'th element from a list.
-}

module DropElts (drop1, drop2) where


{-
	1st implementation: stack recursive
-}

drop1 :: [a] -> Int -> [a]
drop1 = dropAux [] 1


dropAux :: [a] -> Int -> [a] -> Int -> [a]
dropAux sol _ [] _ = sol
dropAux sol idx (x:xs) n = if idx `mod` n == 0 then dropAux sol (idx + 1) xs n
                                               else dropAux (sol ++ [x]) (idx + 1) xs n


{-
	2nd implementation
-}

drop2 :: [a] -> Int -> [a]
drop2 xs n = map fst $ filter (\(elt, idx) -> idx `mod` n /= 0) $ zip xs [1..]


{-
	3rd implementation: a prettier version of the one above
-}

drop3 :: [a] -> Int -> [a]
drop3 xs n = map fst $ filter ((/= n) . snd) $ zip xs $ cycle [1..n]


{-
	4th implementation: using list comprehension
-}

drop4 :: [a] -> Int -> [a]
drop4 xs n = [elt | (elt, idx) <- (zip xs [1..]), idx `mod` n /= 0]
