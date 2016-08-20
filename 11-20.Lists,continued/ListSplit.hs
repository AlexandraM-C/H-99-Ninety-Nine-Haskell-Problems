{-
	17.  Split a list into two parts; the length of the first
	part is given.
-}

module ListSplit (split1, split2, split3, split4) where


{-
	1st implementation: tail recursive
-}

split1 :: [a] -> Int -> ([a], [a])
split1 (x:xs) n | n > 0 = (x : first, second)
                             where (first, second) = split1 xs (n - 1)
split1 xs _ = ([], xs)


{-
	2nd implementation: stack recursive
-}

split2 :: [a] -> Int -> ([a], [a])
split2 xs n = splitAux ([], xs) n


splitAux :: ([a], [a]) -> Int -> ([a], [a])
splitAux (first, (s:second)) n | n > 0 = splitAux (first ++ [s], second) (n - 1)
splitAux (first, second) _ = (first, second)


{-
	3rd implementation: using takeWhile and dropWhile
-}

split3 :: [a] -> Int -> ([a], [a])
split3 xs n = let zipped = zip xs [1..]
                  first = map fst $ takeWhile ((<= n) . snd) zipped
		  second = map fst $ dropWhile ((<= n) . snd) zipped
	      in (first, second)


{-
	4th implementation: using foldl
-}

split4 :: [a] -> Int -> ([a], [a])
split4 xs n = let rez = foldl (\((first, second), idx) elt -> if idx < n then ((first ++ [elt], second), (idx + 1))
                                                                         else ((first, second ++ [elt]), (idx + 1))
			      ) (([], []), 0) xs
	      in fst rez
