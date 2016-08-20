{-
	13.  Run-length encoding of a list (direct solution).

	Implement the so-called run-length encoding data compression
	method directly. I.e. don't explicitly create the sublists
	containing the duplicates, as in problem 9, but only count them.
	As in problem P11, simplify the result list by replacing the
	singleton lists (1 X) by X.
-}

module DirectListEncode (directEncode1) where


data EncodedElt a = Single a | Multiple Int a
	deriving Show


{-
	1st implementation
-}

directEncode1 :: Eq a => [a] -> [EncodedElt a]
directEncode1 [] = []
directEncode1 (x:xs) = let n = countAppearances x xs
		       in if n == 0 then (Single x) : directEncode1 xs
		                    else (Multiple (n + 1) x) : directEncode1 (drop n xs)


countAppearances :: Eq a => a -> [a] -> Int
countAppearances x xs = (length xs) - (length $ dropWhile (== x) xs)


{-
	2nd implementation: using foldl
-}

directEncode2 :: Eq a => [a] -> [EncodedElt a]
directEncode2 [] = []
directEncode2 xs = foldl (\acc elt -> case last acc of (Single x) -> if elt == x
                                                                        then (init acc) ++ [(Multiple 2 elt)]
				                                        else acc ++ [(Single elt)]
				             	       (Multiple n x) -> if elt == x
						                            then (init acc) ++ [(Multiple (n + 1) elt)]
							                    else acc ++ [(Single elt)]
		         ) [(Single (head xs))] $ tail xs
