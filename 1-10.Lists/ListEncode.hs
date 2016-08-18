{-
	10. Run-length encoding of a list. Use the result of problem P09
	to implement the so-called run-length encoding data compression
	method. Consecutive duplicates of elements are encoded as lists
	(N E) where N is the number of duplicates of the element E.
-}

module ListEncode where

import ListPack


{-
	1st implementation: using foldl
-}

encode1 :: Eq a => [a] -> [(a, Int)]
encode1 [] = []
encode1 xs = foldl (\acc l -> acc ++ [(,) (head l) (length l)]) [] $ ListPack.pack1 xs


{-
	2nd implementation: using map
-}

encode2 :: Eq a => [a] -> [(a, Int)]
encode2 [] = []
encode2 xs = map (\l -> (,) (head l) (length l)) $ ListPack.pack1 xs
