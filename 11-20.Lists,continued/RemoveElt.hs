{-
	20. Remove the K'th element from a list.
-}

module RemoveElt where


{-
	1st implementation
-}

removeAt1 :: Int -> [a] -> (a, [a])
removeAt1 n xs | n > 0 && n <= length xs = (,) (xs !! (n - 1)) (take (n - 1) xs ++ drop n xs)
               | otherwise = error "invalid index"


{-
	2nd implementation
-}

removeAt2 :: Int -> [a] -> (a, [a])
removeAt2 1 (x:xs) = (x, xs)
removeAt2 n (x:xs) | n > 0 && n <= length xs + 1 = let (elt, rest) = removeAt2 (n - 1) xs
					       in  (elt, x:rest)
	           | otherwise = error "invalid index"


{-
	3rd implementation: using splitAt
-}

removeAt3 :: Int -> [a] -> (a, [a])
removeAt3 n xs | n > 0 && n <= length xs = (\(start, end) -> (,) (last start) (init start ++ end)) $ splitAt n xs
               | otherwise = error "invalid index"
