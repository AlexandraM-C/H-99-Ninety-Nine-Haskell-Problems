{-
	3. Find the K'th element of a list. The first element in the list is number 1.
-}


{-
	1st implementation: tail recursive
-}

elemAt1 :: [a] -> Int -> a
elemAt1 [] _ = error "index out of bounds"
elemAt1 (x:xs) n
    | n < 1 = error "index out of bounds"
    | n == 1 = x
    |otherwise = elemAt1 xs (n - 1)


{-
	2nd implementation: simply using the !! operator
-}

elemAt2 :: [a] -> Int -> a
elemAt2 xs n = xs !! (n - 1)


{-
	3rd implementation: some too complicated
	thing that was fun to think of :)
-}

elemAt3 :: [a] -> Int -> a
elemAt3 xs n = fst . head . filter (\(e, idx) -> idx == n) $ zipWith (,) xs [1..]
