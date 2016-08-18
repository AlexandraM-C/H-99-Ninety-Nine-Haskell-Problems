{-
	3. Find the K'th element of a list. The first element in the list is number 1.
-}

module EltAt where

{-
	1st implementation: tail recursive
-}

eltAt1 :: [a] -> Int -> a
eltAt1 [] _ = error "index out of bounds"
eltAt1 (x:xs) n
    | n < 1 = error "index out of bounds"
    | n == 1 = x
    |otherwise = eltAt1 xs (n - 1)


{-
	2nd implementation: simply using the !! operator
-}

eltAt2 :: [a] -> Int -> a
eltAt2 [] _ = error "index out of bounds"
eltAt2 xs n = if n < 1 then error "index out of bounds"
                       else xs !! (n - 1)


{-
	3rd implementation: some too complicated
	thing that was fun to think of :)
-}

eltAt3 :: [a] -> Int -> a
eltAt3 [] _ = error "index out of bounds"
eltAt3 xs n = if n < 1 then error "index out of bounds"
                       else fst . head . filter (\(e, idx) -> idx == n) $ zipWith (,) xs [1..]
