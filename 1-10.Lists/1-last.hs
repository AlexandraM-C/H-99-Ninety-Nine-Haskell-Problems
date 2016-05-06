{-
	1. Find the last element of a list.
 -}


{-
	1st implementation: tail-recursive
-}

last1 :: [a] -> a
last1 [] = error "empty list => no last element!"
last1 (x:[]) = x
last1 (_:xs) = last1 xs


{-
	2nd implementation: just picking the last element
	by its index
-}

last2 :: [a] -> a
last2 [] = error "empty list => no last element!"
last2 xs = xs !! (length xs - 1)


{-
	3rd implementation
-}

last3 :: [a] -> a
last3 = head . reverse

