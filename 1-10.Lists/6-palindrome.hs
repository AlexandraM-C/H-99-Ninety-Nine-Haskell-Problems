{-
	6. Find out whether a list is a palindrome. A palindrome can be read
	forward or backward; e.g. (x a m a x).
-}


{-
	1st implementation: tail recursive
-}

palindrome1 :: Eq a => [a] -> Bool
palindrome1 [] = True
palindrome1 [_] = True
palindrome1 (x:xs) = if (x == last xs)
                        then palindrome1 $ init xs
			else False


{-
	2nd implementation
-}

palindrome2 :: Eq a => [a] -> Bool
palindrome2 xs = (== xs) $ reverse xs


{-
	3rd implementation: basically the same thing as the one above,
	but instead of checking the equality between the list and its
	reverse, checks only the equality between the first half of the
	list and the reverse of the other half
-}

palindrome3 :: Eq a => [a] -> Bool
palindrome3 xs = let len = length xs
                     half1 = take (len `div` 2) xs
		     half2 = if (len `mod` 2 == 0)
		                then drop (len `div` 2) xs
				else drop (len `div` 2 + 1) xs
		 in (== half1) $ reverse half2
