{-
	4. Find the number of elements of a list.
-}

module ListLength where

{-
	1sf implementation: using an auxiliary tail recursive
	function, defined in a where statement
-}

length1 :: [a] -> Int
length1 xs = length_aux xs 0
        where length_aux [] n = n
              length_aux (_:xs) n = length_aux xs (n + 1)


{-
	2nd implementation
-}

length2 :: [a] -> Int
length2 [] = 0
length2 xs = len
        where (e, len) = last $ zip xs [1..]


{-
	3rd implementaition: using foldl
-}

length3 :: [a] -> Int
length3 [] = 0
length3 xs = foldl (\acc e -> acc + 1) 0 xs


{-
	4th implementation: using sum and list comprehension
-}

length4 :: [a] -> Int
length4 [] = 0
length4 xs = sum [1 | x <- xs]
