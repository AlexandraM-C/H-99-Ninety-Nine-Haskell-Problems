{-
	8. Eliminate consecutive duplicates of list elements.
-}

module RemoveDups where

{-
	1st implementation: stack recursive
-}

compress1 :: Eq a => [a] -> [a]
compress1 [] = []
compress1 (x:xs) = x : compress1 (dropWhile (== x) xs)


{-
	2nd implementation: tail recursive
-}

compress2 :: Eq a => [a] -> [a]
compress2 [] = []
compress2 (x:xs) = reverse $ compress_aux xs [x]
          where compress_aux [] s = s
	        compress_aux (x:xs) s = if x == head s
		                         then compress_aux xs s
					 else compress_aux xs (x:s)


{-
	3rd implementation: using foldl
-}

compress3 :: Eq a => [a] -> [a]
compress3 [] = []
compress3 (x:xs) = reverse $ foldl (\acc e -> if e == head acc
                                         then acc
				         else e:acc
			           ) [x] xs


{-
	4th implementation: another stack recursive version
-}

compress4 :: Eq a => [a] -> [a]
compress4 [] = []
compress4 (x:[]) = [x]
compress4 (x:ys@(y:_)) = if x == y
                            then compress4 ys
			    else x : compress4 ys
