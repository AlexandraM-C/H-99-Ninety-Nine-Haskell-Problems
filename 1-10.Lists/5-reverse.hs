{-
	5. Reverse a list.
-}


{-
	1sf implementation: using an auxiliary tail recursive
	function defined in a where statement
-}

reverse1 :: [a] -> [a]
reverse1 xs = reverse_aux xs []
         where reverse_aux [] s = s
	       reverse_aux (y:ys) s = reverse_aux ys (y:s)


{-
	2nd implementation: using foldl
-}

reverse2 :: [a] -> [a]
reverse2 = foldl (\acc e -> e:acc) []


{-
	3rd implementation: stack recursive
-}

reverse3 :: [a] -> [a]
reverse3 [] = []
reverse3 (x:xs) = reverse3 xs ++ [x]
