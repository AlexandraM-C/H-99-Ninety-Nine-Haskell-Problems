{-
	15. Replicate the elements of a list a given number of times.
-}

module ReplicateElts (repli1, repli2, repli3, repli4) where


{-
	1st implementation: tail recursive
-}

repli1 :: [a] -> Int -> [a]
repli1 [] _ = []
repli1 (x:xs) n = [x | _ <- [1..n]] ++ repli1 xs n


{-
	2nd implementation: stack recursive
-}

repli2 :: [a] -> Int -> [a]
repli2 = repliAux []


repliAux :: [a] -> [a] -> Int -> [a]
repliAux sol [] _ = sol
repliAux sol (x:xs) n = repliAux (sol ++ [x | _ <- [1..n]]) xs n


{-
	3rd implementation: using foldl
-}

repli3 :: [a] -> Int -> [a]
repli3 xs n = foldl (\acc x -> acc ++ (take n $ repeat x)) [] xs


{-
	4th implementation: using list comprehension
-}

repli4 :: [a] -> Int -> [a]
repli4 xs n = concat [[x | _ <- [1..n]] | x <- xs]
