{-
	14. Duplicate the elements of a list.
-}

module DuplicateElts where


{-
	1st implementation: tail recursive
-}

dupli1 :: [a] -> [a]
dupli1 [] = []
dupli1 (x:xs) = [x, x] ++ dupli1 xs


{-
	2nd implementation: stack recursive
-}

dupli2 :: [a] -> [a]
dupli2 = dupliAux []

dupliAux :: [a] -> [a] -> [a]
dupliAux sol [] = sol
dupliAux sol (x:xs) = dupliAux (sol ++ [x, x]) xs


{-
	3rd implementation: using foldl
-}

dupli3 :: [a] -> [a]
dupli3 = foldl (\acc elt -> acc ++ [elt, elt]) []


{-
	4th implementation: using concatMap
-}

dupli4 :: [a] -> [a]
dupli4 = concatMap (\elt -> [elt, elt])
