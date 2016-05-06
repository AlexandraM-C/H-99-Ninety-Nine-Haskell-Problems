{-
	7. Flatten a nested list structure.
-}


{-
	Defining the data type, for in Haskell all elements
	of list must have the same type.
-}

data NestedList a = Elem a | List [NestedList a]


{-
	1st implementation: using foldl and some stack recursion
-}

flatten1 :: NestedList a -> [a]
flatten1 (Elem x) = [x]
flatten1 (List xs) = foldl (\acc e -> acc ++ flatten1 e) [] xs


{-
	2nd implementation: basically the same thing as the one above,
	but using concatMap instead of foldl with a lambda function
-}

flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List xs) = concatMap flatten2 xs

