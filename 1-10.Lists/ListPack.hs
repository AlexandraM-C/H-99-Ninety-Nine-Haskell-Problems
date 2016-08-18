{-
	9. Pack consecutive duplicates of list elements into sublists. If a list
	contains repeated elements they should be placed in separate sublists.
-}

module ListPack where

{-
	1sf implementation: stack recursive - at every call, it forms
	the pack from the head of the list, then calls itself recursively
	on the rest of the elements
-}

pack1 :: Eq a => [a] -> [[a]]
pack1 [] = []
pack1 (x:xs) = [x : takeWhile (== x) xs] ++ pack1 (dropWhile (== x) xs)


{-
	2nd implementation: the same idea as the one above, but using span
	to form the "packs", instead of takeWhile and dropWhile
-}

pack2 :: Eq a => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = let (elm, rest) = span (== x) xs
               in [x : elm] ++ pack2 rest
