{-
	11.  Modified run-length encoding.

	Modify the result of problem 10 in such a way that if an
	element has no duplicates it is simply copied into the
	result list. Only elements with duplicates are transferred
	as (N E) lists.
-}

module ModifListEncode where

{-
	import first implementation of the problem 10 solution
-}
import ListEncode (encode1)

{-
	As list in haskell are homogeneous, we need to create
	a new type that can handle both "single" and "multiple"
	elements
-}

data EncodedElt a = Single a | Multiple Int a
     deriving Show


{-
	1st implementation: using foldl
-}

modifEncode1 :: Eq a => [a] -> [EncodedElt a]
modifEncode1 = foldl (\acc (elt, n) -> if n == 1 then acc ++ [(Single elt)]
                                                 else acc ++ [(Multiple n elt)]
		     ) [] . encode1


{-
	2nd implementation: using map
-}

modifEncode2 :: Eq a => [a] -> [EncodedElt a]
modifEncode2 = map (\ (elt, n) -> if n == 1 then (Single elt)
                                            else (Multiple n elt)
		   ) . encode1


{-
	3rd implementation: using list comprehension
-}

modifEncode3 :: Eq a => [a] -> [EncodedElt a]
modifEncode3 xs = [if n == 1 then (Single elt) else (Multiple n elt) | (elt, n) <- xsEnc]
                    where xsEnc = encode1 xs
