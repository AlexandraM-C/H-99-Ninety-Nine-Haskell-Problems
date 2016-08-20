{-
	12. Decode a run-length encoded list.

	Given a run-length code list generated as specified in
	problem 11. Construct its uncompressed version.
-}

module ModifListDecode where

{-
	import the EndodedElt data type defined in the implementation
	of problem 11 solution
-}
import ModifListEncode (EncodedElt(Single, Multiple))


{-
	First, define a helping function that decodes one  element
-}

decodeElt :: EncodedElt a -> [a]
decodeElt (Single x) = [x]
decodeElt (Multiple n x) = [x | _ <- [1..n]]


{-
	1st implementation: using foldl
-}

modifDecode1 :: Eq a => [EncodedElt a] -> [a]
modifDecode1 = foldl (\acc x -> acc ++ (decodeElt x)) []


{-
	2nd inmplementation: using concatMap
-}

modifDecode2 :: Eq a => [EncodedElt a] -> [a]
modifDecode2 = concatMap decodeElt


{-
	3rd implementation: using list comprehension
-}

modifDecode3 :: Eq a => [EncodedElt a] -> [a]
modifDecode3 xs = concat [decodeElt e | e <- xs]
