{-
	2. Find the last but one lement of a list.
-}


{-
	1st implementation: tail-recursive
-}

lastButOne1 :: [a] -> a
lastButOne1 [] = error "empty list => no las but one lement!"
lastButOne1 (_:[]) = error "list with only one element => no last but one lement!"
lastButOne1 (x:_:[]) = x
lastButOne1 (_:xs) = lastButOne1 xs


{-
	2nd implementation: picking the element by its index
-}

lastButOne2 :: [a] -> a
lastButOne2 xs = let l = length xs
                 in case l of
                    0 -> error "empty list => no las but one lement!"
                    1 -> error "list with only one element => no last but one lement!"
	            _ -> xs !! (l - 2)


{-
	3rd implementation
-}

lastButOne3 :: [a] -> a
lastButOne3 = head . tail . reverse


{-
	4th implementation
-}

lastButOne4 :: [a] -> a
lastButOne4 = last . init


{-
	5th implementation
-}

lastButOne5 :: [a] -> a
lastButOne5 = (!! 1) . reverse
