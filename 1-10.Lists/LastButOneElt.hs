{-
	2. Find the last but one lement of a list.
-}

module LastButOneElt where

{-
	1st implementation: tail-recursive
-}

lastButOne1 :: [a] -> a
lastButOne1 [] = error "empty list => no las but one element!"
lastButOne1 (_:[]) = error "list with only one element => no last but one element!"
lastButOne1 (x:_:[]) = x
lastButOne1 (_:xs) = lastButOne1 xs


{-
	2nd implementation: picking the element by its index
-}

lastButOne2 :: [a] -> a
lastButOne2 xs = let l = length xs
                 in case l of
                    0 -> error "empty list => no las but one element!"
                    1 -> error "list with only one element => no last but one element!"
	            _ -> xs !! (l - 2)


{-
	3rd implementation
-}

lastButOne3 :: [a] -> a
lastButOne3 [] = error "empty list => no las but one element!"
lastButOne3 (_:[]) = error "list with only one element => no last but one element!"
lastButOne3 xs = head $ tail $ reverse xs


{-
	4th implementation
-}

lastButOne4 :: [a] -> a
lastButOne4 [] = error "empty list => no las but one element!"
lastButOne4 (_:[]) = error "list with only one element => no last but one element!"
lastButOne4 xs = last $ init xs


{-
	5th implementation
-}

lastButOne5 :: [a] -> a
lastButOne5 [] = error "empty list => no las but one element!"
lastButOne5 (_:[]) = error "list with only one element => no last but one element!"
lastButOne5 xs = (!! 1) $ reverse xs
