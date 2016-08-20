{-
	18. Extract a slice from a list.

	Given two indices, i and k, the slice is the list containing
	the elements between the i'th and k'th element of the original
	list (both limits included). Start counting the elements with 1.
-}

module SublistExtract (extract1, extract2, extract3, extract4, extract5) where


{-
	1st implementation: tail recursive
-}

extract1 :: [a] -> Int -> Int -> [a]
extract1 [] _ _ = []
extract1 (x:xs) 1 1 = [x]
extract1 (x:xs) 1 end | end > 1 = [x] ++ extract1 xs 1 (end - 1)
extract1 (x:xs) start end | end >= start, start > 0 = extract1 xs (start - 1) (end - 1)
extract1 _ _ _ = error "incorrect arguments"


{-
	2nd implementation: stack recursive
-}

extract2 :: [a] -> Int -> Int -> [a]
extract2 = extractAux []


extractAux :: [a] -> [a] -> Int -> Int -> [a]
extractAux sol [] _ _ = sol
extractAux sol (x:xs) 1 1 = sol ++ [x]
extractAux sol (x:xs) 1 end | end > 1 = extractAux (sol ++ [x]) xs 1 (end - 1)
extractAux [] (x:xs) start end | end >= start, start > 0 = extractAux [] xs (start - 1) (end - 1)
extractAux _ _ _ _ = error "incorect arguments"


{-
	3rd implementation: using dropWhile and takeWhile
-}

extract3 :: [a] -> Int -> Int -> [a]
extract3 xs start end | end >= start, start > 0 = let zipped = zip xs [1..]
                                                  in map fst $ takeWhile ((<= end) . snd) $ dropWhile ((< start) . snd) zipped
extract3 _ _ _ = error "incorect arguments"


{-
	4th implementation: using filter
-}

extract4 :: [a] -> Int -> Int -> [a]
extract4 xs start end | end >= start, start > 0 = let zipped = zip xs [1..end]
                                                  in map fst $ filter (\(elt, idx) -> idx >= start) zipped
extract4 _ _ _ = error "incorect arguments"


{-
	5th implementation: using list comprehension
-}

extract5 :: [a] -> Int -> Int -> [a]
extract5 xs start end | end >= start, start > 0 = [elt | (elt, i) <- zip xs [1..end], i >= start]
extract5 _ _ _ = error "incorect arguments"
