pass_to_number :: String -> Int
pass_to_number s = pass_to_number_helper $ reverse s

pass_to_number_helper :: String -> Int
pass_to_number_helper ('R':rest) = (2 * (pass_to_number_helper rest)) + 1
pass_to_number_helper ('B':rest) = (2 * (pass_to_number_helper rest)) + 1
pass_to_number_helper ('L':rest) = 2 * (pass_to_number_helper rest)
pass_to_number_helper ('F':rest) = 2 * (pass_to_number_helper rest)
pass_to_number_helper (c:rest) = error $ c : " is not valid"
pass_to_number_helper ([]) = 0

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort (filter (< p) xs)) ++ [p] ++ (quicksort (filter (>= p) xs))

missing_value :: [Int] -> Maybe Int
missing_value ([]) = Nothing
missing_value ([x]) = Just $ x + 1
missing_value (x:y:rest) = if ((x+1) == y) then (missing_value (y:rest)) else Just (x + 1)


main = do
    input <- getContents
    let passes = quicksort ((fmap pass_to_number) (lines input))
    print "Part 1: "
    print (last passes)
    print "Part 2: "
    print (missing_value passes)
