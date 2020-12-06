
type Group = [[Char]]

getGroups :: [[Char]] -> [Group]
getGroups [] = []
getGroups xs = g : getGroups rest
	where (g, rest) = getGroupsHelper xs

getGroupsHelper :: [[Char]] -> (Group, [[Char]])
getGroupsHelper [] = ([], [])
getGroupsHelper ("":xs) = ([], xs)
getGroupsHelper (line:xs) = (line : g, rest)
	where (g, rest) = getGroupsHelper xs

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) y = if (x == y) then True else contains xs y

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = if contains xs x then unique xs else x : unique xs

groupOr :: Group -> String
groupOr [] = ""
groupOr [x] = x
groupOr (x:xs) = unique (x ++ groupOr xs)

groupAnd :: Group -> String
groupAnd [] = ""
groupAnd [x] = x
groupAnd (x:xs) = filter (contains x) (groupAnd xs)

main = do
	input <- getContents
	let groups = getGroups $ lines input
	print $ sum $ fmap (length . groupOr) groups
	print $ sum $ fmap (length . groupAnd) groups
