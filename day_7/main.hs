import System.Environment
import System.IO

type MultiMap a b = [(a, b)]
type Bag = (String, String)

type Rule = MultiMap Bag (Bag, Int)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
	case dropWhile p s of
		[] -> []
		s' -> w : splitBy p s''
			where (w, s'') = break p s'

parseRule :: String -> Rule
parseRule s = parseRuleHelper1 $ splitBy (\c -> (c == ' ' || c == ',' || c == '.')) s

parseRuleHelper1 :: [String] -> Rule
parseRuleHelper1 (adj:color:"bags":"contain":contents) =
	map (\bi -> ((adj,color),bi)) $ parseRuleHelper2 contents

parseRuleHelper2 :: [String] -> [(Bag, Int)]
parseRuleHelper2 (count:adj:color:bag_or_bags:xs) =
	((adj,color),read count):parseRuleHelper2 xs
parseRuleHelper2 ["no","other","bags"] = []
parseRuleHelper2 [] = []

combineRules :: [Rule] -> Rule
combineRules = foldl (++) []

findDirectParents :: Bag -> Rule -> [Bag]
findDirectParents bag rule =
	[pbag | (pbag,_) <- filter (\(b1,(b2,i)) -> b2 == bag) rule]

findAllParents :: Bag -> Rule -> [Bag]
findAllParents bag rule =
	case directParents of
		[] -> []
		--_ -> directParents ++ map (\b -> foldl (++) [] $ findAllParents b rule) directParents
		--_ -> directParents ++ map (\b -> findAllParents b rule) directParents
		_ -> unique $ (directParents ++ (foldl (++) [] $ map (\b -> findAllParents b rule) directParents))
	where directParents = findDirectParents bag rule

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : (unique $ filter (/= x) xs)

combineChildren :: [(Bag, Int)] -> [(Bag, Int)]
combineChildren [] = []
combineChildren ((b,i):xs) = (b, (foldl (+) i $ map snd $ filter (\(b2,i2) -> b2 == b) xs)) : (combineChildren $ filter (\(b2,i2) -> b2 /= b) xs)

findDirectChildren :: (Bag, Int) -> Rule -> [(Bag, Int)]
findDirectChildren (bag,count) rule =
	[(cbag,i*count) | (_,(cbag,i)) <- filter (\(b1,(b2,i)) -> b1 == bag) rule]

findAllChildren :: (Bag, Int) -> Rule -> [(Bag, Int)]
findAllChildren (bag,count) rule =
	case directChildren of
		[] -> []
		_ -> combineChildren $ (directChildren ++ (foldl (++) [] $ map (\bi -> findAllChildren bi rule) directChildren))
	where directChildren = findDirectChildren (bag,count) rule

main = do
	args <- getArgs
	let [fileName,adj,color] = args
	let bag = (adj,color)
	
	file <- openFile fileName ReadMode
	contents <- hGetContents file
	let rulesStrs = lines contents
	let rule = combineRules $ map parseRule rulesStrs
--	print rule
	
--	print $ findDirectParents bag rule
--	print $ findAllParents bag rule
--	putChar '\n'
	print $ length $ findAllParents bag rule
--	print $ findAllChildren (bag, 1) rule
	print $ foldl (+) 0 $ map snd $ findAllChildren (bag, 1) rule

