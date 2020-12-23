import System.Environment
import System.IO
import Text.Read (readMaybe)
import Data.Maybe (maybeToList, isJust)

-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
	case dropWhile p s of
		[] -> []
		s' -> w : splitBy p s''
			where (w, s'') = break p s'

parseIDs :: String -> [Int]
parseIDs s = concatMap (maybeToList . readMaybe) (splitBy (==',') s)

findBest :: Int -> [Int] -> (Int, Int)
findBest time ids =
	case filter (\id -> (mod time id == 0)) ids of
		[] -> findBest (time+1) ids
		(x:xs) -> (time, x)

helper1 :: (Integer, Maybe Integer) -> [(Integer, Integer)]
helper1 (i, Just j) = [(i, j)]
helper1 _ = []

parseIDs2 :: String -> [(Integer, Integer)]
parseIDs2 s = concatMap helper1 (zip [0..] $ map (readMaybe) (splitBy (==',') s))

-- TODO: Implement Chinese Remainder Theorem

crtCs :: [Integer] -> [Integer]
crtCs ns = map (\n -> foldl (*) 1 $ filter (/=n) ns) ns

helper2 :: Integer -> [(Integer, Integer)] -> Integer
helper2 i ids =
	if all (\(rem, md) -> mod i md == mod rem md) ids
		then i
		else helper2 (i+1) ids

findT :: [(Integer, Integer)] -> Integer
findT ids = helper2 0 ids

main :: IO ()
main = do
	args <- getArgs
	let filename = head args
	file <- openFile filename ReadMode

	contents <- hGetContents file
	let [earliestStr, idsStr] = lines contents

	let earliest = read earliestStr :: Int
	let ids = parseIDs idsStr

	let (leaveTime, id) = findBest earliest ids
	print ((leaveTime-earliest) * id)

--	let ids2 = parseIDs2 idsStr
--	print ids2
--	print $ findT ids2
