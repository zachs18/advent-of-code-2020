import System.Environment
import System.IO
import qualified Data.List as List

-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
	case dropWhile p s of
		[] -> []
		s' -> w : splitBy p s''
			where (w, s'') = break p s'

-- Numbers stored in reverse order
iterateGame :: [Int] -> [Int]
iterateGame [] = error "Must be given starting numbers"
iterateGame all@(prev:rest) = case List.elemIndex prev rest of
	Nothing -> 0:all
	Just index -> index+1:all



main :: IO ()
main = do
	args <- getArgs
	let filename = head args
	file <- openFile filename ReadMode

	contents <- hGetContents file
	let input = reverse $ map read $ splitBy (==',') $ head $ lines contents :: [Int]

	let results = foldl (\history _ -> iterateGame history) input [1..2020]

	print $ (reverse results) !! 2019 -- 0-indexed
