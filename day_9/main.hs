import System.Environment
import System.IO
import Data.List

headN :: Int -> [a] -> [a]
headN = curry (fst . uncurry splitAt)

tailN :: Int -> [a] -> [a]
tailN = curry (snd . uncurry splitAt)

sums :: (Eq a) => (a -> a -> a) -> [a] -> [a]
sums f [] = []
sums f (a:as) = (map (f a) $ filter (/=a) as) ++ sums f as

firstInvalidXMASSequence :: Eq a => Int -> (a -> a -> a) -> [a] -> Maybe [a]
firstInvalidXMASSequence preambleLength f xs =
	case preambleLength >= length xs of
		True -> Nothing
		False -> case any (== (xs !! preambleLength)) $ sums f $ headN preambleLength xs of
			False -> Just xs
			True -> firstInvalidXMASSequence preambleLength f $ tail xs



main :: IO ()
main = do
	args <- getArgs
	if 2 /= length args
		then putStrLn "Usage: filename preambleLength"
		else do
			let filename = head args
			file <- openFile filename ReadMode
			contents <- hGetContents file

			let preambleLength = read (args !! 1) :: Int

			let numbers = map (read :: String -> Int) $ lines contents
			case firstInvalidXMASSequence preambleLength (+) numbers of
				Nothing -> putStrLn "Part 1: Not found"
				Just numbers' -> do
					let missingNumber = numbers' !! preambleLength 
					putStr "Part 1: "
					print missingNumber
					
					let possibleInits = tail $ tail $ Data.List.inits numbers
					let possibleSubseqs = concatMap (init . init . Data.List.tails) possibleInits
					let (validPossibleSubseq:_) = filter (\s -> missingNumber == foldl (+) 0 s) possibleSubseqs
					putStr "Part 2: "
					print $ (head validPossibleSubseq) + (last validPossibleSubseq)
