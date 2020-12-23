import System.Environment
import System.IO
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.Map as Map

-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
	case dropWhile p s of
		[] -> []
		s' -> w : splitBy p s''
			where (w, s'') = break p s'

data Mask = Mask String
	deriving Show

data Action = ChangeMask Mask | SetMemory Int Int
	deriving Show

data State = State Mask (Map.Map Int Int)
	deriving Show

parseAction :: String -> Action
parseAction ('m':'a':'s':'k':' ':'=':' ':maskStr) = ChangeMask $ Mask $ reverse maskStr
parseAction ('m':'e':'m':'[':restStr) =
	let [indexStr,(' ':'=':' ':valueStr)] = splitBy (==']') restStr in
		SetMemory (read indexStr) (read valueStr)

getAndMask1 :: Mask -> Int
getAndMask1 (Mask []) = -1
getAndMask1 (Mask (m:ms)) = (shiftL (getAndMask1 $ Mask ms) 1) .|. (fromEnum (m /= '0'))

getOrMask1 :: Mask -> Int
getOrMask1 (Mask []) = 0
getOrMask1 (Mask (m:ms)) = (shiftL (getOrMask1 $ Mask ms) 1) .|. (fromEnum (m == '1'))
	
applyMask1 :: Mask -> Int -> Int
applyMask1 ms i = (i .&. (getAndMask1 ms)) .|. getOrMask1 ms

doAction1 :: Action -> State -> State
doAction1 (ChangeMask mask') (State _ memory) = State mask' memory
doAction1 (SetMemory index value) (State mask memory) = State mask $ Map.insert index (applyMask1 mask value) memory

applyMask2 :: Mask -> Int -> [Int]
applyMask2 (Mask []) i = [i]
applyMask2 (Mask (m:ms)) i = case m of
	'0' -> [(i .&. 1) .|. r | r <- rest]
	'1' -> [    1     .|. r | r <- rest]
	'X' -> concat [[0 .|. r, 1 .|. r] | r <- rest]
	where rest = map (\x -> shiftL x 1) (applyMask2 (Mask ms) (shiftR i 1))

doAction2 :: Action -> State -> State
doAction2 (ChangeMask mask') (State _ memory) = State mask' memory
doAction2 (SetMemory index value) (State mask memory) =
	State mask $ foldr (\ind mem -> Map.insert ind value mem) memory (applyMask2 mask index)

main :: IO ()
main = do
	args <- getArgs
	let filename = head args
	file <- openFile filename ReadMode

	contents <- hGetContents file
	let actions = map parseAction $ lines contents

	let memory = Map.fromList []
	let state = State (Mask "") memory

	let state'1 = foldl (\st act -> doAction1 act st) state actions
	let State mask'1 memory'1 = state'1
	print $ Map.foldl (+) 0 memory'1

	let state'2 = foldl (\st act -> doAction2 act st) state actions
	let State mask'2 memory'2 = state'2
	print $ Map.foldl (+) 0 memory'2
