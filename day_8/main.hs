{-# LANGUAGE GADTs #-}

import Text.Read (readMaybe)
import Data.Maybe (maybeToList)

data Instruction = Nop Int | Acc Int | Jmp Int
	deriving Show

data ComputerState = Execute | Halt
	deriving Show

data Computer where
	Computer ::
		{ instructions :: [Instruction]
		, accumulator :: Int
		, index :: Int
		, state :: ComputerState
		} -> Computer
	deriving Show

getDefault :: a -> Int -> [a] -> a
getDefault def index memory = 
	case index < length memory of
		True -> memory !! index
		False -> def

getz :: Num a => Int -> [a] -> a
getz = getDefault 0

gete :: Int -> [a] -> a
gete = getDefault (error "out of bounds array access")

setDefault :: a -> Int -> a -> [a] -> [a]
setDefault def index value memory = 
	case index < length memory of
		True -> front ++ [value] ++ tail back
			where (front, back) = splitAt index memory
		False -> memory ++ (fst $ splitAt (index - length memory) $ repeat def) ++ [value]

setz :: Num a => Int -> a -> [a] -> [a]
setz = setDefault 0

sete :: Int -> a -> [a] -> [a]
sete = setDefault (error "out of bounds array access")

readInt :: String -> Int
readInt ('+':num) = readInt num
readInt num = read num

range :: Int -> [Int]
range 0 = []
range i = case i > 0 of
	True -> (range (i-1)) ++ [i-1]
	False -> error "range of negative"

parseInstructions :: [String] -> [Instruction]
parseInstructions ("nop":arg:xs) = (Nop $ readInt arg) : parseInstructions xs
parseInstructions ("acc":arg:xs) = (Acc $ readInt arg) : parseInstructions xs
parseInstructions ("jmp":arg:xs) = (Jmp $ readInt arg) : parseInstructions xs
parseInstructions [] = []

step :: Computer -> Computer
step computer@Computer{instructions=instructions, accumulator=accumulator, index=index} =
	case index < length instructions of
		False -> computer{state=Halt}
		True -> case instructions !! index of
			Nop _ -> computer{index=index+1,state=Execute}
			Acc arg -> computer{accumulator=accumulator+arg,index=index+1,state=Execute}
			Jmp arg -> computer{index=index+arg,state=Execute}

findDuplicated :: Computer -> [Int] -> (Computer, [Int])
findDuplicated computer@Computer{index=index,state=state} is =
	case (state, filter (== index) is) of
		(Execute, []) -> findDuplicated (step computer) (index:is)
		_ -> (computer, (index:is))

swapInstruction :: Instruction -> Maybe Instruction
swapInstruction (Nop x) = Just $ Jmp x
swapInstruction (Jmp x) = Just $ Nop x
swapInstruction _ = Nothing

swapInstructionAt :: Int -> Computer -> Maybe Computer
swapInstructionAt index computer =
	case swapInstruction $ gete index (instructions computer) of
		Just inst -> Just $ computer{instructions=sete index inst (instructions computer)}
		Nothing -> Nothing

findCorruption :: Computer -> [(Computer, Int)]
findCorruption computer = do
	index <- range $ length $ instructions computer
	computer' <- maybeToList $ swapInstructionAt index computer
	case findDuplicated computer' [] of
		(computer''@Computer{state=Halt},_) -> [(computer'',index)]
		_ -> []

-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
	case dropWhile p s of
		[] -> []
		s' -> w : splitBy p s''
			where (w, s'') = break p s'
main :: IO ()
main = do
	rawInput <- getContents
	let input = splitBy (\c -> (c == ' ') || (c == '\n')) rawInput
	let instructions = parseInstructions input
--	print instructions
	let computer = Computer{instructions=instructions,accumulator=0,index=0,state=Execute}
	let (computer',indices) = findDuplicated computer []
	putStrLn "Part 1:"
	print $ accumulator computer'
	let [corruption@(computer'',_)] = findCorruption computer
	putStrLn "Part 2:"
	print $ accumulator computer''
