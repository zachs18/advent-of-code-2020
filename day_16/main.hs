import System.Environment
import System.IO
import qualified Data.List as List
import Data.Char
import qualified Data.Map as Map
import Data.Maybe (isJust, maybeToList)

-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
	case dropWhile p s of
		[] -> []
		s' -> w : splitBy p s''
			where (w, s'') = break p s'

type FieldCheck = Int -> Bool

orFuncs :: (a -> Bool) -> (a -> Bool) -> a -> Bool
orFuncs f g x = (f x) || (g x)

helper1 :: [Int] -> FieldCheck
helper1 [s,e] = (\i -> i >= s && i <= e)

makeFieldCheck :: [String] -> FieldCheck
makeFieldCheck wrds =
	let rangesStrs = filter (all $ orFuncs Data.Char.isDigit (=='-')) wrds in
		foldl orFuncs (const False) $ map (helper1 . (map read)) $ map (splitBy (=='-')) rangesStrs


zipLists :: [[a]] -> [[a]]
zipLists [] = []
zipLists (l:ls) =
	case zipLists ls of
		[] -> map (\i -> [i]) l
		zs -> zipWith (:) l zs

setDefault :: a -> Int -> a -> [a] -> [a]
setDefault d 0 v (_:vs) = v:vs
setDefault d 0 v [] = [v]
setDefault d i v [] = replicate (i-1) d ++ [v]
setDefault d i v (vv:vs) = vv : setDefault d (i-1) v vs


helper2 :: Int -> [[Int]] -> [[Int]]
helper2 current possibleFields =
	case filter (\i -> isJust (List.elemIndex current $ possibleFields !! i)) [0..length possibleFields - 1] of
		[] -> (
			case all ((==1) . length) possibleFields of
				True -> possibleFields
				False -> helper2 0 possibleFields
			)
		[i] -> helper3 current $ setDefault [] i [current] $ map (List.\\ [current]) possibleFields
		_ -> helper3 current possibleFields

helper3 :: Int -> [[Int]] -> [[Int]]
helper3 current possibleFields =
	case filter (\i -> [current] == possibleFields !! i) [0..length possibleFields - 1] of
		[i] -> helper2 (current+1) $ setDefault [] i [current] $ map (List.\\ [current]) possibleFields
		_ -> helper2 (current+1) possibleFields

simplifyPossibleFields :: [[Int]] -> [[Int]]
simplifyPossibleFields possibleFields = helper2 0 possibleFields
	

main :: IO ()
main = do
	args <- getArgs
	let filename = head args
	file <- openFile filename ReadMode

	contents <- hGetContents file
	let [fieldsStrs,_:myticketStrs,_:nearbyStrs] = splitBy (=="") $ lines contents

	let fcs = map (makeFieldCheck . words) fieldsStrs
	let fieldNames = map (head . (splitBy (==':'))) fieldsStrs
--	print fieldNames
	let anyField = foldl orFuncs (const False) fcs

	let nearbys = map (\s -> map read $ splitBy (==',') s) nearbyStrs :: [[Int]]

	let nearbyFields = concat nearbys

	let invalidValues = filter (not . anyField) nearbyFields

	print $ sum invalidValues


	let nearbys' = filter (all anyField) nearbys

	let fields = zipLists nearbys'

	let fieldIndices = [0..(length fields-1)]

	let possibleFields = map (\values -> filter (\i -> (all (fcs !! i)) values) fieldIndices) fields
	let solution = concat $ helper2 0 possibleFields
	-- corresponding location in this list contains the index of the fieldName for that index
	-- e.g. [3,0,2,1] means that field names of "a","b","c","d" and a ticket of 4,5,6,7, d=4, a=5, c=6, b=7

	let departureFields = concatMap (maybeToList . \e -> List.elemIndex e solution) $ filter (\i -> List.isPrefixOf "departure" (fieldNames !! i)) [0..length fieldNames-1]
	
	let myTicket = map read $ splitBy (==',') $ head myticketStrs :: [Int]

	print $ foldl (*) 1 $ map (myTicket !!) departureFields

