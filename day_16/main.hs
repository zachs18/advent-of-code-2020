import System.Environment
import System.IO
import qualified Data.List as List
import Data.Char
import qualified Data.Map as Map

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

helper :: [Int] -> FieldCheck
helper [s,e] = (\i -> i >= s && i <= e)

makeFieldCheck :: [String] -> FieldCheck
makeFieldCheck wrds =
	let rangesStrs = filter (all $ orFuncs Data.Char.isDigit (=='-')) wrds in
		foldl orFuncs (const False) $ map (helper . (map read)) $ map (splitBy (=='-')) rangesStrs




main :: IO ()
main = do
	args <- getArgs
	let filename = head args
	file <- openFile filename ReadMode

	contents <- hGetContents file
	let [fieldsStrs,_:myticketStrs,_:nearbyStrs] = splitBy (=="") $ lines contents
--	print fieldsStrs
--	print myticketStrs
--	print nearbyStrs

	let fcs = map (makeFieldCheck . words) fieldsStrs
	let anyField = foldl orFuncs (const False) fcs

	let nearbys = map (\s -> map read $ splitBy (==',') s) nearbyStrs :: [[Int]]

	let nearbyFields = concat nearbys

	let invalidValues = filter (not . anyField) nearbyFields

	print $ sum invalidValues


--	let nearbys' = filter (all anyField) nearbys
