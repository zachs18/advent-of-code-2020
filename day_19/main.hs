import System.Environment
import System.IO
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe (isJust, catMaybes)
import Text.Read (readMaybe)

data Rule = Terminal String | SubRules [[Int]]
    deriving Show
type Rules = M.Map Int Rule

-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
    case dropWhile p s of
        [] -> []
        s' -> w : splitBy p s''
            where (w, s'') = break p s'

parseRule :: String -> (Int, Rule)
parseRule s =
    case readMaybe $ head $ words right of
        Just terminal -> (read left, Terminal terminal)
        Nothing -> (read left, SubRules (map (map read . words) $ splitBy (=='|') right))
    where [left, right] = splitBy (==':') s

helper :: Rules -> String -> [Int] -> [String]
helper rules input [] = [input]
helper rules input (rule:rs) =
    concatMap (\input' -> helper rules input' rs) $ matchHead rules input rule

matchHead :: Rules -> String -> Int -> [String]
matchHead rules input rule =
    case rules M.! rule of
        Terminal terminal ->
            if List.isPrefixOf terminal input
                then [snd $ splitAt (length terminal) input]
                else []
        SubRules subrules ->
            concatMap (helper rules input) subrules

matchFull :: Rules -> String -> Int -> Bool
matchFull rules input rule =
    "" `elem` matchHead rules input rule

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadMode

    [rulesStrs, input] <- fmap (splitBy (=="")) $ fmap lines $ hGetContents file

    let rules = M.fromList $ map parseRule rulesStrs
    let (matches, nonMatches) = List.partition (\s -> matchFull rules s 0) input
    print $ length matches
    
    let rules' = M.insert 8 (SubRules [[42],[42,8]]) $ M.insert 11 (SubRules [[42,31],[42,11,31]]) rules
    let (matches', nonMatches') = List.partition (\s -> matchFull rules' s 0) input
    print $ length matches'
