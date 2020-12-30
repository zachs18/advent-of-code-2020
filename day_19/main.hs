import System.Environment
import System.IO
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe (isJust)
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

helper :: Rules -> String -> [Int] -> Maybe String
helper rules input [] = Just input
helper rules input (rule:rs) =
    case matchHead rules input rule of
        Just input' -> helper rules input' rs
        Nothing -> Nothing

matchHead :: Rules -> String -> Int -> Maybe String
matchHead rules input rule =
    case rules M.! rule of
        Terminal terminal ->
            if List.isPrefixOf terminal input
                then Just $ snd $ splitAt (length terminal) input
                else Nothing
        SubRules subrules ->
            case dropWhile (not . isJust) $ map (helper rules input) subrules of
                (Just input':_) -> Just input'
                _ -> Nothing

matchFull :: Rules -> String -> Int -> Bool
matchFull rules input rule =
    case matchHead rules input rule of
        Just "" -> True
        _ -> False

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadMode

    [rulesStrs, input] <- fmap (splitBy (=="")) $ fmap lines $ hGetContents file

    let rules = M.fromList $ map parseRule rulesStrs
    print $ length $ filter id $ map (\s -> matchFull rules s 0) input
