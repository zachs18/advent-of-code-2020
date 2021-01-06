import System.Environment
import System.IO
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Text.Read (readMaybe)

-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
    case dropWhile p s of
        [] -> []
        s' -> w : splitBy p s''
            where (w, s'') = break p s'

type Deck = [Int]

playRound :: [Deck] -> [Deck]
playRound decks =
    beforeRests ++ [winnerRest'] ++ afterRests
    where
        topCards = map head decks
        rests = map tail decks
        winningCard = maximum topCards
        Just winnerIndex = List.elemIndex winningCard topCards
        (beforeRests, (winnerRest:afterRests)) = splitAt winnerIndex rests
        winnerRest' = winnerRest ++ [winningCard] ++ filter (/= winningCard) topCards

playGame :: [Deck] -> [Deck]
playGame [] = []
playGame [deck] = [deck]
playGame decks =
    case List.elemIndex [] decks of
        Nothing -> playGame $ playRound decks
        Just loser -> decks

score :: Deck -> Int
score deck = sum $ zipWith (*) [1..] $ reverse deck

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadMode

    hands <- fmap (fmap $ fmap (read :: String -> Int)) $ fmap (splitBy (isNothing . (readMaybe :: String -> Maybe Int))) $ fmap lines $ hGetContents file
    print hands
    let result = playGame hands
    print result
    print $ map score result
    