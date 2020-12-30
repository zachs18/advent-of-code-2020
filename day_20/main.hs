import System.Environment
import System.IO
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe (isJust, catMaybes)
import Text.Read (readMaybe)

data Tile = Tile [String]
    deriving Show
type Tiles = M.Map Int Tile

-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
    case dropWhile p s of
        [] -> []
        s' -> w : splitBy p s''
            where (w, s'') = break p s'

parseTile :: [String] -> (Int, Tile)
parseTile ss = (f $ head ss, g $ tail ss)
    where
        f s = read $ head $ tail $ splitBy (\c -> c == ' ' || c == ':') s
        g ss = Tile ss

borders :: Tile -> [String]
borders (Tile ss) = concatMap (\t -> [t,reverse t])
    [head ss
    ,last ss
    ,head tss
    ,last tss
    ] where
        tss = List.transpose ss

orientations :: Tile -> [Tile]
orientations (Tile ss) = fmap Tile
    [ss
    ,ty ss
    ,tx ss
    ,tac ss
    ,ty . tx $ ss -- R2
    ,tac . tx $ ss -- R
    ,tac . ty $ ss -- R3
    ,tac . tx . ty $ ss -- TBD
    ]
    where
        ty = reverse
        tx = fmap reverse
        tac = List.transpose

commonBorders :: [Tile] -> M.Map String [Tile]
commonBorders ts =
    foldl (M.unionWith (++)) (M.fromList []) $ fmap (\t -> M.fromList $ fmap (\b -> (b, [t])) $ borders t) ts

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadMode

    tiles <- fmap (fmap parseTile) $ fmap (splitBy (=="")) $ fmap lines $ hGetContents file
    print tiles
    
    let commonBorders' = commonBorders $ fmap snd tiles
    --print $ commonBorders'
    --print $ M.filter (==1) $ fmap length commonBorders'
    
    let corners = filter (\(_,t) -> 4 == (length $ filter (\b -> length (commonBorders' M.! b) == 1) (borders t))) tiles
    --print corners
    --print $ length corners
    --print $ map fst corners
    print $ product $ map fst corners
