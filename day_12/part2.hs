import System.Environment
import System.IO

data Action = Action Char Int
	deriving Show

type Waypoint = (Int, Int)

data State = State Waypoint Int Int -- X Y
	deriving Show

stateWaypoint :: State -> Waypoint
stateWaypoint (State d _ _) = d

stateX :: State -> Int
stateX (State _ x _) = x

stateY :: State -> Int
stateY (State _ _ y) = y

parseAction :: String -> Action
parseAction (c:cs) = Action c (read cs)

rotate180 :: Waypoint -> Waypoint
rotate180 (wx, wy) = (-wx, -wy)


rotateLeft :: Waypoint -> Waypoint
rotateLeft (wx, wy) = (-wy, wx)

rotateRight :: Waypoint -> Waypoint
rotateRight (wx, wy) = (wy, -wx)

doAction :: Action -> State -> State
doAction (Action 'E' dist) (State way@(wx, wy) x y) = State (wx+dist, wy) x y
doAction (Action 'N' dist) (State way@(wx, wy) x y) = State (wx, wy+dist) x y
doAction (Action 'W' dist) (State way@(wx, wy) x y) = State (wx-dist, wy) x y
doAction (Action 'S' dist) (State way@(wx, wy) x y) = State (wx, wy-dist) x y
doAction (Action 'F' dist) (State way@(wx, wy) x y) = State way (x+wx*dist) (y+wy*dist)
doAction (Action 'L' 90) (State way x y) = State (rotateLeft way) x y
doAction (Action 'R' 90) (State way x y) = State (rotateRight way) x y
doAction (Action 'L' 180) (State way x y) = State (rotate180 way) x y
doAction (Action 'R' 180) (State way x y) = State (rotate180 way) x y
doAction (Action 'L' 270) (State way x y) = State (rotateRight way) x y
doAction (Action 'R' 270) (State way x y) = State (rotateLeft way) x y
doAction a s = error (show a ++ "," ++ show s)

doActions :: [Action] -> State -> State
doActions [] s = s
doActions (a:as) s = doActions as (doAction a s)

main :: IO ()
main = do
	args <- getArgs
	let filename = head args
	file <- openFile filename ReadMode

	contents <- hGetContents file
	let actions = map parseAction $ lines contents
	
	let state = (State (10, 1) 0 0)
	let state' = doActions actions state

	print $ (abs $ stateX state') + (abs $ stateY state')
