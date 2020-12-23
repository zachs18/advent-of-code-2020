import System.Environment
import System.IO

data Action = Action Char Int
	deriving Show

type Direction = Int

data State = State Direction Int Int -- X Y
	deriving Show

stateDirection :: State -> Direction
stateDirection (State d _ _) = d

stateX :: State -> Int
stateX (State _ x _) = x

stateY :: State -> Int
stateY (State _ _ y) = y

parseAction :: String -> Action
parseAction (c:cs) = Action c (read cs)

rotate180 :: Direction -> Direction
rotate180 dir = mod (dir+2) 4

rotateLeft :: Direction -> Direction
rotateLeft dir = mod (dir+1) 4

rotateRight :: Direction -> Direction
rotateRight dir = mod (dir-1) 4

doAction :: Action -> State -> State
doAction (Action 'E' dist) (State dir x y) = State dir (x+dist) y
doAction (Action 'N' dist) (State dir x y) = State dir x (y+dist)
doAction (Action 'W' dist) (State dir x y) = State dir (x-dist) y
doAction (Action 'S' dist) (State dir x y) = State dir x (y-dist)
doAction (Action 'F' dist) (State dir@0 x y) = State dir (x+dist) y
doAction (Action 'F' dist) (State dir@1 x y) = State dir x (y+dist)
doAction (Action 'F' dist) (State dir@2 x y) = State dir (x-dist) y
doAction (Action 'F' dist) (State dir@3 x y) = State dir x (y-dist)
doAction (Action 'L' 90) (State dir x y) = State (rotateLeft dir) x y
doAction (Action 'R' 90) (State dir x y) = State (rotateRight dir) x y
doAction (Action 'L' 180) (State dir x y) = State (rotate180 dir) x y
doAction (Action 'R' 180) (State dir x y) = State (rotate180 dir) x y
doAction (Action 'L' 270) (State dir x y) = State (rotateRight dir) x y
doAction (Action 'R' 270) (State dir x y) = State (rotateLeft dir) x y
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
	
	let state = (State 0 0 0)
	let state' = doActions actions state

	print $ (abs $ stateX state') + (abs $ stateY state')
