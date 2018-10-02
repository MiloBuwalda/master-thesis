{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
module GridHandler where

import           System.IO
import           Control.Monad
-- import           Control.Monad.Trans
import           Control.Applicative
-- import {-qualified-} Control.Monad.State {-as S-}

import           Data.List
import qualified Data.List.Split            as L
import           Data.Int
import           Data.Char
import qualified Data.Map                   as M
import qualified Data.Maybe                 as M

-- import           GridHandler
import qualified Math.Geometry.Grid.Square as G
import qualified Math.Geometry.GridMap.Lazy as G
import qualified Math.Geometry.GridMap as G
import qualified Math.Geometry.Grid as G

-- data Expr = IF Condition Expr Expr
--           | WHILE Expr
--           | SEQ [Expr]
--           | Forward
--           | TurnLeft
--           | TurnRight
--           | TERMINATE
--           | Empty 
--           | VOID 
--             deriving (Show, Eq, Ord)

-- data Condition = PathAhead 
--                | PathLeft 
--                | PathRight 
--                | GoalReached 
--                     deriving (Show, Eq, Ord, Enum)

-- type Forward   = String
-- type TurnLeft  = String
-- type TurnRight = String

-- -- type StateExpr = (State,[Expr])

type Depth    = Int
type Distance = Int
type Length   = Int

type Location = (Int,Int)
data Direction = UpD | RightD | DownD | LeftD  deriving (Show,Eq,Enum, Bounded)

data Character = Character {
    location    :: Location, -- 
    orientation :: Direction-- playerOrientation
    -- grid :: GridMapType --[((Int,Int), GridType)]
    } deriving (Eq)

instance Show Character where
    show x = "Character @ " ++ show (location x) ++ " going " ++ show (orientation x) ++ " = " ++show (characterTile x) ++ "\n" ++ showGrid x


-- eval :: Expr -> Character -> Character
-- eval x            char | goalReached char = char
-- eval x            char | not (isWalkable $ location char)  = stepBack char
-- eval Forward      char = walk char
-- eval TurnLeft     char = turn TurnLeft char
-- eval TurnRight    char = turn TurnRight char
-- eval (IF c l r)   char = if evalCondition c char then eval l char else eval r char
-- --
-- eval (SEQ [])     char = char  
-- eval (SEQ (x:xs)) char = eval (SEQ xs) (eval x char )
-- -- 
-- eval (WHILE x)    char =
--     if (evalCondition GoalReached char) 
--         then char
--         else if not (isWalkable $ location char)
--                 then stepBack char
--                 else eval (WHILE x) (eval x char)  
-- eval VOID         char = char
-- eval Empty        char = char
-- eval TERMINATE    char = char




-- evalCondition :: Condition -> Character -> Bool
-- evalCondition PathAhead char
--     | characterTile (walk char) == Path = True
--     | characterTile (walk char) == Goal = True
--     | otherwise                                         = False
-- evalCondition PathLeft      s = pathLeft s
-- evalCondition PathRight     s = pathRight s
-- evalCondition GoalReached   s = goalReached s  

pathLeft :: Character -> Bool
pathLeft char = characterTile ( walk $ turn CounterClockWise char) == Path

pathRight :: Character -> Bool
pathRight char = characterTile (walk $ turn ClockWise char) == Path

goalReached :: Character -> Bool
goalReached char = characterTile (char) == Goal

------------------------------- Walking and Turning

newLocation :: Direction -> Location -> Location
newLocation UpD (x,y)    = (x-1,y)
newLocation DownD (x,y)  = (x+1,y)
newLocation LeftD (x,y)  = (x,y-1)
newLocation RightD (x,y) = (x,y+1)

initializeCharacter :: Character
initializeCharacter = Character (playerLocation gridMap18) RightD 

walk :: Character -> Character
walk char = Character (newLocation (orientation char) (location char)) (orientation char)

stepBack :: Character -> Character
stepBack char = turn ClockWise $ turn ClockWise $ walk $ turn CounterClockWise $ turn CounterClockWise char 

data Turn = ClockWise | CounterClockWise deriving (Show)

turn :: Turn -> Character -> Character
turn CounterClockWise char = case orientation char of
    UpD -> char {orientation = LeftD}
    _   -> char {orientation = pred (orientation char)}
turn ClockWise char = case orientation char of
    LeftD -> char {orientation = UpD}
    _   -> char {orientation = succ (orientation char)}
-- turn _ char = char

turnRight,turnLeft :: Character -> Character
turnRight = turn ClockWise
turnLeft = turn CounterClockWise

characterTile :: Character -> GridType
characterTile char = case G.lookup (location $ char) gridMap18 of
  Nothing -> Wall
  Just y  -> y

onWalkable :: Character -> Bool
onWalkable char = characterTile char == Path || characterTile char == Start || characterTile char == Goal

----------------- GRID STUFF
data GridType = Start | Goal | Player | Path | Wall | Direction Direction deriving (Eq, Show)

type GRID = G.LGridMap G.RectSquareGrid GridType
type GridMapType = GRID
type Tile = (Location, GridType)

grid :: G.RectSquareGrid
grid = G.rectSquareGrid 8 7

gridMap18 :: GRID
gridMap18 = G.lazyGridMap grid grid18


getTileFromType :: GridType -> GridMapType -> [Tile] -- Maybe (Int,Int)
getTileFromType x zs = filter ((==x).snd) $ G.toList zs

playerLocation :: GRID -> Location
playerLocation g = fst (head (getTileFromType Start g))

goalLocation :: GRID -> Location
goalLocation  g = fst (head (getTileFromType Goal g))


setPlayerLocation :: GridMapType -> Location -> GridMapType
setPlayerLocation gM pL = G.insert pL Player gM

setPlayer :: GridMapType -> Location -> Direction -> GridMapType
setPlayer gM pL pO = G.insert pL (Direction pO) gM

getAllPaths :: GridMapType -> [Location]
getAllPaths zs = map fst ( getTileFromType Path zs)

isTile :: Location -> GridType -> Bool
isTile pL gT = elem pL $ map fst $ getTileFromType gT gridMap18

isWalkable :: Location -> Bool
isWalkable pL = isTile pL Path || isTile pL Start || isTile pL Goal

isGoal :: Location -> Bool
isGoal tile = isTile tile Goal

grid18 = [w, w, w, w, w, w, w, w,
         w, w, p, w, w, w, w, w,
         g, p, p, p, p, p, p, w,
         w, w, w, p, w, w, p, w,
         w, w, w, p, w, w, p, w,
         w, w, w, w, w, s, p, w,
         w, w, w, w, w, w, w, w]
    where
        w,s,g,pl,p :: GridType
        w  = Wall
        s  = Start
        g  = Goal
        pl = Player
        p  = Path

-------------------- Print --------------------

showGrid :: Character -> String
showGrid char = 
    let grid = setPlayer gridMap18 (location char) (orientation char)
    in concatMap (++"\n") $ L.chunksOf 8 (map printGridTile (G.toList grid ) )

printGrid :: Character -> IO ()
printGrid char = 
    let grid = setPlayer gridMap18 (location char) (orientation char)
    in mapM_ putStrLn $ L.chunksOf 8 (map printGridTile (G.toList grid ) )

-- showChar :: Character -> String
-- showChar char = 
--     let grid = setPlayer gridMap18 (location char) (orientation char)
--         printer = showGrid grid 
--     in printer


-- printChar :: Character -> IO ()
-- printChar char = printGrid (showChar)

printGridTile :: (Location, GridType) -> Char
printGridTile ((_,_),Wall)   = '#' -- "w"
printGridTile ((_,_),Path)   = '.' -- "w"
printGridTile ((_,_),Direction UpD)    = '^' -- "w"
printGridTile ((_,_),Direction RightD) = '>' -- "w"
printGridTile ((_,_),Direction DownD)  = 'V' -- "w"
printGridTile ((_,_),Direction LeftD)  = '<' -- "w"
printGridTile ((_,_),Goal)   = 'g' -- "w"
printGridTile ((_,_),Start)  = 's' -- "w"

showTile :: (Int,Int) -> String
showTile (x,y) = "(" ++ show x ++"," ++ show y ++ ")"



----------------------------------- 
distanceToGoal18 :: Location -> Int
distanceToGoal18 pL = distanceToGoal gridMap18 pL

distanceToGoal :: GridMapType -> Location -> Int
distanceToGoal gM pL = G.distance gridMap18 pL (goalLocation gridMap18)

findClosestPath :: GridMapType -> Location -> Location 
findClosestPath gM cP = snd $ minimum $ zip ( map (G.distance gM cP) allPaths ) allPaths
    where allPaths = getAllPaths gM

findPathTo :: GRID -> Location -> Location -> [Location]
findPathTo g start end = 
    if isWalkable end && isWalkable start
        then calcOptimalPath start
        else []
        where 
            calcOptimalPath from = head $ map reverse $ getOptimalPath g [[from]] end

findPathToGoal :: GRID -> Location -> [Location]
findPathToGoal g start =  
    if isWalkable start 
      then calcOptimalPath start
      else calcWallToPath ++ calcOptimalPath nearestPath
      where 
          calcOptimalPath from = head $ map reverse $ getOptimalPath g [[from]] (goalLocation g)
          calcWallToPath = head $ G.minimalPaths (G.toGrid gridMap18) start nearestPath
          nearestPath = findClosestPath gridMap18 start

getOptimalPath :: GRID -> [[Location]] -> Location -> [[Location]]
getOptimalPath g tracks goalLocation = case goalReached of
  []  -> getOptimalPath g (concatMap (gh_expand g) tracks) goalLocation
  x   -> x
  where goalReached = filter (\x -> head x == goalLocation) tracks

gh_expand :: GRID -> [Location] -> [[Location]]
gh_expand g path = map (: path) (getNeighbours g (head path) )


getNeighbours :: GRID -> Location -> [Location]
getNeighbours g current = filter isWalkable $ G.neighbours g current
