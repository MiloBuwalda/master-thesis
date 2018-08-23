{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module GridHandler where

import qualified Data.List.Split as L
import qualified Data.Maybe as M

import qualified Math.Geometry.Grid as G
import qualified Math.Geometry.Grid.Square as G
import qualified Math.Geometry.GridMap.Lazy as G
import qualified Math.Geometry.GridMap as G


-- main :: IO ()
-- main = do 
--     print $ findClosestPath gridMap (3,1) 
--     putStrLn ""
    -- print $ pathSearch gridMap (3,1) (3,1)

type Point = (Int, Int)
type Tile = (Point, GridType)

type GRID = G.LGridMap G.RectSquareGrid GridType

-- import Test.QuickCheck

squaresum :: (G.Grid g) => g -> G.Index g -> G.Index g -> Int 
squaresum = G.distance

fillList :: l -> Int -> [l]
fillList _ 0 = error "cannot fill emptylist"
fillList l 1 = [l]
fillList l i = l : fillList l (i - 1)

grid :: G.RectSquareGrid
grid = G.rectSquareGrid 8 7

gridMap :: GRID
gridMap = gridMap18

gridMap18 :: GRID
gridMap18 = G.lazyGridMap grid grid18

gridMapI :: Int -> GRID
gridMapI i = case i of
            1 -> G.lazyGridMap grid grid1
            2 -> G.lazyGridMap grid grid4
            _ -> G.lazyGridMap grid grid18

data GridType = Start | Goal | Player | Path | Wall deriving (Eq, Show)

type GridMapType = GRID

getTileFromType :: GridType -> GridMapType -> [Tile] -- Maybe (Int,Int)
getTileFromType x zs = filter ((==x).snd) $ G.toList zs

getAllTiles :: GridMapType -> [Point]
getAllTiles zs = map fst $ G.toList zs

playerLocation :: GRID -> Point
playerLocation g = fst (head (getTileFromType Start g))

goalLocation :: GRID -> Point
goalLocation  g = fst (head (getTileFromType Goal g))

swapTiles :: GridMapType -> Point -> Point -> GridMapType
swapTiles gridM p1 p2 =    
    G.insert p1 (M.fromJust $ G.lookup p2 gridM ) $ G.insert p2 (M.fromJust $ G.lookup p1 gridM ) gridM


setPlayerLocation :: GridMapType -> Point -> GridMapType
setPlayerLocation gM pL = G.insert pL Player gM

getAllPaths :: GridMapType -> [Point]
getAllPaths zs = map fst ( getTileFromType Path zs)

isTile :: Point -> GridType -> Bool
isTile pL gT = elem pL $ map fst $ getTileFromType gT gridMap18

isWalkable :: Point -> Bool
isWalkable pL = isTile pL Path || isTile pL Start || isTile pL Goal

isGoal :: Point -> Bool
isGoal tile = isTile tile Goal

findClosestPath :: GridMapType -> Point -> Point 
findClosestPath gM cP = snd $ minimum $ zip ( map (G.distance gM cP) allPaths ) allPaths
    where allPaths = getAllPaths gM

findPathTo :: GRID -> Point -> Point -> [Point]
findPathTo g start end = 
    if isWalkable end && isWalkable start
        then calcOptimalPath start
        else []
        where 
            calcOptimalPath from = head $ map reverse $ getOptimalPath g [[from]] end

findPathToGoal :: GRID -> Point -> [Point]
findPathToGoal g start =  
    if isWalkable start 
      then calcOptimalPath start
      else calcWallToPath ++ calcOptimalPath nearestPath
      where 
          calcOptimalPath from = head $ map reverse $ getOptimalPath g [[from]] (goalLocation g)
          calcWallToPath = head $ G.minimalPaths (G.toGrid gridMap18) start nearestPath
          nearestPath = findClosestPath gridMap18 start

getOptimalPath :: GRID -> [[Point]] -> Point -> [[Point]]
getOptimalPath g tracks goalPoint = case goalReached of
  []  -> getOptimalPath g (concatMap (gh_expand g) tracks) goalPoint
  x   -> x
  where goalReached = filter (\x -> head x == goalPoint) tracks

gh_expand :: GRID -> [Point] -> [[Point]]
gh_expand g path = map (: path) (getNeighbours g (head path) )


getNeighbours :: GRID -> Point -> [Point]
getNeighbours g current = filter isWalkable $ G.neighbours g current




-- pathSearch moet van een path door alle paden zoeken totdat de goal is bereikt

removeItems :: (Eq t) => [t] -> [t] -> [t]
removeItems rs y = foldr removeItem y rs 

removeItem :: (Eq t) => t -> [t] -> [t]
removeItem _ [] = []
removeItem x (y:ys) 
    | x == y    = removeItem x ys
    | otherwise = y : removeItem x ys

-- get all paths, get closest path

grid1, grid4, grid18 :: [GridType]
grid1 = [w, w, w, w, w,
         w, w, w, w, w,
         w, w, p, g, w,
         w, s, p, w, w,
         w, w, w, w, w]

grid4 = [w, w, w, w, w,
         w, w, w, w, w,
         w, w, p, g, w,
         w, s, p, w, w,
         w, w, w, w, w]

grid18 = [w, w, w, w, w, w, w, w,
         w, w, p, w, w, w, w, w,
         g, p, p, p, p, p, p, w,
         w, w, w, p, w, w, p, w,
         w, w, w, p, w, w, p, w,
         w, w, w, w, w, s, p, w,
         w, w, w, w, w, w, w, w]

-- evalCondition GoalReached (moveForward $ turnLeft $ moveForward $ turnRight $ moveForward $ turnLeft $ moveForward getState)

w,s,g,pl,p :: GridType
w  = Wall
s  = Start
g  = Goal
pl = Player
p  = Path

-------------------- Print --------------------

printGridTiles :: GridMapType -> IO ()
printGridTiles gm = mapM_ (putStrLn . concat) (L.chunksOf 5 (map showTile (getAllTiles gm)))

printGrid :: GridMapType -> IO ()
printGrid gm      = mapM_ putStrLn $ L.chunksOf 5 (map printGridTile (G.toList gm ) )

printGridTile :: ((Int, Int), GridType) -> Char
printGridTile ((_,_),Wall)   = '#' -- "w"
printGridTile ((_,_),Path)   = '.' -- "w"
printGridTile ((_,_),Player) = '*' -- "w"
printGridTile ((_,_),Goal)   = 'g' -- "w"
printGridTile ((_,_),Start)  = 's' -- "w"

showTile :: (Int,Int) -> String
showTile (x,y) = "(" ++ show x ++"," ++ show y ++ ")"
