{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
module Expressions where

import           System.IO
import           Control.Monad

import           Data.List
import qualified Data.List.Split            as L
import           Data.Int
import           Data.Char
import qualified Data.Map                   as M
import qualified Data.Maybe                 as M

import qualified Data.Tree                  as T

import           ListZipper
import           Text.Printf (printf)

import qualified Math.Geometry.Grid         as G
import qualified Math.Geometry.Grid.Square  as G
import qualified Math.Geometry.GridMap.Lazy as G
import qualified Math.Geometry.GridMap      as G
import qualified Test.QuickCheck            as QC
import           JsonReader
import           GridHandler
import qualified Edit                       as Ed
import qualified Auxiliary                  as Aux

import           Text.ParserCombinators.Parsec hiding ((<|>), many, State, getState)
import           Control.Applicative

data Expr =   IF Condition Expr Expr
            | WHILE Expr
            | SEQ [Expr]
            | Forward
            | TurnLeft
            | TurnRight
            | TERMINATE
            | Empty 
            | VOID 
              deriving (Show, Eq, Ord)

-- data ExprID = ExprID { expr::Expr, idT::IDTag} deriving (Show, Eq, Ord)

data IDTag = I |W |S |F |L |R |T |E |V |PA |PL |PR| GR | Q deriving (Show, Eq, Ord, Enum) 

extractCond :: Expr -> IDTag
extractCond (IF PathAhead _ _)      = PA
extractCond (IF PathLeft _ _)       = PL
extractCond (IF PathRight _ _)      = PR
-- extractCond (IF GoalReached _ _) = GR
extractCond _                       = T

exprCond :: Expr -> IDTag
exprCond (IF PathAhead VOID VOID)      = PA
exprCond (IF PathAhead VOID TERMINATE) = PA
exprCond (IF PathLeft VOID VOID)       = PL
exprCond (IF PathLeft VOID TERMINATE)  = PL
exprCond (IF PathRight VOID VOID)      = PR
exprCond (IF PathRight VOID TERMINATE) = PR
exprCond x = exprID x 

exprID :: Expr -> IDTag
exprID (IF _ _ _) = I
exprID (WHILE _)  = W
exprID (SEQ _)    = S
exprID Forward    = F
exprID TurnLeft   = L
exprID TurnRight  = R
exprID TERMINATE  = T
exprID Empty      = E
exprID VOID       = V

idExpr :: IDTag -> Expr
idExpr I  = (IF GoalReached VOID VOID)
idExpr W  = (WHILE VOID)
idExpr S  = (SEQ [VOID])
idExpr F  = Forward
idExpr L  = TurnLeft
idExpr R  = TurnRight
idExpr T  = TERMINATE
idExpr E  = Empty
idExpr V  = VOID
idExpr PA = IF PathAhead VOID TERMINATE
idExpr PL = IF PathLeft VOID TERMINATE
idExpr PR = IF PathRight VOID TERMINATE
idExpr GR = WHILE VOID
-- idExpr _ = TERMINATE


idCond :: IDTag -> Condition
idCond PA = PathAhead
idCond PL = PathLeft
idCond PR = PathRight
idCond GR = GoalReached

condID :: Condition -> IDTag
condID PathAhead    = PA
condID PathLeft     = PL
condID PathRight    = PR
condID GoalReached  = GR

setCondition :: Expr -> Condition -> Expr
setCondition (IF _ l r) x = IF x l r
setCondition x _          = x

data Condition =  PathAhead 
                | PathLeft 
                | PathRight 
                | GoalReached 
                    deriving (Show, Eq, Ord, Enum)

type Forward = String
type TurnLeft = String
type TurnRight = String

type StateExpr = (State,[Expr])

type Depth    = Int
type Distance = Int
type Length   = Int

data State = State {
    pL :: (Int,Int), -- 
    pO :: (Int,Int) -- playerOrientation
    -- grid :: GridMapType --[((Int,Int), GridType)]
    } deriving Show


getState :: State -- (loc) (or) (grid)
getState = State (playerLocation gridMap18) (0,1) --gridMap


--------------------------------------------
--    DELETIONS
--------------------------------------------
ts = (WHILE (SEQ [Forward,IF PathAhead TurnRight Forward]),WHILE (IF PathAhead TurnRight Forward))

type Parent     = Expr
type Current    = Expr -- Focus
type Child      = Expr
type ExprZipper = (Expr,Expr)

headExpr :: Expr -> ExprZipper
headExpr (WHILE x)    = (WHILE VOID,x)
headExpr (IF c l r)
  | leaf l && leaf r = (IF c l r,VOID)
  | leaf l           = (IF c l VOID,r)
  | leaf r           = (IF c VOID r,l)
  | otherwise        = (IF c VOID TERMINATE,SEQ [l,r])   -- [(IF c VOID r,l),(IF c l VOID,r)]
headExpr (SEQ [x])    = (x,VOID) 
headExpr (SEQ [x,y])  = (SEQ [x,VOID],SEQ [y]) 
headExpr (SEQ (x:xs)) = (SEQ [x,VOID],SEQ xs)
headExpr x            = (x,VOID)

childrenExpr :: Expr -> [Expr]
childrenExpr (WHILE x)  = [x]
childrenExpr (IF c l r) = [IF c VOID VOID,l,r]
childrenExpr (SEQ xs)   = xs
childrenExpr x          = [x]

childrenContain :: Expr -> IDTag -> Bool
childrenContain inp idT = 
  let tmp x = exprID x == idT 
  in any tmp $ childrenExpr inp

childrenSEQ :: Expr -> [Expr]
childrenSEQ (SEQ xs) = xs
childrenSEQ x        = [x]

type ExprHandler = [Expr] -> Expr

handleChildren :: IDTag -> ([Expr] -> Expr) -> Expr -> Expr
handleChildren i f e
  | i == (exprID e) = let children = (childrenExpr e) in case i of
    W -> WHILE VOID
    I -> IF PathAhead VOID VOID
    S -> SEQ [e]
    _ -> VOID
  | otherwise = TERMINATE

leaf :: Expr -> Bool
leaf Forward    = True
leaf TurnLeft   = True
leaf TurnRight  = True
leaf Empty      = True
leaf _          = False

contains :: IDTag -> Expr -> Bool
contains x xs = containsID x (map exprID $ tokenizer xs) where
  containsID :: IDTag -> [IDTag] -> Bool
  containsID x xs = x `elem` xs


tokenizer :: Expr -> [Expr]
tokenizer (WHILE x)  = (WHILE VOID) : tokenizer x
tokenizer (IF c l r) = (IF c VOID r) : (tokenizer l) ++ (IF c l VOID) : (tokenizer r)
tokenizer (SEQ xs)   = tk xs where
  tk []                = [SEQ []]
  tk [x]               = (SEQ [] ) : tokenizer (x)
  tk (x:xs)            = (SEQ [] ) : tokenizer (x) ++ tk (xs)
tokenizer x          = [x]

flattenSEQ :: Expr -> Expr
flattenSEQ (SEQ (x:[])) = flattenSEQ x
flattenSEQ (SEQ s)      = SEQ $ flattenSEQ' (SEQ s) where
  flattenSEQ' :: Expr -> [Expr]
  flattenSEQ' (SEQ (x:[])) = [x]
  flattenSEQ' (SEQ xs)     = concatMap flattenSEQ' xs
  flattenSEQ' (WHILE x)    = [flattenSEQ (WHILE x)]
  flattenSEQ' (IF c x y)   = [IF c (flattenSEQ x) (flattenSEQ y)]
  flattenSEQ' (x)          = [x]
flattenSEQ (WHILE x)    = WHILE (flattenSEQ x)
flattenSEQ (IF c x y)   = IF c (flattenSEQ x) (flattenSEQ y)
flattenSEQ x            = x

exprCounter :: Expr -> Int
exprCounter (WHILE x) = 1 + (exprCounter x)
exprCounter (IF _ l r)= 1 + exprCounter l + exprCounter r
exprCounter (SEQ xs)  = sum $ map exprCounter xs
exprCounter _         = 1

---------------------------------------------------------------------
-- Distance calculater
---------------------------------------------------------------------

exprSize :: Expr -> Int
exprSize = sum . fmap (\x -> 1) . exprToTree

-- | Naive way of measuring distance by serializing the tree and leving the strings
distance :: Expr -> Expr -> Int
distance x y = Aux.lev (stripEncoding $ encodeExpr x ) (stripEncoding $ encodeExpr y)

-- | Very crude distance metric for our expressions
distanceSp :: Expr -> Expr -> Int
distanceSp in1 in2 = case (in1,in2) of
  (SEQ xs, y)                   -> sum $ map (distanceSp y) xs
  (x, SEQ ys)                   -> sum $ map (distanceSp x) ys
  (WHILE x, WHILE y)            -> distanceSp x y
  (WHILE x, y)                  -> 2 + distanceSp x y
  (x, WHILE y)                  -> 2 + distanceSp x y
  (c1@(IF _ _ _),c2@(IF _ _ _)) -> distanceIF c1 c2
  (c1@(IF _ l r),y)             -> 3 + distanceSp l y + distanceSp r y
  (x, c2@(IF _ l r))            -> 3 + distanceSp x l + distanceSp x r
  (x,y)                         -> distance x y

-- | Measures the distance between IF expr. Compares the expr in true with true en false with false
distanceIF :: Expr -> Expr -> Int
distanceIF (IF c1 l1 r1) (IF c2 l2 r2) = Aux.lev (encodeCond c1) (encodeCond c2) + distance l1 l2 + distance r1 r2
distanceIF x y = distance x y 

minSubDistance :: Expr -> Expr -> (Expr,Int)
minSubDistance crnt sol = head (minSubDistances crnt sol)

{-| returns the subtrees with lowest distance to solution  -}
minSubDistances :: Expr -> Expr -> [(Expr,Int)]
minSubDistances crnt sol = Aux.getMinFromTuple $ 
  map (\(x,y,z) -> (x,(y-z))) $ subDistances crnt sol -- !!!! SCORE: (y-z)

subDistances :: Expr -> Expr -> [(Expr,Distance,Depth)]
subDistances e sol = go e sol 0 where
  go :: Expr -> Expr -> Depth -> [(Expr,Distance,Depth)]
  go e s d = case e of
    crnt@(WHILE x)  -> (crnt,(distance crnt s),d) : (go x s (d+1))
    crnt@(IF c l r) -> (crnt,(distance crnt s),d) : (go l s (d+1)) ++ (go r s (d+1))
    crnt@(SEQ xs)   -> (crnt,(distance crnt s),d) : concatMap (\x-> go x s (d+1)) xs
    x               -> [(x, distance x s, d)]

-------------------------------------------------
---       TREE 
-------------------------------------------------

data Tree a = Null | Node a [Tree a] deriving (Show)

type Trees a = [Tree a]

mergeTree :: Tree IDTag -> Tree IDTag -> Tree IDTag
mergeTree tree@(Node crnt crnts) newTree@(Node new news)
  | tLeaf tree = if 
    | crnt == V -> (Node new news)
    | otherwise -> tree
  | otherwise = Node crnt (map (\x-> mergeTree x newTree) crnts )

tLeaf :: Tree a -> Bool
tLeaf (Null)      = False
tLeaf (Node x xs) = null xs

cat :: (a -> [b] -> b) -> Tree a -> b
cat f (Node root children) = f root (map (cat f) children)

instance Foldable Tree where
  foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts

instance Traversable Tree where
  traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Functor Tree where
  fmap f (Node x ts) = Node (f x) (fmap f <$> ts)

treeMap f (Node a xs) = Node (f a) (map (treeMap f) xs)

treeScanDown :: (a -> b -> a) -> a -> Tree b -> Tree a
treeScanDown f x (Node y subtrees) = Node g (fmap (treeScanDown f g) subtrees)
    where g = f x y

treeScanUp :: (a -> [a] -> a) -> Tree a -> Tree a
treeScanUp f (Node x []) = Node x []
treeScanUp f (Node x subtrees) = Node (f x (fmap tNode g)) g
    where g = fmap (treeScanUp f) subtrees



exprToTree :: Expr -> Tree IDTag
exprToTree x = go x where
  go (WHILE x)  = (Node (W) [go x])
  go (IF c l r) = (Node (I) [Node (condID c) [], go l, go r])
  go (SEQ xs)   = (Node (S) (map go xs))
  go x          = (Node (exprID x) [])  

treeToExpr :: Tree IDTag -> Expr
-- treeToExpr (Node (PA) l@(x:xs)) = (Conds PathAhead)
treeToExpr (Node n l@(x:xs)) = let 
      getCond (Node x _) = idCond x 
      getExpr (Node x _) = idExpr x 
      in case n of
  W -> WHILE (treeToExpr x)
  I -> IF (getCond x) (treeToExpr (xs!!0)) (treeToExpr (xs!!1))
  S -> SEQ (map treeToExpr l)
treeToExpr (Node x []) = (idExpr x)

elemOf :: IDTag -> Tree IDTag -> Bool
elemOf i t = any id $ treeMap (==i) t

idInExpr :: IDTag -> Expr -> Bool
idInExpr i e = elemOf i (exprToTree e)

pathsToNode :: Eq a => a -> Tree a -> [[a]]
pathsToNode x (Node y ns) = [[x] | x == y] ++ map (y:) (pathsToNode x =<< ns)

-- treeSplit :: Eq a => a -> Tree a -> (Tree a, Tree a)
-- treeSplit input crnt@(Node x xs)
--   where nextNode c = case c of
--     W = xs!!0
--     I =  
--   | x == input = case x of
--     I -> (crnt,)
-- treeToNode :: Eq a => a -> Tree a -> [(Tree a)]
-- treeToNode x crnt@(Node y ys)
--   | x == y =  [(crnt)]
--   | otherwise = map (treeToNode x) ys 
-- treeToNode x (Node y ns) = [[Node x xs] | x == y] ++ map (y:) (treeToNode x =<< ns)


treeF :: Eq a => (a -> Bool) -> ([Tree a] -> [Tree a]) -> Tree a -> Tree a
treeF cond procedure input = go cond procedure input where
  go c p i = i

deleteLastError :: Eq a => [Bool] -> [a] -> [Tree a] -> [Tree a]
deleteLastError preConditions erroneousExprs input = go preConditions erroneousExprs input where
  go preC ers i 
    | all id preC = deleteXSTree' tNode ers (reverse i)--map tNode (reverse i)
    | otherwise   = error "error"

-- [IDTag] -> [Tree IDTag] -> [Tree IDTag]

deleteXSTree :: [IDTag] -> [Tree IDTag] -> [Tree IDTag]
deleteXSTree tags [] = []
deleteXSTree tags (t:trees) 
  | elem (tNode t) tags = trees
  | otherwise = t : (deleteXSTree tags trees)

deleteXSTree' :: Eq a => (Tree a -> a) -> [a] -> [Tree a] -> [Tree a]
deleteXSTree' _ _ [] = []
deleteXSTree' f tags (t:trees) 
  | elem (f t) tags = trees
  | otherwise       = t : (deleteXSTree' f tags trees)

deleteXS :: Eq a => [a] -> [a] -> [a]
deleteXS xs [] = []   
deleteXS xs (y:ys)
  | (elem y xs) = ys
  | otherwise   = y:(deleteXS xs ys)
    
tNode :: Tree a -> a
tNode (Node x xs) = x

tChildren :: Tree a -> Trees a
tChildren (Node x xs) = xs

---------------------------------------------------------------------
-- Pretty Printer
---------------------------------------------------------------------
prettyPrint = printExprTree

prettyPrintLn x = mapM prettyPrint x

printExprTree :: Expr -> IO ()
printExprTree x = putStrLn $ printExpr' x 0 "\n"
  where 
    printExpr' x depth b = case x of
      (WHILE x)  -> (replicate (2 * depth) ' ') ++ "WHILE" ++ b ++  (printExpr' x (depth+1) b)
      (IF c t f) -> (replicate (2 * depth) ' ') ++ "IF " ++ (show c) ++ b ++ (printExpr' t (depth+1) b) ++ b ++ (printExpr' f (depth+1) b)
      Forward    -> (replicate (2 * depth) ' ') ++ "Forward"
      TurnLeft   -> (replicate (2 * depth) ' ') ++ "TurnLeft"
      TurnRight  -> (replicate (2 * depth) ' ') ++ "TurnRight"
      Empty      -> (replicate (2 * depth) ' ') ++ "Empty"
      VOID       -> (replicate (2 * depth) ' ') ++ "VOID"
      (SEQ xs)   -> (replicate (2 * depth) ' ') ++ "SEQ " ++ b ++ (intercalate b (map ((\x->printExpr' x (depth+1) b)) xs) )
      -- Node num treeA treeB -> (replicate (2 * depth) ' ') ++ show num ++ "\n" ++ (printExpr' treeA (depth + 1)) ++ "\n" ++ (printExpr' treeB (depth + 1))
      -- Empty -> (replicate (2 * depth) ' ') ++ "Empty" 

printExpr :: Expr -> IO ()
printExpr x = putStrLn $ printExpr' x 0
  where 
    printExpr' x depth  = case x of
      (WHILE x)  -> "WHILE (" ++ (printExpr' x (depth+1) ) ++ ")"
      (IF c t f) -> "IF " ++ (show c) ++ " " ++(printExpr' t (depth+1) )++ " " ++(printExpr' f (depth+1) )
      Forward    -> "Forward"
      TurnLeft   -> "TurnLeft"
      TurnRight  -> "TurnRight"
      Empty      -> "Empty"
      VOID       -> "VOID"
      (SEQ xs)   -> "SEQ [" ++ (intercalate ", " (map (\x->printExpr' x (depth+1)) xs) ) ++ "]"


-------------------------------------------------------------------- Encoding / Decoding
------------------------------------------------------------------

stripEncoding :: String -> String
stripEncoding s = stripChars "(,)" s where
  stripChars :: String -> String -> String
  stripChars = filter . flip notElem

encodeExpr :: Expr -> String
encodeExpr Forward     = "F"
encodeExpr TurnLeft    = "L"
encodeExpr TurnRight   = "R"
encodeExpr Empty       = "E"
encodeExpr TERMINATE   = "T"
encodeExpr VOID        = "V"
-- encodeExpr (SEQ []) = "()"
encodeExpr (SEQ x)     = "(" ++ encodeExpr' (SEQ x) ++ ")" where
  encodeExpr' :: Expr -> String
  encodeExpr' (SEQ []) = ""
  encodeExpr' (SEQ (e:es)) 
    | es == []  = encodeExpr e -- ++ encodeExpr (SEQ es) ++ ")"
    | otherwise = encodeExpr e ++ "," ++ encodeExpr' (SEQ es)
-- encodeExpr (SEQ (e:es)) 
--     | es == [] = encodeExpr e -- ++ encodeExpr (SEQ es) ++ ")"
--     | otherwise = "(" ++ encodeExpr e ++ "," ++ encodeExpr (SEQ es) ++ ")"
encodeExpr (WHILE x) = "w" ++ encodeExpr x -- ++ ")"
encodeExpr (IF condition true false) = 
    encodeCond condition ++ encodeExpr true ++ encodeExpr false -- ++ ")"
-- encodeExpr _ = ""

encodeCond :: Condition -> String
encodeCond PathAhead    = "a"
encodeCond PathLeft     = "l"
encodeCond PathRight    = "r"
encodeCond GoalReached  = "g"

decodeExpr :: String -> Expr
decodeExpr s = case parsed of 
  Left _ -> Empty
  Right x -> x
  where parsed = parse exprParser "test" s

-- WHILE (SEQ [Forward,IF PathLeft (SEQ [TurnLeft,Forward]) Forward])
-- "w(F,l(L,F)F)"
exprParser :: Parser Expr
exprParser = exprForward <|> exprTurnLeft <|> exprTurnRight <|> exprEmpty <|> exprSEQ <|> exprWHILE <|> exprIF

matchSEQ, matchForward, matchTurnLeft, matchTurnRight,matchWHILE, matchEmpty :: Parser String
matchForward    = string "F"
matchTurnRight  = string "R"
matchTurnLeft   = string "L"
matchWHILE      = string "w"
matchSEQ        = string "("
matchEmpty      = string "E"

matchCond :: Parser Condition
matchCond =  (string "a" *> (pure PathAhead)) 
         <|> (string "l" *> (pure PathLeft)) 
         <|> (string "r" *> (pure PathRight)) 
         <|> (string "g" *> (pure GoalReached))

sIF :: Parser Expr
sIF = do
  cond <- matchCond
  ex1  <- exprParser
  ex2  <- exprParser 
  return (IF cond ex1 ex2)


exprIF :: Parser Expr
exprIF = sIF

exprForward :: Parser Expr
exprForward = matchForward *> (pure Forward)

exprTurnLeft :: Parser Expr
exprTurnLeft = matchTurnLeft *> (pure TurnLeft)

exprEmpty :: Parser Expr
exprEmpty = matchEmpty *> (pure Empty)

exprTurnRight :: Parser Expr
exprTurnRight = matchTurnRight *> (pure TurnRight)

-- sWHILE :: Parser Expr
-- sWHILE = matchWHILE *> expr

exprWHILE :: Parser Expr
exprWHILE = WHILE <$> (matchWHILE *> exprParser)
  -- WHILE <$> (matchWHILE *> 
              -- ((char '(') *>
              --   (expr)
              -- <* (char ')')))



sSEQ :: Parser [Expr]
sSEQ =
  (char '(') 
  *>
  (exprParser `sepBy` (char ','))
  <*
  (char ')')

exprSEQ :: Parser Expr
exprSEQ = SEQ <$> sSEQ


-------------------------------------------------
-- PARSING FROM JSON
-------------------------------------------------


parseAST :: AST -> Expr
parseAST AST{astID, children, typeM} 
    | typeM == "maze_ifElse" = IF (parseCondition (head children) ) 
                                  (parseAST (children !! 1) ) 
                                  (parseAST (children !! 2))
    | typeM == "maze_moveForward" = Forward
    | (typeM == "turnRight") || typeM == "maze_turnRight" = TurnRight
    | (typeM == "turnLeft")|| typeM == "maze_turnLeft" = TurnLeft
    | typeM == "maze_forever" = WHILE (if null children then Empty else if length children == 1 then parseAST (head children) else SEQ (map parseAST children))
    | typeM == "statementList" = if null children then Empty else SEQ (map parseAST children)
    | typeM == "DO" = if null children then Empty else parseAST (head children) -- error "type not recognises. Not good"
    | typeM == "program" = if null children then Empty else if length children == 1 then parseAST (head children) else SEQ (map parseAST children) -- error "type not recognises. Not good"
    | typeM == "maze_turn" = if null children then Empty else  parseAST (head children) -- error "type not recognises. Not good"
    | typeM == "ELSE" = if null children then Empty else parseAST (head children) -- error "type not recognises. Not good"
    | otherwise = error ("! " ++ typeM ++ " ! not recognises. Not good") 
    -- where
    --   failSafe :: AST -> Expr
    --   failSafe children = if null inp then Empty else parseAST inp 

parseCondition :: AST -> Condition
parseCondition AST{astID, children, typeM}
    | typeM == "isPathForward" = PathAhead
    | typeM == "isPathLeft" = PathLeft
    | typeM == "isPathRight" = PathRight


-------------------- Printing --------------------
class Display a where
    display :: a -> IO ()

instance Display State where
    display State{pL, pO} = mapM_ putStrLn $ L.chunksOf 5 (map printGridTile (G.toList $gridMap ) ) 

displayTiles :: State -> IO ()
displayTiles State{pL, pO} = mapM_ (putStrLn . concat) (L.chunksOf 8 (map showTile (getAllTiles gridMap)))
-- displayTiles State{pL, pO} = mapM_ putStrLn $ map concat $ L.chunksOf 5 (map showTile (getAllTiles gridMap))


-------------------- grid fn --------------------


getGrid :: GridMapType-- State -> GridMapType
getGrid = gridMap


-- moveForwardState :: State -> State 

moveForward :: State -> Maybe State
moveForward (State pL pO)
    | G.lookup (addPoints pL pO) gridMap == Just Path = Just (State (addPoints pL pO) pO)
    | G.lookup (addPoints pL pO) gridMap == Just Goal = Just (State (addPoints pL pO) pO)
    | otherwise = Nothing

moveForwardState :: State -> State
moveForwardState (State pL pO) 
    | G.lookup (addPoints pL pO) gridMap == Just Path = State (addPoints pL pO) pO
    | G.lookup (addPoints pL pO) gridMap == Just Goal = State (addPoints pL pO) pO
    | otherwise = State pL pO

rotateLeft :: Point -> Point
rotateLeft (pOx, pOy) = (-pOy,pOx)

rotateRight :: Point -> Point
rotateRight (pOx, pOy) = (pOy,-pOx)

turnLeft :: State -> State
turnLeft (State pl (pOx, pOy) ) = State pl (newX, newY) where
    newX = -pOy
    newY = pOx

turnRight :: State -> State
turnRight (State pl (pOx, pOy) ) = State pl (newX, newY) where
    newX = pOy
    newY = -pOx


pathLeft :: State -> Bool
pathLeft (State pl (pox,poy) ) = G.lookup (addPoints pl (-poy,pox)) gridMap == Just Path

pathRight :: State -> Bool
pathRight (State pl (pox,poy) ) = G.lookup (addPoints pl (poy,-pox)) gridMap == Just Path

goalReached :: State -> Bool
goalReached (State pl _ ) = G.lookup pl gridMap == Just Goal


addPoints :: (Int, Int) -> (Int,Int) -> (Int,Int)
addPoints (x1, y1) (x2, y2) = (x1+x2, y1+y2) 

subtractPoints :: (Int, Int) -> (Int,Int) -> (Int,Int)
subtractPoints (x1, y1) (x2, y2) = (x1-x2, y1-y2) 

---------------------Conversion-----------------------------------
fromChar :: Char -> Expr
fromChar 'f' = Forward
fromChar 'r' = TurnRight 
fromChar 'l' = TurnLeft 
fromChar  _  = Empty

toChar :: Expr -> Char
toChar Forward = 'f'
toChar TurnRight = 'r'
toChar TurnLeft = 'l'
toChar _ = 'e'

fromString :: String -> Expr
fromString [] = Empty
fromString xs = SEQ $ map fromChar xs

toString :: Expr -> String
toString Empty = ""
toString xs = case xs of
    SEQ x -> map toChar x
    -- _ -> map toChar xs

-------------------------------------------------------------------------------

solutionFinder :: Expr
solutionFinder = exprSeq ( findPathToGoal gridMap18 (playerLocation gridMap18) ) getState  

currentState = State (3,2) (-1,0) -- (G.lazyGridMap (G.rectSquareGrid 5 5) [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Path,Goal,Wall,Wall,Start,Path,Wall,Wall,Wall,Wall,Wall,Wall,Wall]) )

-- verifiedPart :: [Expr] -> [Expr] -> [Expr]
-- verifiedPart [] _ = []
-- verifiedPart [x] y = if (elem x head y) then [x] else []
-- verifiedPart (x:xs) _ = (x:xs)

-- stateToExpr :: State -> Expr


-- next :: Expr ->  Expr
-- next x = evalState x getState   -- get state
                        -- convert state to [Expr]
currentStateExpr = evalState (SEQ [Forward,TurnLeft,TurnRight,Forward]) (getState, [])

hintE :: StateExpr -> StateExpr -- -> StateExpr --[Expr]
hintE cS = if cmpr then ((nextExpr gridMap18) . nxtPrune) cS else nxtPrune cS 
-- :: StateExpr -- (getState, [nextExpr getState]) :: StateExpr
        where   cmpr = fstT $ compareLists (snd $ nxtPrune cS) (snd cS)
                nxtPrune = (nextExpr gridMap18) . (prune gridMap18)

-- compare two lists and return if equal and if not what differs
    -- first list is first input, second list second input
compareLists :: (Eq a) => [a] -> [a] -> (Bool,[a],[a])
compareLists [] [] = (True, [], [])
compareLists [] y  = (True, [], y)
compareLists x [] = (False, x, [])
-- compareLists [x] [y] = if x == y then True else False
compareLists (x:xs) (y:ys) = if x==y then compareLists xs ys else (False, x : xs, y : ys) 

exprLocation :: [Expr] -> State -> Point
exprLocation [] s = (0,0)
exprLocation ls@(e:es) s = case eval (SEQ ls) s of
                            Just s' -> pL s'
                            Nothing -> (0,0) --init ls
              -- | case eval (hintE e s) s of
              --       Just s' -> (e:[hintE (es) s']
              --       Nothing -> (e:es)
                        -- Just s' -> SEQ (e:[hintE (SEQ es) s'])
                        -- Nothing -> SEQ (e:es)

fstT :: (a,b,c) -> a
fstT (a,_,_) = a

sndT :: (a,b,c) -> b
sndT (_,b,_) = b

thrdT :: (a,b,c) -> c
thrdT (_,_,c) = c

prune :: GRID -> StateExpr -> StateExpr --[Expr] -> [Expr]
prune g (state, expr) = evalState (SEQ exl) (getState,[]) 
    where   exl = exprList (findPathTo g (pL getState) (exprLocation expr getState)) getState


-- filterTurns [F,L,R] -> [F]
-- filterTurns [F,L,R,L] -> [F,L]
filterTurns :: [Expr] -> [Expr] -- -> [Expr]
filterTurns [] = [] 
filterTurns [x] = [x]
filterTurns (e1:e2:es) = 
    if (e1 == TurnLeft && e2 == TurnRight) 
        || (e1 ==TurnRight && e2 ==TurnLeft) 
    then filterTurns (e2:es)
    else filterTurns (e1:es)

checkIfTurnInRightDirection :: StateExpr -> Bool
checkIfTurnInRightDirection (s,e) = case moveForward s of
    Just _ -> True
    Nothing -> False

onWalkable :: Maybe State -> Bool
onWalkable (Just (State pL pO)) = case G.lookup pL gridMap of
  Just Path  -> True
  Just Goal  -> True
  _          -> False
onWalkable Nothing = False

onPath :: Expr -> Bool
onPath x = (not.null) (eval x getState)

correctOrientation :: Expr -> Bool
correctOrientation x = correctOrientation' $ eval x getState where
  correctOrientation' :: Maybe State -> Bool
  correctOrientation' (Just (State pL pO)) = let
    stepForward = addPoints pL pO 
    in case G.lookup stepForward gridMap of
    Just Path  -> True
    Just Goal  -> True
    _          -> False
  correctOrientation' _ = False

isTurn :: Expr -> Bool
isTurn x = x == TurnLeft || x == TurnRight

-- pruning werkt nog niet, want hij neemt de TurnLeft en TurnRight niet mee. Dus hij moet kijken of er nog een TurnLeft of TurnRight is gedaan en die mag hij niet prunen.
    -- case eval (SEQ expr) of
    --     Just s' -> exprList (findPathTo (pL getState) (hintE expr getState)) s'
    --     Nothing -> [Forward] 

-- hintE move s =   case ( moveForward s ) of
--                     Just s' -> 
--                     Nothing -> nextExpr
-- hintE step s = case (eval step s) of
                    -- Just s' -> exprSeq

-- checkForward :: State -> [Expr]
-- checkForward x s = case     moveForward s of 
--                         True ->    moveForward s
--                         otherwise ->  moveForward s 

nextExpr :: GRID -> StateExpr -> StateExpr-- State -> Expr
nextExpr g (s,e) =    if goalReached s then (s,e) else evalState (calcExpr s) (s,e) where --calcExpr :: State -> Expr
    calcExpr s = head $ exprList (findPathToGoal g $ pL s) s
                    -- hintE maintain current.
                    -- remove walls

x = evalState Forward (getState, [])


-------------------- EVAL --------------------
--                   EVAL 
-------------------- EVAL --------------------



evalState :: Expr ->  StateExpr -> StateExpr -- State -> [Expr] -> (State,[Expr])
evalState (SEQ []) (s, [])      = (s,[])
evalState (SEQ []) (s, x)       = (s,x)
evalState (SEQ (e:es)) (s,y)    = 
    case eval e s of
    Just s' -> evalState (SEQ es) (s', y ++ [e])
    Nothing -> (s, y)
evalState Forward (s,y)         = 
    case moveForward s of
        Just s' -> (s', y++[Forward]) -- Forward:y)
        Nothing -> (s, y)
evalState TurnRight (s,y)       = (turnRight s, y++[TurnRight]) --TurnRight:y)
evalState TurnLeft (s,y)        = (turnLeft s, y++[TurnLeft]) --TurnLeft:y)
evalState Empty (s,y)           = (s,[])

-- evalState :: Expr -> State -> State
-- evalState (SEQ []) s = s
-- evalState (SEQ (e:es)) s = case (eval e s) of
--                         Just s' -> evalState (SEQ es) s'
--                         Nothing -> s
-- evalState Forward s = case moveForward s of
--                         Just s' -> s'
--                         Nothing -> s
-- evalState TurnRight s = turnRight s
-- evalState TurnLeft s = turnLeft s


----------------------------------------------
eval :: Expr -> State -> Maybe State
eval (SEQ []) s     = Just s
eval (SEQ (e:es)) s = case eval e s of
    Just s' -> eval (SEQ es) s'
    Nothing -> Nothing
eval Forward s      = moveForward s
eval TurnRight s    = Just $ turnRight s
eval TurnLeft s     = Just $ turnLeft s

----------------------------------------------
eval (IF c t f) s   = if evalCondition c s then eval t s else eval f s
eval (WHILE e) s    = evalWhile e 14 GoalReached s
eval _ s            = Nothing

evalCondition :: Condition -> State -> Bool
evalCondition PathAhead   State{pL, pO}
    | G.lookup (addPoints pL pO) gridMap18 == Just Path = True
    | G.lookup (addPoints pL pO) gridMap18 == Just Goal = True
    | otherwise                                         = False
evalCondition PathLeft      s = pathLeft s
evalCondition PathRight     s = pathRight s
evalCondition GoalReached   s = goalReached s  -- or max depth -- or hit wall
-- eval nothing terminate

evalWhile :: Expr -> Int -> Condition -> State -> Maybe State
evalWhile e i c s    
    | (i == 0) = Nothing
    | evalCondition c s = Just s
    -- | otherwise = evalWhile e (i-1) c (eval e s)
    | otherwise = case eval e s of
        Just s' -> evalWhile e (i - 1) c s'
        Nothing -> Nothing

-- getExpr [(1,3)] getState
exprList :: [Point] -> State -> [Expr]
exprList [] _ = []
exprList (pN:ps) (State pL (pOx,pOy))  
    | pL == pN = exprList ps (State pN (pOx, pOy))
    | stepForward   == pN = Forward : exprList ps (State pN (pOx, pOy))
    | stepLeft      == pN = [TurnLeft, Forward] ++ exprList ps (State pN (-pOy,pOx))
    | stepRight     == pN = [TurnRight, Forward] ++ exprList ps (State pN (pOy,-pOx))
    | stepBackwards == pN = [TurnRight, TurnRight, Forward] ++ exprList ps (State pN (-pOx,-pOy))
    | otherwise = TERMINATE : exprList ps (State pL (pOx, pOy))
    where stepForward     = addPoints pL (pOx,pOy)
          stepLeft        = addPoints pL (-pOy,pOx) -- moveforward rotateLeft
          stepRight       = addPoints pL (pOy,-pOx) -- moveforward rotateRight
          stepBackwards   = addPoints pL (-pOx,-pOy) 

exprSeq :: [Point] -> State -> Expr
exprSeq ps s = SEQ (exprList ps s)

