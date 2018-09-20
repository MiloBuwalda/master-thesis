module Auxiliary where
-- Util functions

import           Data.List            as L
import           Data.List.Split      as L
import           Prelude
import           Data.Array           as A

import           Data.Map (Map)
import qualified Data.Map             as Map
import qualified Data.Map.Strict      as MapS
import qualified Data.Set             as Set

import qualified Data.Maybe           as MB

import           Data.Monoid

import qualified Data.Vector          as V
import qualified Data.Vector.Distance as Vd

import           Text.EditDistance    as T

-----------------------------------------------------------
--			Longest Common Subsequence 
--			& Shortest Edit Distance
-----------------------------------------------------------

longest :: Foldable t => t a -> t a -> t a
longest xs ys = if length xs > length ys then xs else ys

lcs :: Eq a => [a] -> [a] -> [a]
lcs             xs    ys   = a!(0,0) where
  n  = length xs
  m  = length ys
  a  = array ((0,0),(n,m)) $ l1 ++ l2 ++ l3
  l1 = [((i,m),[]) | i <- [0..n]]
  l2 = [((n,j),[]) | j <- [0..m]]
  l3 = [((i,j), f x y i j) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..]]
  f x y i j 
    | x == y    = x : a!(i+1,j+1)
    | otherwise = longest (a!(i,j+1)) (a!(i+1,j))

-- lev :: (Eq a) => [a] -> [a] -> Int
-- lev xs ys = levMemo ! (n, m)
--   where levMemo = array ((0,0),(n,m)) [((i,j),lev i j) | i <- [0..n], j <- [0..m]]
--         n = length xs
--         m = length ys
--         xa = listArray (1, n) xs
--         ya = listArray (1, m) ys
--         lev 0 v = v
--         lev u 0 = u
--         lev u v
--           | xa ! u == ya ! v = levMemo ! (u-1, v-1)
--           | otherwise        = 1 + minimum [levMemo ! (u, v-1),
--                                             levMemo ! (u-1, v),
--                                             levMemo ! (u-1, v-1)] 

lev' :: (Eq a) => [a] -> [a] -> Int
lev'              xs     ys  =  levMemo ! (n, m)
  where levMemo = array ((0,0),(n,m)) [((i,j),lev' i j) | i <- [0..n], j <- [0..m]]
        n        = length xs
        m        = length ys
        xa       = listArray (1, n) xs
        ya       = listArray (1, m) ys
        lev' 0 v = v
        lev' u 0 = u
        lev' u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),
                                            levMemo ! (u-1, v-1)] 

-- | medVecFinal
levFast :: String -> String -> Int
levFast    s1 = V.last . V.ifoldl' scanS1 costs_i . V.fromList
    where vs1 = V.fromList s1
          costs_i =  V.enumFromN 1 $ V.length vs1 -- [0.. length s1]
          scanS1 costs_W costSW_i c2 = 
              let v  =  V.zip vs1 costs_W
                  v' =  V.postscanl' scanVec (costSW_i, costSW_i + 1) v
                  scanVec (costSW, costS) (c1, costW) = 
                     (costW, min (min costS costW + 1)
                                 (costSW + if c1 == c2 then 0 else 1))
               in snd $ V.unzip v'

lev :: String -> String -> Int
lev    s1        s2     = 
  restrictedDamerauLevenshteinDistance newCosts s1 s2

varSubCost :: (Char,Char) -> Int
varSubCost    ('F', 'R')  =  2
varSubCost    ('F', 'L')  =  2
varSubCost    ('F', 'E')  =  1
varSubCost    ('F', 'w')  =  4
varSubCost    ('F', _)    =  100
varSubCost    ('R', 'F')  =  2
varSubCost    ('R', 'L')  =  1
varSubCost    ('R', 'E')  =  1
varSubCost    ('R', _)    =  100
varSubCost    ('L', 'F')  =  2
varSubCost    ('L', 'R')  =  1
varSubCost    ('L', 'E')  =  1
varSubCost    ('L', _)    =  100
varSubCost    ('E', 'F')  =  1
varSubCost    ('E', 'R')  =  1
varSubCost    ('E', 'L')  =  1
varSubCost    ('E', _)    =  100
varSubCost    ('w', 'E')  =  3
varSubCost    ('w', _)    =  100
varSubCost    ('a', 'l')  =  1
varSubCost    ('a', 'r')  =  1
varSubCost    ('a', _)    =  100
varSubCost    ('l', 'r')  =  1
varSubCost    ('l', 'a')  =  1
varSubCost    ('l', _)    =  100
varSubCost    ('r', 'l')  =  1
varSubCost    ('r', 'a')  =  1
varSubCost    ('r', _)    =  100


varDelCost :: Char -> Int
varDelCost    '('  =  1
varDelCost    ')'  =  0
varDelCost    'w'  =  2
varDelCost    'a'  =  3
varDelCost    'l'  =  3
varDelCost    'r'  =  3
varDelCost    _    =  1


newCosts :: T.EditCosts
newCosts = EditCosts 
         { deletionCosts      = VariableCost varDelCost
         , insertionCosts     = ConstantCost 1
         , substitutionCosts  = VariableCost varSubCost
         , transpositionCosts = ConstantCost 10
         }


-- | Editing vectors of 'Char' values, with '(String, Int, Char)' describing
--   changes, and the additive monoid of 'Int' describing costs.
str :: Vd.Params Char (String, Int, Char) (Sum Int)
str = Vd.Params
    { Vd.equivalent     = (==)
    , Vd.delete         = \i c    -> ("delete", i, c)
    , Vd.insert         = \i c    -> ("insert", i, c)
    , Vd.substitute     = \i c c' -> ("replace", i, c')
    , Vd.cost           = const (Sum 1)
    , Vd.positionOffset = \ (op, _, _) -> if op == "delete" then 0 else 1
    }

-- main :: IO ()
-- main = do
--     print $ Vd.leastChanges str (V.fromList "fflr")
                             -- (V.fromList "flfrf")

levL :: String -> String -> Int
levL    s1        s2     = 
  getCost $ fst $ Vd.leastChanges str (V.fromList s1) (V.fromList s2) 
  where
    getCost (Sum x) = (x)

changeLocation :: String -> String -> Int
changeLocation    s1        s2     = snd3.head.snd $ Vd.leastChanges str (V.fromList s1) (V.fromList s2)                             
-----------------------------------------------------------
--			MIN / MAX search in Map
-----------------------------------------------------------


getMinFromMap :: Ord a => Map a1 a -> [a1]
getMinFromMap             m        =  go [] Nothing (Map.toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v > u     = go ks     (Just u) rest
        | v < u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest

getMinFromMap' :: Ord a => Map a1 a -> [(a1,a)]
getMinFromMap'             m        =  go [] Nothing (Map.toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go ((k,v):ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v > u     = go ks     (Just u) rest
        | v < u     = go [(k,v)]    (Just v) rest
        | otherwise = go ((k,v):ks) (Just v) rest

getMaxFromMap :: Ord a => Map a1 a -> [a1]
getMaxFromMap             m        =  go [] Nothing (Map.toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest

getMaxFromMap' :: Ord a => Map a1 a -> [(a1,a)]
getMaxFromMap'             m        =  go [] Nothing (Map.toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go ((k,v):ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [(k,v)]    (Just v) rest
        | otherwise = go ((k,v):ks) (Just v) rest


-- getMaxFromTuple
getMinFromTuple :: Ord a => [(a1, a)] -> [(a1,a)]
getMinFromTuple             m         =  go [] Nothing m
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go ((k,v):ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v > u     = go ks     (Just u) rest
        | v < u     = go [(k,v)]    (Just v) rest
        | otherwise = go ((k,v):ks) (Just v) rest

getMaxFromTuple :: Ord a => [(a1, a)] -> [(a1,a)]
getMaxFromTuple             m         =  go [] Nothing m
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go ((k,v):ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [(k,v)]    (Just v) rest
        | otherwise = go ((k,v):ks) (Just v) rest


-----------------------------------------------------------
--      APPLY Multiple times

-- > Aux.applY ssE (fst $ initCompSE (decodeExpr "w(aFF,wF)")) 5
-----------------------------------------------------------

applY :: (a -> a) -> a -> Int -> [a]
applY    f           x    n   =  take n $ iterate f x

applX :: Eq b => Num b => (a -> a) -> a -> b -> a
applX                     f           x    n =  go f x n where
  go f x 0 = f x 
  go f x d = f (go f x (d-1))

-----------------------------------------------------------
--			(Efficient) List Manipulation
-----------------------------------------------------------

-- From Maybe tuples
catMaybesTuple :: [(b, Maybe a)] -> [(b,a)]
catMaybesTuple    ls             =  [(y,x) | (y, Just x) <- ls]

frequencies :: (Ord a) => [a] -> [(a, Int)]
frequencies               xs  =  Map.toList (Map.fromListWith (+) [(x,1) | x <- xs])

listDifference :: (Ord a) => [a] -> [a] -> [a]
listDifference               xs     ys   =  go initHist xs
  where 
    initHist = MapS.fromListWith (+) [ (x, 1 :: Int) | x <- ys ]

    go _    []     = []
    go hist (x:xs) = case MapS.lookup x hist of
      Just n | n > 0 ->     go (MapS.insert x (n-1) hist) xs
      _              -> x : go hist                      xs

listIntersect :: (Ord a) => [a] -> [a] -> [a]
listIntersect               xs     ys  =  filter (`Set.member` bSet) xs
  where bSet = Set.fromList ys

fst3 :: (a,b,c) -> a
fst3    (x,_,_) =  x

snd3 :: (a,b,c) -> b
snd3    (_,x,_) =  x

trd3 :: (a,b,c) -> c
trd3    (_,_,x) =  x

fsts :: [(a,b)] -> [a]
fsts            =  map fst

snds :: [(a,b)] -> [b]
snds            =  map snd

takeFirstOccurenceTuples :: (Eq a) => (a -> Bool) -> [(b,a)] -> Maybe (b,a)
takeFirstOccurenceTuples              _func          []      =  Nothing
takeFirstOccurenceTuples               func       (y@(yb,ya):ys)
  | func ya  = Just y
  | otherwise = takeFirstOccurenceTuples func ys


safeHead :: [a] -> a  -> a
safeHead    ls     def = safe head ls def

safeLast :: [a] -> a  -> a
safeLast    ls     def = safe last ls def

safeInit :: [a] -> [a] -> [a]
safeInit    ls     def  = safe init ls def

safe :: ([a] -> b) -> [a] -> b  -> b
safe    f             ls     def = if null ls then def else f ls

safeIndex :: [a] -> Int -> a -> a
safeIndex    inp    i      a  = 
  if null inp 
    then a 
    else 
      if i < (length inp) 
        then inp!!i 
        else a


safeTail' :: String -> String
safeTail'    s 
  | s == "" = ""
  | otherwise = tail s

safeHead' :: [a] -> Maybe a
safeHead'    []  =  Nothing
safeHead'    xs  =  Just $ head xs

-- splitted :: String -> [(String,String)]

-- recS 1 ==> [1,2,0]
recursiveSearch :: [(String,String)] -> String  -> [(String,String)]
recursiveSearch    g                    s       =  case lookup s g of
    Nothing  -> []
    Just res -> (s, res) : recursiveSearch g res



lookupRight :: (Eq a) => a -> [(a,b)]   -> [(a,b)]
lookupRight              i    tupleList =  [(x,y) | (x,y) <- tupleList, x == i]

lookupLeft :: (Eq b) => b -> [(a,b)]   -> [(a,b)]
lookupLeft              i    tupleList =  [(x,y) | (x,y) <- tupleList, y == i]


exclusion :: Ord a => a -> [a] -> [a]
exclusion             s    ss  =  MB.fromMaybe [] $ Map.lookup s $ Map.fromList $ selections ss where
  selections :: [a]    -> [(a,[a])]
  selections    []     =  []
  selections    (x:xs) =  (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]


-- obtains all segments when the last char is deleted (init)
subListL :: [String]
subListL = ["flfrf","flfr","flf","fl","f",""]

ordNub :: (Ord a) => [a] -> [a]
ordNub                   =  go Set.empty
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- | \merge' takes a list of list of elements and 
merge :: [[a]] -> [a]    -> [[a]]
merge    xs       []     =  xs
merge    []       ys     =  [ys]
merge    (x:xs)   (y:ys) =  (y : x) : merge xs ys


subList :: [a] -> [[a]]
subList    []  =  [[]]
subList    [s] =  [[s]]
subList    s   =  s : subList (init s)

insertAt :: Int -> a -> [a] -> [a]
insertAt    i      val  ls
    | i<0 = ls
    | otherwise = go i ls
    where
        go 0 xs = val : xs
        go n (x:xs) = x : go (n-1) xs
        go _ [] = []

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace            ns     rs     hs  =  case begins hs ns of
  Just remains -> rs ++ remains
  Nothing      -> case hs of
                    []     -> []
                    x : xs -> x : replace ns rs xs
  where begins :: Eq a => [a] ->   [a]               -> Maybe [a]
        begins            hs       []                =  Just hs
        begins            (x : xs) (y : ys) | x == y =  begins xs ys
        begins            _        _                 =  Nothing


fastContains :: Eq a => [a] -> [a] -> Bool
fastContains            []     []  =  False
fastContains            _      []  =  False
fastContains            []     _   =  False
fastContains            (x:xs) ys  =  (x `elem` ys) || fastContains xs ys

tmp :: String -> String -> Bool
tmp    [_]       _      =  False
tmp    _         [_]    =  False
tmp    _         _      =  True

checkIfSame :: Eq a => [a] -> [a]    -> [Bool]
checkIfSame            []     []     =  [True]
checkIfSame            [_]    []     =  [False]
checkIfSame            []     [_]    =  [False]
checkIfSame            (x:xs) (y:ys) =  (x==y): checkIfSame xs ys

checkIfSame' :: Eq a => [a] -> [a]    -> [Bool]
checkIfSame'            []     []     =  [True]
checkIfSame'            [_]    []     =  [False]
checkIfSame'            []     [_]    =  [False]
checkIfSame'            (x:xs) (y:ys) =  (x==y): checkIfSame' xs ys

allEqual :: Eq a => [a]          -> Bool
allEqual            []           =  True
allEqual            (first:rest) =  all (\elem -> elem == first) rest

-- Tuple and [Tuple] manipultation

showTup :: (String,String) -> String
showTup    (x, y)          =  x ++ "," ++ y

stringify :: [(String,String)] -> String
stringify    ss                =  unlines $ map showTup ss

splitted :: String -> [(String,String)]
splitted    s      =  map splitter (lines s)

splitter :: String -> (String,String)
splitter    s       = toTuple $ splitOn "," s

toTuple :: [String] -> (String, String)
toTuple    [a,b]     = (a,b)
toTuple    _         = ("","")

fromTuple :: (a, a) -> [a]
fromTuple    (a, b) =  [a,b]
-- fromTuple _ = []

goesToX :: Int -> (String, String) -> Bool
goesToX    int    (_, y)            = (read y::Int) == int

astIDsToX :: Int -> [(String,String)] -> [String]
astIDsToX    x      combs             =  map fst $ filter (goesToX x) combs

allThatPointToX :: Int -> [(String,String)] -> [(String,String)]
allThatPointToX    x                         = filter (goesToX x)


----- LOGIC -------
xor :: Bool -> Bool -> Bool
xor    a       b    =  a /= b


------ ZIP ------


zip3' :: [(a,b)] -> [c]                              -> [(a,b,c)]
zip3'    ((a,b):bs) (c:cs)                           =  (a,b,c) : zip3' bs cs
zip3'    _          _                                =  []

zip4'' :: [a] -> [b] -> [(c,d)]                      -> [(a,b,c,d)]
zip4''    (a:as) (b:bs) ((c,d):cs)                   =  (a,b,c,d) : zip4'' as bs cs
zip4''   _          _   _                            =  []

zip4' :: [(a,b)] -> [c] -> [d]                       -> [(a,b,c,d)]
zip4'    ((a,b):bs) (c:cs) (d:ds)                    =  (a,b,c,d) : zip4' bs cs ds
zip4'    _          _       _                        =  []

zip5' :: [(a,b)] -> [(c,d)] -> [e]                   -> [(a,b,c,d,e)]
zip5'    ((a,b):bs) ((c,d):ds) (e:es)                =  (a,b,c,d,e) : zip5' bs ds es
zip5'    _          _          _                     =  []

zip6' :: [(a,b)] -> [(c,(d,e))] -> [f]               -> [(a,b,d,c,e,f)]
zip6'    ((a,b):bs) ((c,(d,e)):ds) (f:es)            =  (a,b,d,c,e,f) : zip6' bs ds es
zip6'    _          _              _                 =  []

zip7' :: [(a,b)] -> [(c,(d,e))] -> [f] -> [g]        -> [(a,b,d,c,e,f,g)]
zip7'    ((a,b):bs) ((c,(d,e)):ds) (f:fs) (g:gs)     =  (a,b,d,c,e,f,g) : zip7' bs ds fs gs
zip7'    _          _              _      _          =  []

zip8' :: [(a,b)] -> [(c,(d,e))] -> [(f,g)] -> [h]    -> [(a,b,d,c,e,f,g,h)]
zip8'    ((a,b):bs) ((c,(d,e)):ds) ((f,g):fs) (h:hs) =  (a,b,d,c,e,f,g,h) : zip8' bs ds fs hs
zip8'    _          _              _          _      =  []


-----------------------------------------------------------
--      List Zipper
-----------------------------------------------------------

type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a  
goForward    ([],x)       =  ([],x)
goForward    (x:xs, bs)   =  (xs, x:bs)
  
goBack :: ListZipper a -> ListZipper a  
goBack    (y,[])        = (y,[])
goBack    (xs, b:bs)    = (b:xs, bs)


printList ss = mapM putStrLn ss

boolTo10  s = if s then 1 else 0

-----------------------------------------------------------
--      PRINTER
-----------------------------------------------------------

printTuplesLn   :: (Show a, Show b)                         => [(a,b)]       -> IO [()]
printTuplesLn                                                  xs             = 
  mapM (\(x,y) -> putStrLn ("(" ++(show x)++" , " ++ (show y)++ ")")) xs

printTuples3Ln' :: (Show a, Show b, Show c)                 => [(a,(b,c))]   -> IO [()]
printTuples3Ln'                                                xs             = 
  mapM (\(x,(y,z)) -> putStrLn ((show x)++":" ++ (show y) ++ ": " ++ (show z))) xs

printTuples3Ln  :: (Show a, Show b, Show c)                 => [(a,b,c)]     -> IO [()]
printTuples3Ln                                                 xs             = 
  mapM (\(x,y,z) -> putStrLn ((show x)++":" ++ (show y) ++ ": " ++ (show z))) xs


printTuples4Ln  :: (Show a, Show b, Show c, Show d)         => [(a,(b,c),d)] -> IO [()]
printTuples4Ln                                                 xs             = 
  mapM (\(x,(y,z),zz) -> putStrLn ((show x)++" (" ++ (show y) ++ "," ++ (show z) ++ ") : " ++ (show zz))) xs

printTuples5Ln  :: (Show a, Show b, Show c, Show d, Show e) => [(a,b,c,d,e)] -> IO [()]
printTuples5Ln                                                 xs             = 
  mapM (\(x,y,z,zz,zzz) -> putStrLn ((show x)++" :" ++ (show y) ++ ": " ++ (show z) ++ " " ++ show zz ++ " " ++ show zzz)) xs


-----------------------------------------------------------
--      WRITER
-----------------------------------------------------------

writeLines :: Show a => FilePath -> [a] -> IO ()
writeLines              fp          xs   = writeFile fp $ intercalate "\n" $ map (\x -> show x) xs 


-----------------------------------------------------------
--			Temporary Lists containing data
-----------------------------------------------------------
hoc18SingleEdit :: [(String,String)]
hoc18SingleEdit = [("(F,F)","F"),("(F,L)","F"),("(F,L,F)","(F,L)"),("(F,L,F,F)","(F,L,F)"),("(F,L,F,F,F)","(F,L,F,F)"),("(F,L,F,F,F,F)","(F,L,F,F,F)"),("(F,L,F,F,F,F,L,wF)","(F,L,F,F,F,L,wF)"),("(F,L,F,F,F,L)","(F,L,F,F,F)"),("(F,L,F,F,F,L,F,F,F)","(F,L,F,F,F,L,F,F)"),("(F,L,F,F,F,L,F,F,F,F)","(F,L,F,F,F,L,F,F,F)"),("(F,L,F,F,F,L,F,F,F,F,F)","(F,L,F,F,F,L,F,F,F,F)"),("(F,L,F,F,F,L,F,F,F,F,F,F)","(F,L,F,F,F,L,F,F,F,F,F)"),("(F,L,F,F,F,L,aFF)","(F,L,F,F,F,L,aFE)"),("(F,L,F,F,F,L,aFF,wF)","(F,L,F,F,F,L,aFF,wE)"),("(F,L,F,F,F,L,wF)","(F,L,F,F,F,L,wE)"),("(F,L,F,F,F,L,waFF)","(F,L,F,F,F,waFF)"),("(F,L,F,F,F,R,wF)","(F,L,F,F,F,wF)"),("(F,L,F,F,L,wF)","(F,L,F,F,wF)"),("(F,L,F,L,F)","(F,L,F,F)"),("(F,L,aFF)","(F,L,aFE)"),("(F,L,aFL)","(F,aFL)"),("(F,L,aFL,wF)","(F,L,aFL,wE)"),("(F,L,aLF)","(F,aLF)"),("(F,L,w(F,L))","(F,L,wF)"),("(F,L,w(F,L,F))","(F,L,w(F,L))"),("(F,L,w(F,aFL))","(F,L,waFL)"),("(F,L,w(F,aLF))","(F,L,waLF)"),("(F,L,w(F,lLF))","(F,w(F,lLF))"),("(F,L,waFF)","(F,L,waFE)"),("(F,L,waFL)","(F,waFL)"),("(F,L,waLF)","(F,L,waEF)"),("(F,R)","F"),("(F,aFL)","aFL"),("(F,aFL,wF)","(F,aFL,wE)"),("(F,aFR)","(F,aFL)"),("(F,aLF)","aLF"),("(F,aLF,wF)","(F,aLE,wF)"),("(F,aLL)","(F,waLL)"),("(F,aLR)","(F,aLL)"),("(F,aRF)","(F,waRF)"),("(F,aRF)","w(F,aRF)"),("(F,aRL)","(F,waRL)"),("(F,l(L,F)F)","(F,lLF)"),("(F,lFF)","(F,aFF)"),("(F,lFF)","lFF"),("(F,lFL)","lFL"),("(F,lLF)","lLF"),("(F,lLF,F)","(F,lLF)"),("(F,lLF,F,F,F,L,wF)","(lLF,F,F,F,L,wF)"),("(F,lLF,wF)","(F,aLF,wF)"),("(F,lLL)","(F,aLL)"),("(F,lLR)","(F,aLR)"),("(F,w(F,L))","w(F,L)"),("(F,w(L,F))","(F,wF)"),("(F,w(L,F))","(F,wL)"),("(F,wL)","(F,wE)"),("(F,waFL)","waFL"),("(F,waLF)","(F,waLE)"),("(F,waLL)","waLL"),("(F,waLR)","(F,waER)"),("(F,waLR)","waLR"),("(F,waRL)","(F,waEL)"),("(F,wlFL)","(F,waFL)"),("(F,wlLF)","wlLF"),("(aFL,F)","w(aFL,F)"),("(aFL,L)","aFL"),("(aFL,w(F,L))","(aFL,wF)"),("(aFL,w(L,F))","(aFL,wF)"),("(aFL,wF)","(aFL,wE)"),("(aFL,wL)","(aFL,wE)"),("(aFL,wR)","(aFL,wE)"),("(aFR,wF)","(aFL,wF)"),("(aFR,wF)","(aFR,wE)"),("(aLF,wF)","(aLF,wE)"),("(aLR,wF)","(aER,wF)"),("(aLR,wF)","(aLR,wE)"),("(lFL,wF)","(aFL,wF)"),("(lLF,F)","lLF"),("(lLF,wF)","(aLF,wF)"),("E","wE"),("F","E"),("a(F,L)L","aFL"),("a(F,L)R","aFR"),("a(F,L)wF","aFwF"),("a(L,F)F","aFF"),("aF(L,F)","aFL"),("aFF","waFF"),("aFL","waFL"),("aFR","aFL"),("aFR","waFR"),("aFwF","aFwE"),("aFwL","aFwE"),("aFwR","aFwE"),("aLF","aEF"),("aLF","waLF"),("aLL","waLL"),("aLR","aER"),("aLwF","aLwE"),("aRF","aEF"),("aRL","waRL"),("aw(F,L)F","a(F,L)F"),("awFL","a(F,wF)L"),("awFL","aFL"),("awFR","awFL"),("awLF","awEF"),("l(F,L)F","lLF"),("l(L,F)F","lLF"),("lFF","aFF"),("lFL","aFL"),("lFR","aFR"),("lFwL","aFwL"),("lLF","wlLF"),("lLwF","aLwF"),("lLwF","lLwE"),("lwFL","awFL"),("lwLF","awLF"),("w(F,F,L)","w(F,L)"),("w(F,F,L,F)","w(F,L,F)"),("w(F,F,lLF)","w(F,lLF)"),("w(F,L)","wF"),("w(F,L,F)","w(F,L)"),("w(F,L,F,F)","w(F,L,F)"),("w(F,L,F,F,F)","w(F,L,F,F)"),("w(F,L,F,F,F,F)","w(F,L,F,F,F)"),("w(F,L,F,F,F,L)","w(F,L,F,F,F)"),("w(F,L,F,F,F,L,F)","w(F,L,F,F,F,L)"),("w(F,L,F,F,F,L,F,F,F)","w(F,L,F,F,F,L,F,F)"),("w(F,L,F,F,F,L,F,F,F,F,F)","w(F,L,F,F,F,L,F,F,F,F)"),("w(F,L,F,F,F,L,F,F,F,F,F,F)","w(F,L,F,F,F,L,F,F,F,F,F)"),("w(F,L,F,F,F,L,aFF)","w(F,L,F,F,F,aFF)"),("w(F,L,F,F,L)","w(F,L,F,F)"),("w(F,L,F,F,aFF)","w(F,L,F,aFF)"),("w(F,L,F,L)","w(F,L,F)"),("w(F,L,F,R)","w(F,L,F)"),("w(F,L,F,aFF)","w(F,L,aFF)"),("w(F,L,F,aFL)","w(F,L,aFL)"),("w(F,L,F,aFR)","w(F,L,aFR)"),("w(F,L,F,aLF)","w(F,L,aLF)"),("w(F,L,F,lFF)","w(F,L,lFF)"),("w(F,L,L)","w(F,L)"),("w(F,L,R)","w(F,L)"),("w(F,L,a(F,F)F)","w(F,L,aFF)"),("w(F,L,aF(L,F))","w(F,L,aFL)"),("w(F,L,aFF)","w(F,L,aFE)"),("w(F,L,aFF)","w(F,aFF)"),("w(F,L,aFF,F)","w(F,aFF,F)"),("w(F,L,aFL)","w(F,aFL)"),("w(F,L,aFL,F)","w(F,L,aFL)"),("w(F,L,aFR)","w(F,aFR)"),("w(F,L,aLF)","w(F,aLF)"),("w(F,L,aLR)","w(F,L,aER)"),("w(F,L,aLR)","w(F,aLR)"),("w(F,L,aRF)","w(F,L,aEF)"),("w(F,L,lFF)","w(F,L,aFF)"),("w(F,L,lFL)","w(F,lFL)"),("w(F,L,lLF)","w(F,lLF)"),("w(F,R)","wF"),("w(F,R,F)","w(F,R)"),("w(F,R,L)","w(F,L)"),("w(F,a(F,L)F)","w(F,aFF)"),("w(F,a(L,F)(L,F))","w(F,aF(L,F))"),("w(F,a(L,F)(R,F))","w(F,a(L,F)(L,F))"),("w(F,a(L,F)L)","w(F,aFL)"),("w(F,a(L,F)R)","w(F,aFR)"),("w(F,a(R,F)(L,F))","w(F,aF(L,F))"),("w(F,a(R,F)L)","w(F,aFL)"),("w(F,aF(L,F))","w(F,aFL)"),("w(F,aFF)","w(F,aFE)"),("w(F,aFL)","waFL"),("w(F,aFL,F)","w(F,aFL)"),("w(F,aFR)","w(F,aFL)"),("w(F,aFR)","waFR"),("w(F,aLF)","w(F,aEF)"),("w(F,aLF)","waLF"),("w(F,aLF,F)","w(F,aLE,F)"),("w(F,aLF,F)","w(F,aLF)"),("w(F,aLL)","w(F,aEL)"),("w(F,aLL,F)","w(F,aLL)"),("w(F,aLL,F,F)","w(F,aEL,F,F)"),("w(F,aLL,F,F)","w(F,aLL,F)"),("w(F,aLR)","waLR"),("w(F,aLR,F)","w(F,aLR)"),("w(F,aR(F,L))","w(F,aRL)"),("w(F,aRF)","w(F,aEF)"),("w(F,aRF)","waRF"),("w(F,aRL)","w(F,aEL)"),("w(F,aRL,F)","w(F,aRL)"),("w(F,aRL,F,F)","w(F,aRL,F)"),("w(F,aRR)","w(F,aRL)"),("w(F,l(F,L)F)","w(F,lLF)"),("w(F,l(L,F)(L,F))","w(F,l(L,F)F)"),("w(F,l(L,F)(R,F))","w(F,l(L,F)F)"),("w(F,l(L,F)F)","w(F,lLF)"),("w(F,l(L,F)L)","w(F,a(L,F)L)"),("w(F,l(L,F)R)","w(F,lLR)"),("w(F,l(L,R)F)","w(F,lLF)"),("w(F,lFF)","wlFF"),("w(F,lFL)","w(F,aFL)"),("w(F,lFR)","w(F,aFR)"),("w(F,lL(F,F))","w(F,aL(F,F))"),("w(F,lL(F,R))","w(F,lLF)"),("w(F,lL(R,F))","w(F,lLF)"),("w(F,lLF)","w(F,aLF)"),("w(F,lLF,F)","w(F,lLF)"),("w(F,lLF,R)","w(F,lLF)"),("w(F,lLF,aFF)","w(F,lLF,aFE)"),("w(F,lLF,aLF)","w(F,lLF,aEF)"),("w(F,lLF,lLF)","w(F,lLF,lEF)"),("w(F,lLF,rRF)","w(F,lLF,aRF)"),("w(F,lLL)","w(F,aLL)"),("w(F,lLL,F)","w(F,lLL)"),("w(F,lLR)","w(F,aLR)"),("w(F,lLR,F)","w(F,lLR)"),("w(F,lLwF)","w(F,aLwF)"),("w(F,lRF)","w(F,lLF)"),("w(F,lRL)","w(F,aRL)"),("w(F,lRR)","w(F,lLR)"),("w(F,rFF)","w(F,aFF)"),("w(F,rFL)","w(F,aFL)"),("w(F,rFR)","w(F,aFR)"),("w(F,rLF)","w(F,aLF)"),("w(F,rLL)","w(F,aLL)"),("w(F,rLR)","w(F,aLR)"),("w(F,rRF)","w(F,aRF)"),("w(F,rRL)","w(F,aRL)"),("w(F,rRR)","w(F,aRR)"),("w(L,F)","wF"),("w(L,F)","wL"),("w(L,aFF)","waFF"),("w(L,aFL)","waFL"),("w(L,aFR)","waFR"),("w(aFF,L)","waFF"),("w(aFL,F)","waFL"),("w(aFR,F)","waFR"),("w(aFR,L)","waFR"),("w(aLF,F)","waLF"),("w(aLR,F)","waLR"),("w(aRL,F)","w(aEL,F)"),("w(lLF,F)","w(aLF,F)"),("w(lLF,F)","wlLF"),("wF","wE"),("wL","wE"),("wR","wE"),("wa(F,L)F","waFF"),("wa(F,L)L","waFL"),("wa(F,L)R","waFR"),("wa(F,L,F)F","wa(F,L)F"),("wa(F,L,F,F)F","wa(F,L,F)F"),("wa(F,R)L","waFL"),("wa(L,F)F","waFF"),("waF(L,F)","waFL"),("waF(L,R)","waFL"),("waF(R,F)","waF(L,F)"),("waF(R,L)","waFL"),("waFF","waFE"),("waFR","waFL"),("waLF","waEF"),("waLL","waEL"),("waRF","waEF"),("waRF","waLF"),("waRL","waEL"),("waRR","waER"),("wl(F,L)F","wa(F,L)F"),("wl(F,L)F","wlLF"),("wl(L,F)F","wa(L,F)F"),("wl(L,F)R","wa(L,F)R"),("wlFF","waFF"),("wlFL","waFL"),("wlFR","waFR"),("wlLF","waLF"),("wlLL","waLL"),("wlLR","waLR"),("wlRF","wlLF"),("wlRL","waRL"),("wlRL","wlLL"),("wrFF","waFF"),("wrFL","waFL"),("wrFR","waFR"),("wrLF","waLF"),("wrRF","waRF"),("wrRL","waRL")]

hoc18 :: [(String,String)]
hoc18 = [("(F,F)","F"),("(F,L)","F"),("(F,L,F)","(F,L)"),("(F,L,F,F)","(F,L,F)"),("(F,L,F,F,F)","(F,L,F,F)"),("(F,L,F,F,F,F)","(F,L,F,F,F)"),("(F,L,F,F,F,F,L,wF)","(F,L,F,F,F,L,wF)"),("(F,L,F,F,F,L)","(F,L,F,F,F)"),("(F,L,F,F,F,L,F,F,F)","(F,L,F,F,F,L,F,F)"),("(F,L,F,F,F,L,F,F,F,F)","(F,L,F,F,F,L,F,F,F)"),("(F,L,F,F,F,L,F,F,F,F,F)","(F,L,F,F,F,L,F,F,F,F)"),("(F,L,F,F,F,L,F,F,F,F,F,F)","(F,L,F,F,F,L,F,F,F,F,F)"),("(F,L,F,F,F,L,aFF)","(F,L,F,F,F,L,aFE)"),("(F,L,F,F,F,L,aFF,wF)","(F,L,F,F,F,L,aFF,wE)"),("(F,L,F,F,F,L,wF)","(F,L,F,F,F,L,wE)"),("(F,L,F,F,F,L,waFF)","(F,L,F,F,F,waFF)"),("(F,L,F,F,F,R,wF)","(F,L,F,F,F,wF)"),("(F,L,F,F,L,wF)","(F,L,F,F,wF)"),("(F,L,F,L,F)","(F,L,F,F)"),("(F,L,aFF)","(F,L,aFE)"),("(F,L,aFL)","(F,aFL)"),("(F,L,aFL,wF)","(F,L,aFL,wE)"),("(F,L,aLF)","(F,aLF)"),("(F,L,w(F,L))","(F,L,wF)"),("(F,L,w(F,L,F))","(F,L,w(F,L))"),("(F,L,w(F,aFL))","(F,L,waFL)"),("(F,L,w(F,aLF))","(F,L,waLF)"),("(F,L,w(F,lLF))","(F,w(F,lLF))"),("(F,L,waFF)","(F,L,waFE)"),("(F,L,waFL)","(F,waFL)"),("(F,L,waLF)","(F,L,waEF)"),("(F,R)","F"),("(F,aFL)","aFL"),("(F,aFL,wF)","(F,aFL,wE)"),("(F,aFR)","(F,aFL)"),("(F,aLF)","aLF"),("(F,aLF)","w(F,aLE)"),("(F,aLF,wF)","(F,aLE,wF)"),("(F,aLL)","(F,aFL)"),("(F,aLL)","(F,waLL)"),("(F,aLR)","(F,aLL)"),("(F,aRF)","(F,waRF)"),("(F,aRF)","w(F,aRF)"),("(F,aRL)","(F,waRL)"),("(F,l(L,F)F)","(F,lLF)"),("(F,lFF)","(F,aFF)"),("(F,lFF)","lFF"),("(F,lFL)","lFL"),("(F,lLF)","lLF"),("(F,lLF,F)","(F,lLF)"),("(F,lLF,F,F,F,L,wF)","(lLF,F,F,F,L,wF)"),("(F,lLF,wF)","(F,aLF,wF)"),("(F,lLL)","(F,aLL)"),("(F,lLR)","(F,aLR)"),("(F,w(F,L))","w(F,L)"),("(F,w(F,L))","wF"),("(F,w(F,L))","wL"),("(F,w(L,F))","(F,wF)"),("(F,w(L,F))","(F,wL)"),("(F,wL)","(F,wE)"),("(F,waFL)","waFL"),("(F,waLF)","(F,waLE)"),("(F,waLL)","waLL"),("(F,waLR)","(F,waER)"),("(F,waLR)","waLR"),("(F,waRL)","(F,waEL)"),("(F,wlFL)","(F,waFL)"),("(F,wlLF)","wlLF"),("(aFF,wL)","(aFL,wL)"),("(aFL,F)","w(aFL,F)"),("(aFL,L)","aFL"),("(aFL,w(F,L))","(aFL,wF)"),("(aFL,w(L,F))","(aFL,wF)"),("(aFL,wF)","(aFL,wE)"),("(aFL,wL)","(aFL,wE)"),("(aFL,wR)","(aFL,wE)"),("(aFR,wF)","(aFL,wF)"),("(aFR,wF)","(aFR,wE)"),("(aLF,wF)","(aLF,wE)"),("(aLR,wF)","(aER,wF)"),("(aLR,wF)","(aLR,wE)"),("(lFL,wF)","(aFL,wF)"),("(lLF,F)","lLF"),("(lLF,wF)","(aLF,wF)"),("E","wE"),("F","E"),("a(F,L)L","aFL"),("a(F,L)R","aFR"),("a(F,L)wF","aFwF"),("a(L,F)F","aFF"),("aF(L,F)","aFL"),("aF(L,wF)","aFL"),("aFF","waFF"),("aFL","waFL"),("aFR","aFL"),("aFR","waFR"),("aFwF","aFwE"),("aFwL","aFwE"),("aFwR","aFwE"),("aLF","aEF"),("aLF","waLF"),("aLL","waLL"),("aLR","aER"),("aLwF","aLwE"),("aRF","aEF"),("aRL","waRL"),("aw(F,L)F","a(F,L)F"),("awFL","a(F,wF)L"),("awFL","aFL"),("awFR","awFL"),("awFR","waFR"),("awLF","awEF"),("l(F,L)F","lLF"),("l(L,F)F","lLF"),("lFF","aFF"),("lFL","aFL"),("lFR","aFR"),("lFwL","aFwL"),("lLF","wlLF"),("lLwF","aLwF"),("lLwF","lLwE"),("lwFL","awFL"),("lwLF","awLF"),("w(F,F,L)","w(F,L)"),("w(F,F,L,F)","w(F,L,F)"),("w(F,F,lLF)","w(F,lLF)"),("w(F,L)","wF"),("w(F,L,F)","w(F,L)"),("w(F,L,F,F)","w(F,L,F)"),("w(F,L,F,F,F)","w(F,L,F,F)"),("w(F,L,F,F,F,F)","w(F,L,F,F,F)"),("w(F,L,F,F,F,L)","w(F,L,F,F,F)"),("w(F,L,F,F,F,L,F)","w(F,L,F,F,F,L)"),("w(F,L,F,F,F,L,F,F,F)","w(F,L,F,F,F,L,F,F)"),("w(F,L,F,F,F,L,F,F,F,F)","(F,L,F,F,F,L,F,F,F)"),("w(F,L,F,F,F,L,F,F,F,F,F)","w(F,L,F,F,F,L,F,F,F,F)"),("w(F,L,F,F,F,L,F,F,F,F,F,F)","w(F,L,F,F,F,L,F,F,F,F,F)"),("w(F,L,F,F,F,L,aFF)","w(F,L,F,F,F,aFF)"),("w(F,L,F,F,L)","w(F,L,F,F)"),("w(F,L,F,F,aFF)","w(F,L,F,aFF)"),("w(F,L,F,L)","w(F,L,F)"),("w(F,L,F,L,F)","(F,L,F,L)"),("w(F,L,F,R)","w(F,L,F)"),("w(F,L,F,aFF)","w(F,L,aFF)"),("w(F,L,F,aFL)","w(F,L,aFL)"),("w(F,L,F,aFR)","w(F,L,aFR)"),("w(F,L,F,aLF)","w(F,L,aLF)"),("w(F,L,F,lFF)","w(F,L,lFF)"),("w(F,L,L)","w(F,L)"),("w(F,L,R)","w(F,L)"),("w(F,L,a(F,F)F)","w(F,L,aFF)"),("w(F,L,aF(L,F))","w(F,L,aFL)"),("w(F,L,aFF)","w(F,L,aFE)"),("w(F,L,aFF)","w(F,aFF)"),("w(F,L,aFF,F)","w(F,aFF,F)"),("w(F,L,aFL)","w(F,aFL)"),("w(F,L,aFL,F)","w(F,L,aFL)"),("w(F,L,aFR)","w(F,aFR)"),("w(F,L,aLF)","w(F,aLF)"),("w(F,L,aLR)","w(F,L,aER)"),("w(F,L,aLR)","w(F,aLR)"),("w(F,L,aRF)","w(F,L,aEF)"),("w(F,L,lFF)","w(F,L,aFF)"),("w(F,L,lFL)","w(F,lFL)"),("w(F,L,lLF)","w(F,lLF)"),("w(F,R)","wF"),("w(F,R,F)","w(F,R)"),("w(F,R,L)","w(F,L)"),("w(F,a(F,L)F)","w(F,aFF)"),("w(F,a(L,F)(L,F))","w(F,aF(L,F))"),("w(F,a(L,F)(R,F))","w(F,a(L,F)(L,F))"),("w(F,a(L,F)F)","w(F,aLE)"),("w(F,a(L,F)L)","w(F,aFL)"),("w(F,a(L,F)R)","w(F,aFR)"),("w(F,a(R,F)(L,F))","w(F,aF(L,F))"),("w(F,a(R,F)L)","w(F,aFL)"),("w(F,aF(L,F))","w(F,aFL)"),("w(F,aF(L,F))","waFL"),("w(F,aFF)","w(F,aFE)"),("w(F,aFL)","waFL"),("w(F,aFL,F)","w(F,aFL)"),("w(F,aFL,F)","w(F,aFL)"),("w(F,aFR)","w(F,aFL)"),("w(F,aFR)","waFR"),("w(F,aLF)","w(F,aEF)"),("w(F,aLF)","waLF"),("w(F,aLF,F)","w(F,aLE,F)"),("w(F,aLF,F)","w(F,aLF)"),("w(F,aLL)","w(F,aEL)"),("w(F,aLL,F)","w(F,aEL)"),("w(F,aLL,F)","w(F,aLL)"),("w(F,aLL,F,F)","w(F,aEL,F,F)"),("w(F,aLL,F,F)","w(F,aLL,F)"),("w(F,aLR)","waLR"),("w(F,aLR,F)","w(F,aLL)"),("w(F,aLR,F)","w(F,aLR)"),("w(F,aR(F,L))","w(F,aRL)"),("w(F,aR(L,F))","w(F,aEL)"),("w(F,aRF)","w(F,aEF)"),("w(F,aRF)","waRF"),("w(F,aRL)","w(F,aEL)"),("w(F,aRL,F)","w(F,aRL)"),("w(F,aRL,F,F)","w(F,aRL,F)"),("w(F,aRR)","w(F,aRL)"),("w(F,l(F,L)F)","w(F,lLF)"),("w(F,l(L,F)(L,F))","w(F,l(L,F)F)"),("w(F,l(L,F)(R,F))","w(F,l(L,F)F)"),("w(F,l(L,F)F)","w(F,lLF)"),("w(F,l(L,F)L)","w(F,a(L,F)L)"),("w(F,l(L,F)R)","w(F,lLR)"),("w(F,l(L,R)F)","w(F,lLF)"),("w(F,lFF)","wlFF"),("w(F,lFL)","w(F,aFL)"),("w(F,lFR)","w(F,aFR)"),("w(F,lL(F,F))","w(F,aL(F,F))"),("w(F,lL(F,R))","w(F,lLF)"),("w(F,lL(R,F))","w(F,lLF)"),("w(F,lLF)","w(F,aLF)"),("w(F,lLF,F)","w(F,lLF)"),("w(F,lLF,R)","w(F,lLF)"),("w(F,lLF,aFF)","w(F,lLF,aFE)"),("w(F,lLF,aFL)","(F,lLF,waFL)"),("w(F,lLF,aLF)","w(F,lLF,aEF)"),("w(F,lLF,lLF)","w(F,lLF,lEF)"),("w(F,lLF,lLF)","w(F,lLF,lEL)"),("w(F,lLF,rRF)","w(F,lLF,aRF)"),("w(F,lLL)","w(F,aLL)"),("w(F,lLL,F)","w(F,lLL)"),("w(F,lLR)","w(F,aLR)"),("w(F,lLR,F)","w(F,lLR)"),("w(F,lLaFF)","waLaFF"),("w(F,lLwF)","w(F,aLwF)"),("w(F,lRF)","w(F,lLF)"),("w(F,lRL)","w(F,aRL)"),("w(F,lRR)","w(F,lLR)"),("w(F,rFF)","w(F,aFF)"),("w(F,rFL)","w(F,aFL)"),("w(F,rFR)","w(F,aFR)"),("w(F,rLF)","w(F,aLF)"),("w(F,rLL)","w(F,aLL)"),("w(F,rLR)","w(F,aLR)"),("w(F,rRF)","w(F,aRF)"),("w(F,rRL)","w(F,aRL)"),("w(F,rRR)","w(F,aRR)"),("w(L,F)","wF"),("w(L,F)","wL"),("w(L,aFF)","waFF"),("w(L,aFL)","waFL"),("w(L,aFR)","waFR"),("w(aFF,L)","waFF"),("w(aFL,F)","waFL"),("w(aFR,F)","waFR"),("w(aFR,L)","waFR"),("w(aLF,F)","waLF"),("w(aLR,F)","waLR"),("w(aRL,F)","w(aEL,F)"),("w(lLF,F)","w(aLF,F)"),("w(lLF,F)","wlLF"),("wF","wE"),("wL","wE"),("wR","wE"),("wa(F,L)F","waFF"),("wa(F,L)L","waFL"),("wa(F,L)R","waFR"),("wa(F,L,F)F","wa(F,L)F"),("wa(F,L,F,F)F","wa(F,L,F)F"),("wa(F,R)L","waFL"),("wa(L,F)F","waFF"),("waF(L,F)","waFL"),("waF(L,R)","waFL"),("waF(R,F)","waF(L,F)"),("waF(R,L)","waFL"),("waFF","waFE"),("waFR","waFL"),("waLF","waEF"),("waLL","waEL"),("waLR","waEF"),("waRF","waEF"),("waRF","waLF"),("waRL","waEL"),("waRR","waER"),("wl(F,L)F","wa(F,L)F"),("wl(F,L)F","wlLF"),("wl(L,F)F","wa(L,F)F"),("wl(L,F)R","wa(L,F)R"),("wlFF","waFF"),("wlFL","waFL"),("wlFR","waFR"),("wlLF","waLF"),("wlLL","waLL"),("wlLR","waLR"),("wlRF","wlLF"),("wlRL","waRL"),("wlRL","wlLL"),("wrFF","waFF"),("wrFL","waFL"),("wrFR","waFR"),("wrLF","waLF"),("wrRF","waRF"),("wrRL","waRL")]

hoc18WS :: [(String,String)]
hoc18WS = [("(F,F)","F"),("(F,L)","F"),("(F,L,F)","(F,L)"),("(F,L,F,F)","(F,L,F)"),("(F,L,F,F,F)","(F,L,F,F)"),("(F,L,F,F,F,F)","(F,L,F,F,F)"),("(F,L,F,F,F,F,L,wF)","(F,L,F,F,F,L,wF)"),("(F,L,F,F,F,L)","(F,L,F,F,F)"),("(F,L,F,F,F,L,F,F,F)","(F,L,F,F,F,L,F,F)"),("(F,L,F,F,F,L,F,F,F,F)","(F,L,F,F,F,L,F,F,F)"),("(F,L,F,F,F,L,F,F,F,F,F)","(F,L,F,F,F,L,F,F,F,F)"),("(F,L,F,F,F,L,F,F,F,F,F,F)","(F,L,F,F,F,L,F,F,F,F,F)"),("(F,L,F,F,F,L,wF)","(F,L,F,F,F,L,wE)"),("(F,L,F,F,F,R,wF)","(F,L,F,F,F,wF)"),("(F,L,F,F,L,wF)","(F,L,F,F,wF)"),("(F,L,F,L,F)","(F,L,F,F)"),("(F,L,w(F,L))","(F,L,wF)"),("(F,L,w(F,L,F))","(F,L,w(F,L))"),("(F,R)","F"),("(F,w(F,L))","w(F,L)"),("(F,w(F,L))","wF"),("(F,w(F,L))","wL"),("(F,w(L,F))","(F,wF)"),("(F,w(L,F))","(F,wL)"),("(F,wL)","(F,wE)"),("E","wE"),("F","E"),("w(F,F,L)","w(F,L)"),("w(F,F,L,F)","w(F,L,F)"),("w(F,L)","wF"),("w(F,L,F)","w(F,L)"),("w(F,L,F,F)","w(F,L,F)"),("w(F,L,F,F,F)","w(F,L,F,F)"),("w(F,L,F,F,F,F)","w(F,L,F,F,F)"),("w(F,L,F,F,F,L)","w(F,L,F,F,F)"),("w(F,L,F,F,F,L,F)","w(F,L,F,F,F,L)"),("w(F,L,F,F,F,L,F,F,F)","w(F,L,F,F,F,L,F,F)"),("w(F,L,F,F,F,L,F,F,F,F)","(F,L,F,F,F,L,F,F,F)"),("w(F,L,F,F,F,L,F,F,F,F,F)","w(F,L,F,F,F,L,F,F,F,F)"),("w(F,L,F,F,F,L,F,F,F,F,F,F)","w(F,L,F,F,F,L,F,F,F,F,F)"),("w(F,L,F,F,L)","w(F,L,F,F)"),("w(F,L,F,L)","w(F,L,F)"),("w(F,L,F,L,F)","(F,L,F,L)"),("w(F,L,F,R)","w(F,L,F)"),("w(F,L,L)","w(F,L)"),("w(F,L,R)","w(F,L)"),("w(F,R)","wF"),("w(F,R,F)","w(F,R)"),("w(F,R,L)","w(F,L)"),("w(L,F)","wF"),("w(L,F)","wL"),("wF","wE"),("wL","wE"),("wR","wE")]


hoc18NoS :: [(String,String)]
hoc18NoS = [("E","wE"),("F","E"),("aFF","waFF"),("aFL","waFL"),("aFR","aFL"),("aFR","waFR"),("aFwF","aFwE"),("aFwL","aFwE"),("aFwR","aFwE"),("aLF","aEF"),("aLF","waLF"),("aLL","waLL"),("aLR","aER"),("aLwF","aLwE"),("aRF","aEF"),("aRL","waRL"),("awFL","aFL"),("awFR","awFL"),("awFR","waFR"),("awLF","awEF"),("lFF","aFF"),("lFL","aFL"),("lFR","aFR"),("lFwL","aFwL"),("lLF","wlLF"),("lLwF","aLwF"),("lLwF","lLwE"),("lwFL","awFL"),("lwLF","awLF"),("wF","wE"),("wL","wE"),("wR","wE"),("waFF","waFE"),("waFR","waFL"),("waLF","waEF"),("waLL","waEL"),("waLR","waEF"),("waRF","waEF"),("waRF","waLF"),("waRL","waEL"),("waRR","waER"),("wlFF","waFF"),("wlFL","waFL"),("wlFR","waFR"),("wlLF","waLF"),("wlLL","waLL"),("wlLR","waLR"),("wlRF","wlLF"),("wlRL","waRL"),("wlRL","wlLL"),("wrFF","waFF"),("wrFL","waFL"),("wrFR","waFR"),("wrLF","waLF"),("wrRF","waRF"),("wrRL","waRL")]

hoc18GT :: [String]
hoc18GT = ["1 4","2 1","3 0","5 0","6 4","7 0","8 18","10 28","11 8","12 0","14 20","16 10","17 4","19 3","20 4","21 3","22 27","24 13","25 5","25 7","26 3","26 7","27 6","29 39","30 17","31 57","33 5","33 493","36 9","37 34","38 2","39 35","40 2","41 16","43 1","44 6","45 111","46 5","47 7","48 0","49 35","50 37","51 18","52 85","54 1","56 4","59 14","60 5","61 20","62 2","64 9","66 9","67 138","69 26","70 28","71 26","73 5","74 41","76 116","77 31","78 0","79 10","80 6","81 0","82 55","83 9","85 145","86 46","87 118","91 81","92 35","93 60","94 27","95 7","96 10","97 87","98 0","101 233","102 75","103 342","104 444","106 7","107 88","108 163","108 6","110 2","111 35","113 3","114 31","115 67","116 120","117 92","118 249","119 112","119 520","120 50","121 3364","122 63","123 0","123 3","124 58","125 8","126 28","126 135","127 98","128 13","133 76","134 83","137 273","138 280","138 46","139 70","140 33","141 74","142 16","143 18","144 23","145 249","146 432","147 215","150 8","150 13","151 5593","152 25","153 429","153 117","154 11","155 43","156 18","157 11","158 2","159 3","161 14","161 353","162 32","163 4","165 21","166 176","167 196","168 128","171 8","172 297","173 141","174 16","175 328","176 49","177 480","178 36","179 2","180 0","181 1724","182 7","182 33","183 313","184 38","185 25","187 0","188 32","189 74","191 37","192 76","193 0","194 55","195 22","196 3","197 1263","199 127","200 26","201 11","203 1981","204 92","205 100","207 390","209 16","210 19","211 6","211 49","212 123","213 30","214 57","216 77","218 57","220 42","222 37","223 138","225 10","226 0","227 1","228 2","229 13","230 32","231 2","232 7","235 31","237 7","238 10","240 53","241 5","242 42","244 35","245 26","246 486","248 46","249 482","250 124","252 7","253 123","254 38","255 35","258 114","259 32","262 48","263 2","264 936","265 21","267 35","268 116","269 82","269 42","271 27","275 27","276 3","276 3","278 253","279 69","280 55","281 68","286 98","287 666","289 2","290 24","291 5","292 10","292 28","292 135","293 41","294 21","296 9","298 22","300 0"]

singleEditsL :: [String]
singleEditsL = ["flfr","flff","flrf","ffrf","lfrf","flf","flr","ffr","lfr","fl","ff","lf","f","l","","fflfrf","flffrf","flfrff","rflfrf","frlfrf","flrfrf","flfrrf","flfrfr","lflfrf","fllfrf","flflrf","flfrlf","flfrfl","fflfr","flffr","flfrf","rflfr","frlfr","flrfr","flfrr","lflfr","fllfr","flflr","flfrl","fflf","rflf","frlf","lflf","fllf","flfl","ffl","rfl","frl","lfl","fll","rf","fr","fffrf","frfrf","flfff","flflf","flrrf","fllrf","rlfrf","llfrf","fffr","frfr","flrr","fllr","rlfr","llfr","fff","frf","rlf","llf","rl","ll","r"]

singleEditsFilteredL :: [String]
singleEditsFilteredL = ["flff","flrf","ffrf","lfrf","flr","ffr","lfr","ff","lf","l","","fflfrf","flffrf","flfrff","rflfrf","frlfrf","flrfrf","flfrrf","flfrfr","lflfrf","fllfrf","flflrf","flfrlf","flfrfl","fflfr","flffr","rflfr","frlfr","flrfr","flfrr","lflfr","fllfr","flflr","flfrl","fflf","rflf","frlf","lflf","fllf","flfl","ffl","rfl","frl","lfl","fll","rf","fr","fffrf","frfrf","flfff","flflf","flrrf","fllrf","rlfrf","llfrf","fffr","frfr","flrr","fllr","rlfr","llfr","fff","frf","rlf","llf","rl","ll","r"]

singleEditsSegL :: [String]
singleEditsSegL = ["","f","l","fl","ff","lf","flf","r","fr","lr","flr","ffr","lfr","flfr","fff","lff","flff","rf","frf","lrf","flrf","ffrf","lfrf","ll","rl","rr","ffl","lfl","fll","rfl","frl","llf","rlf","rfr","frr","rff","rrf","fflf","lflf","fllf","flfl","rflf","frlf","lffr","llfr","lflr","lfrl","rlfr","lrfr","lfrr","frff","frfl","rfrf","frrf","frfr","fflfr","flffr","flfrf","lflfr","fllfr","flflr","flfrl","rflfr","frlfr","flrfr","flfrr","lffrf","lfrff","llfrf","lflrf","lfrlf","lfrfl","rlfrf","lrfrf","lfrrf","lfrfr","fflfrf","flffrf","flfrff","lflfrf","fllfrf","flflrf","flfrlf","flfrfl","rflfrf","frlfrf","flrfrf","flfrrf","flfrfr","llr","lrr","fffr","fllr","flrr","lfff","llrf","lrrf","fffrf","flfff","flflf","fllrf","flrrf","frfrf"]

-- Does not contain segmentsOf (["","f","l","fl","lf","flf","r","fr","lfr","flfr","rf","frf","lfrf","flfrf"])
singleEditsSegFilteredL :: [String]
singleEditsSegFilteredL = ["ff","lr","flr","ffr","fff","lff","flff","lrf","flrf","ffrf","ll","rl","rr","ffl","lfl","fll","rfl","frl","llf","rlf","rfr","frr","rff","rrf","fflf","lflf","fllf","flfl","rflf","frlf","lffr","llfr","lflr","lfrl","rlfr","lrfr","lfrr","frff","frfl","rfrf","frrf","frfr","fflfr","flffr","lflfr","fllfr","flflr","flfrl","rflfr","frlfr","flrfr","flfrr","lffrf","lfrff","llfrf","lflrf","lfrlf","lfrfl","rlfrf","lrfrf","lfrrf","lfrfr","fflfrf","flffrf","flfrff","lflfrf","fllfrf","flflrf","flfrlf","flfrfl","rflfrf","frlfrf","flrfrf","flfrrf","flfrfr","llr","lrr","fffr","fllr","flrr","lfff","llrf","lrrf","fffrf","flfff","flflf","fllrf","flrrf","frfrf"]

gshintL :: String
gshintL = "flf flfr\nflfr flfrf\nflrf flfrf\nflff flf\nflff flfrf\nflr flfr\nflr fl\nfflf flf\nfflfrf flfrf\nfl flf\nf fl\nfrf flf\nfflrf flrf\nflflf flfrf\nffl fl\nffrf flfrf\nffrf frf\nflfl flfr\nffr fr\nffr flfr\nfrflf flflf\nfrlf flf\nfflfr flfr\nfr fl\nff f\nff flf\nfflr flr\nflffr flfr\nfrl fl\nfflff flff\nfrfrf flfrf\nfllf flf\nfrff flff\nflffrf flfrf\nflfrff flfrf\nfrfl flfl\nflfff flff\nfrfr flfr\nflfrr flfr\nfll fl\nfff ff\nfff flff\nfrr flr\nfrrf flrf\nffrl frl\nl fl\nl \nffrff frff\n f\nffrlf frlf\nfffr ffr\nfffr flffr\nflfrl flfr\nfffl ffl\nfllr fllrf\nfllr flr\nfllr fll\nfrlff flff\nlfrf flfrf\nrf f\nfllrf fllrfr\nfllrf flrf\nlff ff\nffrfl frfl\nflrr flr\nlfrff frff\nflrfr flfr\nlfr flfr\nflrlf flrlfr\nflflr flfr\nfllff flff\nr \nflrl flrlf\nflrl flr\nflffl flfl\nffrflf frflf\nffrr frr\nrlf lf\nflrrf flrf\nrfr fr\nrflf flf\nffrfr frfr\nffllf fllf\nffll fll\nfrfff flfff\nflfrrf flfrf\nfrlr frl\nrff ff\nfflfl flfl\nfffrf ffrf\nffff fff\nfflrff flrff\nffrfrf frfrf\nfrlrf flrf\nfflflf flflf\nffflf fflf\nfflffrf flffrf\nflrfl flfl\nrfl fl\nfllfr flfr\nlr flr\nffrrf frrf\nflrlfrf flrfrf\nfrrff flrff\nffflr fflr\nfllrr fllr\nfrlfr flfr\nfrrl flrl\nflfflf flflf\nflflrf flfrf\nlfff flfff\nflrlr flrl\nfflfrff flfrff\nfflffr flffr\nflfll flfrl\nfrffr flffr\nfrffl flffl\nlrff lrf\nfllrfrf fllfrf\nfllrfrf flrfrf\nflfrlf flfrf\nfrll fll\nrfrf frf\nflrlfr flrlfrf\nfffff ffff\nfllfrf flfrf\nflrlff flrlf\nffllr fllr\nflffrff flfrff\nflrfrf flfrf\nflrfff flfff\nfffrl ffrl\nlfl fl\nlffr flffr\nlflf flflf\nfflrr flrr\nrlff lff\nrlff rff\nffffl fffl\nffffr fffr\nfllrfr fllrfrf\nrflff flff\nrl frl\nfrrr frrrf\nflfrfr flfrf\nll fll\nflflff flfrff\nflflfr flfrfr\nrfff fff\nfrllfr frllfrf\nfrlfl flfl\nllf lf\nfrflff flflff\nfrlfrf flfrf\nfllrff fllrf\nflfffr flffr\nrflfrf flfrf\nfrfflf flfflf\nflfrfff flfrff\nfrflr flflr\nflfrfl flfrf\nrffl ffl\nrffr ffr\nfrrrfr frrrfrf\nflrrl flrl\nlrfff lrff\nlffff ffff\nffrlr frlr\nflrflf flflf\nffrrl frrl\nffrlff frlff\nfllfl flfl\nfflrl flrl\nflfrrl flfrrlf\nlrflf lrflfr\nffflrf fflrf\nfffrr ffrr\nfrllr frll\nflrlfl flrlfr\nflffff flfff\nlrfl flrfl\nflfrlr flfrlrf\nfrffrf flffrf\nfrrrflf frrrfrf\nrfrff frff\nfrfrl flfrl\nflrflr flflr\nfrllff frllf\nffllff fllff\nflfrfrf flfrfr\nfrrrfl frrrfr\nlrfr lrfl\nflrll flrl\nfrrrr frrr\nffllrf fllrf\nlrr flrr\nfrfrr flfrr\nflflrr flflrrf\nfflfff flfff\nffrffl frffl\nfrflfr flflfr\nflflfrf flfrfrf\nrflfr flfr\nrlr frlr\nflflfl flfrfl\nfflffrff flffrff\nlffl ffl\nrffrf ffrf\nffflff fflff\nffllrr fllrr\nffrrr frrr\nlrflfr lrflfrf\nflrrff flrff\nfffll ffll\nfrrflf frrrflf\nflrffr flffr\nrrl frrl\nfrlrr flrr\nlflff flff\nllff lff\nffflfrf fflfrf\nrfrl frl\nfllrfl fllrfr\nrfflf fflf\nflffrr flfrr\nfflfrr flfrr\nfrfrff flfrff\nfrrrff frrrf\nfllll flll\nfrllrf frllf\nffrll frll\nlrlf rlf\nfrrlr frrl\nrrff rff\nrlflfrf lflfrf\nlll flll\nfrfll flfll\nflfllr flflr\nrffff ffff\n"

gsHintIn :: [String]
gsHintIn = ["flf","flfr","flrf","flff","flff","flr","flr","fflf","fflfrf","fl","f","frf","fflrf","flflf","ffl","ffrf","ffrf","flfl","ffr","ffr","frflf","frlf","fflfr","fr","ff","ff","fflr","flffr","frl","fflff","frfrf","fllf","frff","flffrf","flfrff","frfl","flfff","frfr","flfrr","fll","fff","fff","frr","frrf","ffrl","l","l","ffrff","f","ffrlf","fffr","fffr","flfrl","fffl","fllr","fllr","fllr","frlff","lfrf","rf","fllrf","fllrf","lff","ffrfl","flrr","lfrff","flrfr","lfr","flrlf","flflr","fllff","r","flrl","flrl","flffl","ffrflf","ffrr","rlf","flrrf","rfr","rflf","ffrfr","ffllf","ffll","frfff","flfrrf","frlr","rff","fflfl","fffrf","ffff","fflrff","ffrfrf","frlrf","fflflf","ffflf","fflffrf","flrfl","rfl","fllfr","lr","ffrrf","flrlfrf","frrff","ffflr","fllrr","frlfr","frrl","flfflf","flflrf","lfff","flrlr","fflfrff","fflffr","flfll","frffr","frffl","lrff","fllrfrf","fllrfrf","flfrlf","frll","rfrf","flrlfr","fffff","fllfrf","flrlff","ffllr","flffrff","flrfrf","flrfff","fffrl","lfl","lffr","lflf","fflrr","rlff","rlff","ffffl","ffffr","fllrfr","rflff","rl","frrr","flfrfr","ll","flflff","flflfr","rfff","frllfr","frlfl","llf","frflff","frlfrf","fllrff","flfffr","rflfrf","frfflf","flfrfff","frflr","flfrfl","rffl","rffr","frrrfr","flrrl","lrfff","lffff","ffrlr","flrflf","ffrrl","ffrlff","fllfl","fflrl","flfrrl","lrflf","ffflrf","fffrr","frllr","flrlfl","flffff","lrfl","flfrlr","frffrf","frrrflf","rfrff","frfrl","flrflr","frllff","ffllff","flfrfrf","frrrfl","lrfr","flrll","frrrr","ffllrf","lrr","frfrr","flflrr","fflfff","ffrffl","frflfr","flflfrf","rflfr","rlr","flflfl","fflffrff","lffl","rffrf","ffflff","ffllrr","ffrrr","lrflfr","flrrff","fffll","frrflf","flrffr","rrl","frlrr","lflff","llff","ffflfrf","rfrl","fllrfl","rfflf","flffrr","fflfrr","frfrff","frrrff","fllll","frllrf","ffrll","lrlf","frrlr","rrff","rlflfrf","lll","frfll","flfllr","rffff"]

fffL :: String
fffL = "flff flf\nflff flfrf\nff f\nff flf\nfflff flff\nfrff flff\nflfrff flfrf\nflfff flff\nfff ff\nfff flff\nffrff frff\nfrlff flff\nlff ff\nlfrff frff\nfllff flff\nfrfff flfff\nrff ff\nffff fff\nfflrff flrff\nfrrff flrff\nlfff flfff\nfflfrff flfrff\nlrff lrf\nfffff ffff\nflrlff flrlf\nflffrff flfrff\nflrfff flfff\nrlff lff\nrlff rff\nrflff flff\nflflff flfrff\nrfff fff\nfrflff flflff\nfllrff fllrf\nflfrfff flfrff\nlrfff lrff\nlffff ffff\nffrlff frlff\nflffff flfff\nrfrff frff\nfrllff frllf\nffllff fllff\nfflfff flfff\nfflffrff flffrff\nffflff fflff\nflrrff flrff\nlflff flff\nllff lff\nfrfrff flfrff\nfrrrff frrrf\nrrff rff\nrffff ffff"


tmpIDsL :: [(String,String)]
tmpIDsL = [("453","114"),("451","72"),("447","150"),("446","195"),("444","502"),("442","109"),("441","138"),("437","84"),("436","162"),("434","155"),("433","195"),("430","182"),("428","36"),("425","40"),("423","40"),("422","6"),("417","206"),("416","28"),("412","7"),("411","63"),("405","4"),("404","66"),("398","138"),("397","27"),("396","318"),("395","99"),("392","16"),("390","278"),("389","213"),("380","136"),("379","29"),("378","17"),("377","15"),("374","181"),("373","261"),("364","108"),("362","2"),("359","333"),("356","225"),("351","154"),("350","38"),("349","324"),("348","40"),("346","66"),("342","62"),("341","213"),("339","80"),("337","315"),("336","270"),("333","216"),("329","73"),("327","155"),("326","72"),("325","55"),("320","32"),("318","156"),("317","33"),("316","381"),("315","123"),("308","38"),("307","169"),("305","162"),("304","83"),("303","12"),("295","390"),("293","415"),("292","80"),("290","18"),("289","59"),("286","138"),("285","13"),("284","108"),("283","114"),("280","158"),("277","80"),("270","156"),("268","19"),("266","15"),("261","0"),("259","72"),("254","36"),("251","143"),("238","0"),("237","27"),("235","62"),("234","0"),("233","220"),("230","20"),("229","18"),("228","185"),("226","44"),("225","216"),("220","36"),("219","42"),("216","0"),("213","182"),("211","28"),("209","4"),("206","160"),("205","54"),("204","56"),("201","109"),("201","63"),("200","66"),("196","13"),("192","27"),("191","9"),("188","47"),("186","38"),("183","0"),("181","36"),("176","57"),("175","70"),("173","0"),("172","114"),("169","133"),("166","11"),("162","42"),("161","0"),("160","183"),("160","173"),("158","74"),("154","81"),("151","27"),("150","55"),("149","27"),("148","36"),("147","80"),("146","38"),("144","0"),("143","13"),("138","80"),("137","2"),("136","57"),("135","26"),("134","16"),("133","183"),("129","46"),("128","5"),("126","2"),("124","9"),("123","18"),("121","33"),("120","6"),("119","13"),("118","3"),("117","30"),("116","16"),("114","44"),("113","17"),("112","18"),("109","25"),("108","28"),("105","0"),("101","38"),("99","42"),("92","31"),("90","39"),("89","1"),("87","24"),("85","3"),("84","20"),("83","45"),("82","21"),("81","18"),("80","5"),("80","70"),("78","51"),("73","4"),("72","2"),("70","169"),("69","2"),("68","2"),("67","32"),("66","5"),("64","37"),("63","25"),("62","3"),("62","206"),("61","10"),("60","0"),("59","4"),("57","42"),("57","5"),("57","62"),("56","15"),("55","2"),("54","27"),("54","19"),("52","22"),("51","10"),("50","32"),("49","51"),("49","9"),("47","28"),("46","3"),("45","5"),("44","4"),("44","25"),("42","9"),("40","2"),("39","2"),("38","4"),("37","18"),("36","0"),("33","0"),("32","4"),("31","1"),("30","0"),("29","4"),("28","9"),("27","2"),("26","5"),("25","1"),("25","10"),("24","9"),("23","2"),("22","1"),("21","13"),("19","2"),("19","24"),("18","2"),("17","11"),("17","0"),("15","9"),("13","0"),("12","3"),("11","1"),("10","9"),("9","1"),("7","0"),("6","1"),("5","9"),("5","2"),("4","0"),("4","1"),("3","0"),("2","0"),("1","2")]


analysisResults = [(True,224,("(F,F)","F"),"F"),(True,224,("(F,L)","F"),"F"),(True,224,("(F,L,F)","(F,L)"),"(F,L)"),(True,224,("(F,L,F,F)","(F,L,F)"),"(F,L,F)"),(True,224,("(F,L,F,F,F)","(F,L,F,F)"),"(F,L,F,F)"),(True,224,("(F,L,F,F,F,F)","(F,L,F,F,F)"),"(F,L,F,F,F)"),(True,224,("(F,L,F,F,F,F,L,wF)","(F,L,F,F,F,L,wF)"),"(F,L,F,F,F,L,wF)"),(True,224,("(F,L,F,F,F,L)","(F,L,F,F,F)"),"(F,L,F,F,F)"),(True,224,("(F,L,F,F,F,L,F,F,F)","(F,L,F,F,F,L,F,F)"),"(F,L,F,F,F,L,F,F)"),(True,224,("(F,L,F,F,F,L,F,F,F,F)","(F,L,F,F,F,L,F,F,F)"),"(F,L,F,F,F,L,F,F,F)"),(True,224,("(F,L,F,F,F,L,F,F,F,F,F)","(F,L,F,F,F,L,F,F,F,F)"),"(F,L,F,F,F,L,F,F,F,F)"),(True,224,("(F,L,F,F,F,L,F,F,F,F,F,F)","(F,L,F,F,F,L,F,F,F,F,F)"),"(F,L,F,F,F,L,F,F,F,F,F)"),(True,31,("(F,L,F,F,F,L,aFF)","(F,L,F,F,F,L,aFE)"),"(F,L,F,F,F,L,aFE)"),(True,0,("(F,L,F,F,F,L,aFF,wF)","(F,L,F,F,F,L,aFF,wE)"),"(F,L,F,F,F,L,aFF,wE)"),(False,25,("(F,L,F,F,F,L,wF)","(F,L,F,F,F,L,wE)"),"(F,L,F,F,F,wF)"),(True,25,("(F,L,F,F,F,L,waFF)","(F,L,F,F,F,waFF)"),"(F,L,F,F,F,waFF)"),(True,25,("(F,L,F,F,F,R,wF)","(F,L,F,F,F,wF)"),"(F,L,F,F,F,wF)"),(True,25,("(F,L,F,F,L,wF)","(F,L,F,F,wF)"),"(F,L,F,F,wF)"),(False,224,("(F,L,F,L,F)","(F,L,F,F)"),"(F,L,F,L)"),(True,31,("(F,L,aFF)","(F,L,aFE)"),"(F,L,aFE)"),(True,224,("(F,L,aFL)","(F,aFL)"),"(F,aFL)"),(True,0,("(F,L,aFL,wF)","(F,L,aFL,wE)"),"(F,L,aFL,wE)"),(True,25,("(F,L,aLF)","(F,aLF)"),"(F,aLF)"),(True,20,("(F,L,w(F,L))","(F,L,wF)"),"(F,L,wF)"),(True,20,("(F,L,w(F,L,F))","(F,L,w(F,L))"),"(F,L,w(F,L))"),(True,221,("(F,L,w(F,aFL))","(F,L,waFL)"),"(F,L,waFL)"),(False,115,("(F,L,w(F,aLF))","(F,L,waLF)"),"(F,L,w(F,waLF))"),(False,221,("(F,L,w(F,lLF))","(F,w(F,lLF))"),"(F,L,wlLF)"),(True,31,("(F,L,waFF)","(F,L,waFE)"),"(F,L,waFE)"),(True,224,("(F,L,waFL)","(F,waFL)"),"(F,waFL)"),(True,31,("(F,L,waLF)","(F,L,waEF)"),"(F,L,waEF)"),(True,224,("(F,R)","F"),"F"),(True,221,("(F,aFL)","aFL"),"aFL"),(True,0,("(F,aFL,wF)","(F,aFL,wE)"),"(F,aFL,wE)"),(True,107,("(F,aFR)","(F,aFL)"),"(F,aFL)"),(False,115,("(F,aLF)","aLF"),"(F,waLF)"),(False,0,("(F,aLF,wF)","(F,aLE,wF)"),"(F,aLF,wE)"),(True,115,("(F,aLL)","(F,waLL)"),"(F,waLL)"),(False,115,("(F,aLR)","(F,aLL)"),"(F,waLR)"),(True,115,("(F,aRF)","(F,waRF)"),"(F,waRF)"),(True,115,("(F,aRL)","(F,waRL)"),"(F,waRL)"),(True,107,("(F,l(L,F)F)","(F,lLF)"),"(F,lLF)"),(True,224,("(F,lFF)","lFF"),"lFF"),(True,224,("(F,lFL)","lFL"),"lFL"),(True,221,("(F,lLF)","lLF"),"lLF"),(True,221,("(F,lLF,F)","(F,lLF)"),"(F,lLF)"),(False,103,("(F,lLF,F,F,F,L,wF)","(lLF,F,F,F,L,wF)"),"(F,aLF,F,F,F,L,wF)"),(True,103,("(F,lLF,wF)","(F,aLF,wF)"),"(F,aLF,wF)"),(True,103,("(F,lLL)","(F,aLL)"),"(F,aLL)"),(True,103,("(F,lLR)","(F,aLR)"),"(F,aLR)"),(False,20,("(F,w(F,L))","w(F,L)"),"(F,wF)"),(True,20,("(F,w(L,F))","(F,wL)"),"(F,wL)"),(True,3,("(F,wL)","(F,wE)"),"(F,wE)"),(True,221,("(F,waFL)","waFL"),"waFL"),(False,31,("(F,waLF)","(F,waLE)"),"(F,waEF)"),(False,33,("(F,waLL)","waLL"),"(F,waEL)"),(True,32,("(F,waLR)","(F,waER)"),"(F,waER)"),(True,33,("(F,waRL)","(F,waEL)"),"(F,waEL)"),(True,99,("(F,wlFL)","(F,waFL)"),"(F,waFL)"),(True,221,("(F,wlLF)","wlLF"),"wlLF"),(False,224,("(aFL,F)","w(aFL,F)"),"aFL"),(True,224,("(aFL,L)","aFL"),"aFL"),(True,20,("(aFL,w(F,L))","(aFL,wF)"),"(aFL,wF)"),(False,20,("(aFL,w(L,F))","(aFL,wF)"),"(aFL,wL)"),(True,0,("(aFL,wF)","(aFL,wE)"),"(aFL,wE)"),(True,3,("(aFL,wL)","(aFL,wE)"),"(aFL,wE)"),(True,3,("(aFL,wR)","(aFL,wE)"),"(aFL,wE)"),(True,107,("(aFR,wF)","(aFL,wF)"),"(aFL,wF)"),(True,0,("(aLF,wF)","(aLF,wE)"),"(aLF,wE)"),(True,0,("(aLR,wF)","(aLR,wE)"),"(aLR,wE)"),(True,103,("(lFL,wF)","(aFL,wF)"),"(aFL,wF)"),(True,224,("(lLF,F)","lLF"),"lLF"),(True,103,("(lLF,wF)","(aLF,wF)"),"(aLF,wF)"),(True,30,("E","wE"),"wE"),(True,31,("F","E"),"E"),(True,107,("a(F,L)L","aFL"),"aFL"),(True,24,("a(F,L)R","aFR"),"aFR"),(True,24,("a(F,L)wF","aFwF"),"aFwF"),(True,24,("a(L,F)F","aFF"),"aFF"),(True,107,("aF(L,F)","aFL"),"aFL"),(True,11,("aFF","waFF"),"waFF"),(True,11,("aFL","waFL"),"waFL"),(True,107,("aFR","aFL"),"aFL"),(True,31,("aFwF","aFwE"),"aFwE"),(True,3,("aFwL","aFwE"),"aFwE"),(True,3,("aFwR","aFwE"),"aFwE"),(True,11,("aLF","waLF"),"waLF"),(True,11,("aLL","waLL"),"waLL"),(False,11,("aLR","aER"),"waLR"),(True,31,("aLwF","aLwE"),"aLwE"),(False,11,("aRF","aEF"),"waRF"),(True,11,("aRL","waRL"),"waRL"),(False,20,("aw(F,L)F","a(F,L)F"),"awFF"),(True,16,("awFL","aFL"),"aFL"),(True,32,("awFR","awFL"),"awFL"),(True,3,("awLF","awEF"),"awEF"),(True,107,("l(F,L)F","lLF"),"lLF"),(True,107,("l(L,F)F","lLF"),"lLF"),(True,103,("lFF","aFF"),"aFF"),(True,103,("lFL","aFL"),"aFL"),(True,103,("lFR","aFR"),"aFR"),(True,13,("lFwL","aFwL"),"aFwL"),(True,102,("lLF","wlLF"),"wlLF"),(True,13,("lLwF","aLwF"),"aLwF"),(True,13,("lwFL","awFL"),"awFL"),(True,13,("lwLF","awLF"),"awLF"),(True,20,("w(F,F,L)","w(F,L)"),"w(F,L)"),(True,20,("w(F,F,L,F)","w(F,L,F)"),"w(F,L,F)"),(True,25,("w(F,F,lLF)","w(F,lLF)"),"w(F,lLF)"),(True,20,("w(F,L)","wF"),"wF"),(True,20,("w(F,L,F)","w(F,L)"),"w(F,L)"),(True,20,("w(F,L,F,F)","w(F,L,F)"),"w(F,L,F)"),(True,20,("w(F,L,F,F,F)","w(F,L,F,F)"),"w(F,L,F,F)"),(True,20,("w(F,L,F,F,F,F)","w(F,L,F,F,F)"),"w(F,L,F,F,F)"),(True,20,("w(F,L,F,F,F,L)","w(F,L,F,F,F)"),"w(F,L,F,F,F)"),(True,20,("w(F,L,F,F,F,L,F)","w(F,L,F,F,F,L)"),"w(F,L,F,F,F,L)"),(True,20,("w(F,L,F,F,F,L,F,F,F)","w(F,L,F,F,F,L,F,F)"),"w(F,L,F,F,F,L,F,F)"),(True,20,("w(F,L,F,F,F,L,F,F,F,F,F)","w(F,L,F,F,F,L,F,F,F,F)"),"w(F,L,F,F,F,L,F,F,F,F)"),(True,20,("w(F,L,F,F,F,L,F,F,F,F,F,F)","w(F,L,F,F,F,L,F,F,F,F,F)"),"w(F,L,F,F,F,L,F,F,F,F,F)"),(True,25,("w(F,L,F,F,F,L,aFF)","w(F,L,F,F,F,aFF)"),"w(F,L,F,F,F,aFF)"),(True,20,("w(F,L,F,F,L)","w(F,L,F,F)"),"w(F,L,F,F)"),(True,25,("w(F,L,F,F,aFF)","w(F,L,F,aFF)"),"w(F,L,F,aFF)"),(True,20,("w(F,L,F,L)","w(F,L,F)"),"w(F,L,F)"),(True,20,("w(F,L,F,R)","w(F,L,F)"),"w(F,L,F)"),(True,25,("w(F,L,F,aFF)","w(F,L,aFF)"),"w(F,L,aFF)"),(True,25,("w(F,L,F,aFL)","w(F,L,aFL)"),"w(F,L,aFL)"),(True,25,("w(F,L,F,aFR)","w(F,L,aFR)"),"w(F,L,aFR)"),(True,25,("w(F,L,F,aLF)","w(F,L,aLF)"),"w(F,L,aLF)"),(True,25,("w(F,L,F,lFF)","w(F,L,lFF)"),"w(F,L,lFF)"),(True,20,("w(F,L,L)","w(F,L)"),"w(F,L)"),(True,20,("w(F,L,R)","w(F,L)"),"w(F,L)"),(True,24,("w(F,L,a(F,F)F)","w(F,L,aFF)"),"w(F,L,aFF)"),(True,107,("w(F,L,aF(L,F))","w(F,L,aFL)"),"w(F,L,aFL)"),(True,25,("w(F,L,aFF)","w(F,aFF)"),"w(F,aFF)"),(False,20,("w(F,L,aFF,F)","w(F,aFF,F)"),"w(F,L,aFF)"),(True,25,("w(F,L,aFL)","w(F,aFL)"),"w(F,aFL)"),(True,20,("w(F,L,aFL,F)","w(F,L,aFL)"),"w(F,L,aFL)"),(True,25,("w(F,L,aFR)","w(F,aFR)"),"w(F,aFR)"),(True,25,("w(F,L,aLF)","w(F,aLF)"),"w(F,aLF)"),(True,25,("w(F,L,aLR)","w(F,aLR)"),"w(F,aLR)"),(False,25,("w(F,L,aRF)","w(F,L,aEF)"),"w(F,aRF)"),(False,25,("w(F,L,lFF)","w(F,L,aFF)"),"w(F,lFF)"),(True,25,("w(F,L,lFL)","w(F,lFL)"),"w(F,lFL)"),(True,25,("w(F,L,lLF)","w(F,lLF)"),"w(F,lLF)"),(True,20,("w(F,R)","wF"),"wF"),(True,20,("w(F,R,F)","w(F,R)"),"w(F,R)"),(True,20,("w(F,R,L)","w(F,L)"),"w(F,L)"),(True,24,("w(F,a(F,L)F)","w(F,aFF)"),"w(F,aFF)"),(True,104,("w(F,a(L,F)(L,F))","w(F,aF(L,F))"),"w(F,aF(L,F))"),(False,104,("w(F,a(L,F)(R,F))","w(F,a(L,F)(L,F))"),"w(F,aF(R,F))"),(True,107,("w(F,a(L,F)L)","w(F,aFL)"),"w(F,aFL)"),(True,125,("w(F,a(L,F)R)","w(F,aFR)"),"w(F,aFR)"),(True,104,("w(F,a(R,F)(L,F))","w(F,aF(L,F))"),"w(F,aF(L,F))"),(True,107,("w(F,a(R,F)L)","w(F,aFL)"),"w(F,aFL)"),(True,107,("w(F,aF(L,F))","w(F,aFL)"),"w(F,aFL)"),(True,15,("w(F,aFF)","w(F,aFE)"),"w(F,aFE)"),(True,221,("w(F,aFL)","waFL"),"waFL"),(True,20,("w(F,aFL,F)","w(F,aFL)"),"w(F,aFL)"),(True,107,("w(F,aFR)","w(F,aFL)"),"w(F,aFL)"),(True,14,("w(F,aLF)","w(F,aEF)"),"w(F,aEF)"),(True,20,("w(F,aLF,F)","w(F,aLF)"),"w(F,aLF)"),(True,14,("w(F,aLL)","w(F,aEL)"),"w(F,aEL)"),(True,20,("w(F,aLL,F)","w(F,aLL)"),"w(F,aLL)"),(True,20,("w(F,aLL,F,F)","w(F,aLL,F)"),"w(F,aLL,F)"),(False,14,("w(F,aLR)","waLR"),"w(F,aER)"),(True,20,("w(F,aLR,F)","w(F,aLR)"),"w(F,aLR)"),(True,125,("w(F,aR(F,L))","w(F,aRL)"),"w(F,aRL)"),(True,14,("w(F,aRF)","w(F,aEF)"),"w(F,aEF)"),(True,14,("w(F,aRL)","w(F,aEL)"),"w(F,aEL)"),(True,20,("w(F,aRL,F)","w(F,aRL)"),"w(F,aRL)"),(True,20,("w(F,aRL,F,F)","w(F,aRL,F)"),"w(F,aRL,F)"),(False,14,("w(F,aRR)","w(F,aRL)"),"w(F,aER)"),(True,107,("w(F,l(F,L)F)","w(F,lLF)"),"w(F,lLF)"),(True,101,("w(F,l(L,F)(L,F))","w(F,l(L,F)F)"),"w(F,l(L,F)F)"),(True,101,("w(F,l(L,F)(R,F))","w(F,l(L,F)F)"),"w(F,l(L,F)F)"),(True,107,("w(F,l(L,F)F)","w(F,lLF)"),"w(F,lLF)"),(True,13,("w(F,l(L,F)L)","w(F,a(L,F)L)"),"w(F,a(L,F)L)"),(True,125,("w(F,l(L,F)R)","w(F,lLR)"),"w(F,lLR)"),(True,107,("w(F,l(L,R)F)","w(F,lLF)"),"w(F,lLF)"),(False,13,("w(F,lFF)","wlFF"),"w(F,aFF)"),(True,13,("w(F,lFL)","w(F,aFL)"),"w(F,aFL)"),(True,13,("w(F,lFR)","w(F,aFR)"),"w(F,aFR)"),(False,107,("w(F,lL(F,F))","w(F,aL(F,F))"),"w(F,lLF)"),(True,107,("w(F,lL(F,R))","w(F,lLF)"),"w(F,lLF)"),(True,107,("w(F,lL(R,F))","w(F,lLF)"),"w(F,lLF)"),(True,22,("w(F,lLF)","w(F,aLF)"),"w(F,aLF)"),(True,20,("w(F,lLF,F)","w(F,lLF)"),"w(F,lLF)"),(True,20,("w(F,lLF,R)","w(F,lLF)"),"w(F,lLF)"),(True,31,("w(F,lLF,aFF)","w(F,lLF,aFE)"),"w(F,lLF,aFE)"),(True,31,("w(F,lLF,aLF)","w(F,lLF,aEF)"),"w(F,lLF,aEF)"),(False,31,("w(F,lLF,lLF)","w(F,lLF,lEF)"),"w()"),(True,106,("w(F,lLF,rRF)","w(F,lLF,aRF)"),"w(F,lLF,aRF)"),(True,13,("w(F,lLL)","w(F,aLL)"),"w(F,aLL)"),(True,20,("w(F,lLL,F)","w(F,lLL)"),"w(F,lLL)"),(True,13,("w(F,lLR)","w(F,aLR)"),"w(F,aLR)"),(True,20,("w(F,lLR,F)","w(F,lLR)"),"w(F,lLR)"),(True,22,("w(F,lLwF)","w(F,aLwF)"),"w(F,aLwF)"),(True,107,("w(F,lRF)","w(F,lLF)"),"w(F,lLF)"),(True,13,("w(F,lRL)","w(F,aRL)"),"w(F,aRL)"),(False,13,("w(F,lRR)","w(F,lLR)"),"w(F,aRR)"),(True,106,("w(F,rFF)","w(F,aFF)"),"w(F,aFF)"),(True,106,("w(F,rFL)","w(F,aFL)"),"w(F,aFL)"),(True,106,("w(F,rFR)","w(F,aFR)"),"w(F,aFR)"),(True,106,("w(F,rLF)","w(F,aLF)"),"w(F,aLF)"),(True,106,("w(F,rLL)","w(F,aLL)"),"w(F,aLL)"),(True,106,("w(F,rLR)","w(F,aLR)"),"w(F,aLR)"),(True,106,("w(F,rRF)","w(F,aRF)"),"w(F,aRF)"),(True,106,("w(F,rRL)","w(F,aRL)"),"w(F,aRL)"),(True,106,("w(F,rRR)","w(F,aRR)"),"w(F,aRR)"),(True,20,("w(L,F)","wL"),"wL"),(True,25,("w(L,aFF)","waFF"),"waFF"),(True,25,("w(L,aFL)","waFL"),"waFL"),(True,25,("w(L,aFR)","waFR"),"waFR"),(True,20,("w(aFF,L)","waFF"),"waFF"),(True,20,("w(aFL,F)","waFL"),"waFL"),(True,20,("w(aFR,F)","waFR"),"waFR"),(True,20,("w(aFR,L)","waFR"),"waFR"),(True,20,("w(aLF,F)","waLF"),"waLF"),(True,20,("w(aLR,F)","waLR"),"waLR"),(False,20,("w(aRL,F)","w(aEL,F)"),"waRL"),(True,20,("w(lLF,F)","wlLF"),"wlLF"),(True,0,("wF","wE"),"wE"),(True,3,("wL","wE"),"wE"),(True,3,("wR","wE"),"wE"),(True,24,("wa(F,L)F","waFF"),"waFF"),(True,107,("wa(F,L)L","waFL"),"waFL"),(True,24,("wa(F,L)R","waFR"),"waFR"),(True,24,("wa(F,L,F)F","wa(F,L)F"),"wa(F,L)F"),(True,24,("wa(F,L,F,F)F","wa(F,L,F)F"),"wa(F,L,F)F"),(True,107,("wa(F,R)L","waFL"),"waFL"),(True,24,("wa(L,F)F","waFF"),"waFF"),(True,107,("waF(L,F)","waFL"),"waFL"),(True,107,("waF(L,R)","waFL"),"waFL"),(False,24,("waF(R,F)","waF(L,F)"),"waFR"),(True,107,("waF(R,L)","waFL"),"waFL"),(True,15,("waFF","waFE"),"waFE"),(True,107,("waFR","waFL"),"waFL"),(True,14,("waLF","waEF"),"waEF"),(True,14,("waLL","waEL"),"waEL"),(True,14,("waRF","waEF"),"waEF"),(True,14,("waRL","waEL"),"waEL"),(True,14,("waRR","waER"),"waER"),(True,99,("wl(F,L)F","wa(F,L)F"),"wa(F,L)F"),(True,99,("wl(L,F)F","wa(L,F)F"),"wa(L,F)F"),(True,99,("wl(L,F)R","wa(L,F)R"),"wa(L,F)R"),(True,99,("wlFF","waFF"),"waFF"),(True,99,("wlFL","waFL"),"waFL"),(True,99,("wlFR","waFR"),"waFR"),(True,99,("wlLF","waLF"),"waLF"),(True,99,("wlLL","waLL"),"waLL"),(True,99,("wlLR","waLR"),"waLR"),(True,98,("wlRF","wlLF"),"wlLF"),(True,99,("wlRL","waRL"),"waRL"),(True,99,("wrFF","waFF"),"waFF"),(True,99,("wrFL","waFL"),"waFL"),(True,99,("wrFR","waFR"),"waFR"),(True,99,("wrLF","waLF"),"waLF"),(True,99,("wrRF","waRF"),"waRF"),(True,99,("wrRL","waRL"),"waRL")]