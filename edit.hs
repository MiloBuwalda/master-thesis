module Edit where

import Auxiliary as Aux
import Data.List
import Data.List.Split

data Edit = Addition | Deletion | Substitution | Empty deriving (Eq, Show)

test1 = "(F,aFL)"
test2 = "awFL"

encExprTypes = "wFLR"
encCondTypes = "alr"
encTypes = "wFLRalr" -- while,F,L,R,ahead,pathleft,pathright

---------- transitions 
--

-- insertExpr :: a -> a
-- insertExpr a = 



-- expr
insertions :: [a] -> [a] -> [[a]]
insertions s xs = [ans | tmp <- xs, ans <- map (\x -> Aux.insertAt x tmp s) [0..(length s)]]


deletions :: String -> [String]
deletions iS = map (replaceWithE iS) encExprTypes

replaceWithE :: String -> Char -> String
replaceWithE s c = map (\x -> if x == c then 'e' else x) s


findDeletions :: String -> String -> [String]
findDeletions s = concatMap (findDeletion' s)
    where
    findDeletion' :: String -> Char -> [String]
    findDeletion' s = fD (length s) s
        where
            fD :: Int -> String -> Char -> [String] 
            fD 0 _ _ = []
            fD i s c = 
                if cur == c
                    then (remn++curs):fD (i-1) s c
                    else fD (i-1) s c
                where
                    (remn,cur:curs) = splitAt (i-1) s

-- find...        :: Input -> Options (flr) -> output
findSubstitutions :: String -> String -> [String]
findSubstitutions s o = 
    filter ((&&) <$> (\ x -> Aux.lev s x == 1) <*> checkF s) expanded
    where   expanded = mapM (const o) [1 .. (length s)]


-- -- calcLengthSubpartSol :: String -> Int
-- -- calcLengthSubpartSol s = length.filter (==True) $ map (`elem` Aux.subListL) (Aux.subList s)


-- -- isOnPath :: String  -> Bool
-- -- isOnPath input = elem input $ Aux.subListL ++ [""] 

-- -- -- ["f","l","r","fl","lf","fr","rf","flf","lfr","frf","flfr","lfrf",solution,""]
-- -- segmentsOf :: String -> [String]
-- -- segmentsOf s = Aux.ordNub (concatMap (\ x -> L.divvy x 1 s) [1 .. (length s)])
-- --     ++ [""]

-- -- isSegmentOf :: String -> Bool
-- -- isSegmentOf s = isSegmentOf' s solution 
-- --     where
-- --         isSegmentOf' :: String -> String -> Bool
-- --         isSegmentOf' s' sl' = s' `elem` segmentsOf sl'

-- -- find...        :: Input -> Options (flr) -> output
-- findSubstitutions :: String -> String -> [String]
-- findSubstitutions s o = 
--     filter ((&&) <$> (\ x -> Aux.lev s x == 1) <*> checkF s) expanded
--     where   expanded = mapM (const o) [1 .. (length s)]

-- Given inputString and a string to check against. see if all the f's are left alone and only non-f's (r,l) are changed.
checkF :: String -> String -> Bool -- "flf" "flr" False
checkF "" "" = True
checkF _  "" = False 
checkF "" _  = False 
checkF (x:xs) (y:ys) 
    | x == 'f' && x /= y = False  
    | y == 'f' && x /= y = False  
    | otherwise = checkF xs ys

-- -- findDeletions :: String -> [String]
-- -- findDeletions s = filter (\x -> length x==(length s-1)) $ subsequences s

-- -- | Filters on second input string "rl" -> only takes deletions with "rl"
-- -- findDeletions' :: String -> String -> [String]
-- -- findDeletions' s = filter (\x -> length x==(length s-1)) $ subsequences s


-- -- findDeletion' (x:xs) tmp y = 
-- --     if x == y 
-- --     then xs:findDeletion' (tmp:xs) x y   
-- --     else findDeletion' (tmp:xs) x y
-- -- findDeletions' (x:xs) y = xs 

-- -- thnx all subsequences, also of all segments
-- -- findDeletionsSeg :: String -> [String]
-- -- findDeletionsSeg s = init $ subsequences s

-- -- findInsertions :: input -> Expansions -> input+expansion set
-- findInsertions :: String -> String -> [String]
-- findInsertions s xs = [ans | tmp <- xs, ans <- map (\x -> Aux.insertAt x tmp s) [0..(length s)]] -- still needs: Aux.ordNub
  

-- -- getToNearestSubSolUsingSpecificEdit
-- nearestSubSol :: Edit -> String -> [String]
-- nearestSubSol Addition s = Aux.ordNub $ findInsertions s "flr"
-- nearestSubSol Deletion s = Aux.ordNub $ findDeletions s "flr"
-- nearestSubSol Substitution s = Aux.ordNub $ findSubstitutions s "flr"    
-- --
-- findSingleEdits :: String -> String -> [String]
-- findSingleEdits s o = Aux.ordNub $ dels ++ adds ++ subst
--     where
--         adds = findInsertions s o
--         dels = findDeletions s "flr"
--         subst = findSubstitutions s o

-- -- findAllSingleEdits :: input (includes sub strings flr=flr+fl+f)
-- -- findAllSingleEdits :: String -> String -> [String]
-- -- findAllSingleEdits s o = Aux.ordNub $ concat $[dels] ++ adds ++ subst
-- --     where
-- --         dels = concatMap (`findDeletions` "flr" ) $ Aux.subList s
-- --         adds = map (`findInsertions` o) $ Aux.subList s
-- --         subst = map (`findSubstitutions` o) $ Aux.subList s


-- -- findAllSingleEditsSeg :: String -> String -> [String]
-- -- findAllSingleEditsSeg s o = Aux.ordNub $ concat $[dels] ++ adds ++ subst
-- --     where
-- --         dels = findDeletionsSeg s
-- --         adds = map (`findInsertions` o) $ segmentsOf s
-- --         subst = map (`findSubstitutions` o) $ segmentsOf s

-- -- verifies if all the generated expressions are add,del or swap.
--     -- verify segmentsOf $ (findAllSingleEditsSeg solution "flr")
-- -- verify :: (String -> [String]) -> [String] -> (Int, [(String, Bool)])
-- -- verify x allEdits = (total, listVers2)
-- --     where   subs = x solution -- x = Aux.subList or segmentsOf
-- --             listVers = map (\x-> 1 `elem` map (Aux.lev x) subs) allEdits
-- --             listVers2 = zip allEdits listVers
-- --             total = length $ filter (==False) listVers 
-- -- = [ans|tmp <- Aux.subList solution, ans <- map (\x -> lev x tmp ) $ findAllSingleEdits solution "frl"]