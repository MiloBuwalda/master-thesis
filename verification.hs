-------------------------------------------------------------------------------                      Results and Evaluation
-----------------------------------------------------------------------------
module Verification (
    -- verifyStrategy
  ) where 

import Data.List as L
import Data.List.Split as L
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import qualified Data.Maybe as MB


import qualified Data.ByteString.Char8 as B

import qualified System.Directory as SD


import qualified Database as Db


import Strategy
import Expressions
import Auxiliary as Aux

type Result = (String,String,String,Strategy,Float,Strategy,Float,Bool)
type Results = [Result] 

outFile :: IO FilePath
outFile = do 
  currentDir <- SD.getCurrentDirectory
  return $ currentDir ++ "\\output\\"

type Input = String
type MyHint = String
type GSHint = String

type Result18 = (Input,MyHint,GSHint,Bool)
type Results18 = [Result18]

verify :: Results18
-- verify = [("","",False)]
verify =
  let gt = Db.hoc18Exprs
      -- gsHints = map (snd) gt
      myHints = map (hint . fst) gt 
  in
    zipWith comp gt myHints 

comp :: (Expr, Expr) -> Expr -> Result18
comp (inp, x) y = (encodeExpr inp, encodeExpr x, encodeExpr y, x == y)


writeRes :: Results18 -> String -> IO ()
writeRes xs infixS = do
    currentDir <- outFile
    outh <- openFile (currentDir ++ "hPA"++ infixS ++ ".hs") WriteMode
    sequence_ [hPutStrLn outh $ printLn x | x <- xs ]
    hPutStrLn outh ""
    hPutStrLn outh (show (totalFromFreq freqScore) ++ ", True: " ++ show (count True freqScore) ++ ", False: " ++ show (count False freqScore))
    -- sequence_ [hPutStrLn outh $ printFrequency x | x <- freqScore]
    hClose outh
    where printLn :: Result18 -> String
          printLn (a,b,c,d) = unwords [a,b,c,show d]
          freqScore = frequency xs

frequency :: Results18 -> [(Bool, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(b,1) | (_,_,_,b) <- xs])

totalFromFreq :: [(Bool, Int)] -> Int 
totalFromFreq = foldl (\ a (_,x) -> a + x) 0


-- printFrequency :: ((Strategy,Bool), Int) -> String
-- printFrequency ((x, y), z) = show x ++ " = " ++ show y ++ ", " ++ show z
    
-- showFrequency :: [((Strategy,Bool), Int)] -> IO ()
-- showFrequency xs = sequence_  $ putStrLn (show (totalFromFreq xs) ++
--     ", True: " ++
--        show (count True xs) ++ ", False: " ++ show (count False xs)) :
--     [putStrLn (show x ++ " = " ++ show y ++ ", " ++ show z) |
--     ((x, y), z) <- xs]

count :: Bool -> [(Bool, Int)] -> Int
count b =  foldl (\ a (bl, x) -> if bl==b then a + x else a+0) 0


removeDuplicates :: Results18 -> Results18
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates 
    (outp1@(inp1,out1,hnt1,eqOutHint1):
     outp2@(inp2,out2,hnt2,eqOutHint2):xs) =
      if inp1 == inp2 then
        if eqOutHint1 then removeDuplicates (outp1 : xs) else
          if eqOutHint2 then removeDuplicates (outp2 : xs) else
              outp1 : outp2 : removeDuplicates xs
        else outp1 : removeDuplicates (outp2:xs)

-- -- | Verify applyExpressionEvaluation against the hints given by expert
-- --   Take the fist in the list and apply .
-- verifyStrategy :: Results
-- verifyStrategy = verifyEvalStrategy1

-- verifyEvalStrategy1 :: Results
-- verifyEvalStrategy1 = 
--     Aux.zip8' 
--         gs 
--         myHint
--         getHintStrategies
--         (Aux.checkIfSame (map (fst.snd) myHint) (map snd gs) )
--     where gs :: [(String,String)]
--           gs = map (toTuple . L.splitOn " ") $ lines Aux.gshintL
--           -- appl = applyStrategy 
--           -- myHint = map (\x -> ((fst.appl) . fst x),((snd.appl) . fst x )) gs
--           myHint :: [(Strategy,Score)]
--           myHint = map f gs
--           f :: (String,String) -> (Strategy,Score)
--           f xs =  fromSS $ applyStrategy1 (fst xs)
--              -- map (\x->(x,appl x)) gs

-- removeDuplicates :: Results -> Results
-- removeDuplicates [] = []
-- removeDuplicates [x] = [x]
-- removeDuplicates 
--     (outp1@(inp1,out1,hnt1,strat1,score1,strat1b,score1b,eqOutHint1):
--     outp2@(inp2,out2,hnt2,strat2,score2,strat2b,score2b,eqOutHint2):xs) =
--         if inp1 == inp2 then
--             if eqOutHint1 then removeDuplicates (outp1 : xs) else
--                 if eqOutHint2 then removeDuplicates (outp2 : xs) else
--                     outp1 : outp2 : removeDuplicates xs
--             else outp1 : removeDuplicates (outp2:xs)

-- remdups :: [(String,Bool)] -> [(String,Bool)]
-- remdups [] = []
-- remdups [x] = [x] 
-- remdups (x@(x1,x2):y@(y1,y2):xs) = 
--     if x1 == y1 then
--         if x2 then remdups (x : xs) else
--             if y2 then remdups (y : xs) else x : y : remdups xs
--         else x : remdups (y:xs)  

-- frequency :: Results -> [((Strategy,Bool), Int)]
-- frequency xs = Map.toList (Map.fromListWith (+) [((s,b),1) | (_,_,_,s,_,_,_,b) <- xs])

-- filterResStrategy :: Strategy -> Results -> Results
-- filterResStrategy s = filter (\ (_, _, _, x, _,_,_, _) -> x == s)

-- filterResBool :: Bool -> Results -> Results
-- filterResBool b = filter (\ (_,_,_,_,_,_,_,x) -> x == b)

-- -- getResultScores :: Results -> [(Strategy,Float,Strategy,Float)]
-- -- getResultScores [] = []
-- -- getResultScores ((i,_,_,myStr,mySc,,x,_):xs) = (myStr,mySc,x,((snd.score).(`toScore` i)) x):getResultScores xs

-- -- writeRes ((removeDuplicates verifyEvalStrategy1)) "14"

-- writeRes :: Results -> String -> IO ()
-- writeRes xs infixS = do
--     currentDir <- outFile
--     outh <- openFile (currentDir ++ "hPA"++ infixS ++ ".hs") WriteMode
--     sequence_ [hPutStrLn outh $ printLn x | x <- xs ]
--     hPutStrLn outh ""
--     hPutStrLn outh (show (totalFromFreq freqScore) ++ ", True: " ++ show (count True freqScore) ++ ", False: " ++ show (count False freqScore))
--     sequence_ [hPutStrLn outh $ printFrequency x | x <- freqScore]
--     hClose outh
--     where printLn :: Result -> String
--           printLn (a,b,c,d,e,f,g,h) = unwords [a,b,c,show d,show e,show f, show g,show h]
--           freqScore = frequency xs


-- printRes :: Results -> IO ()
-- printRes xs = sequence_ [putStrLn $ printLn x | x <- xs ]
--     where printLn :: Result -> String
--           printLn (a,b,c,d,e,f,g,h) = unwords [a,b,c,show d,show e,show f,show g, show h]

-- printFrequency :: ((Strategy,Bool), Int) -> String
-- printFrequency ((x, y), z) = show x ++ " = " ++ show y ++ ", " ++ show z
    


-- showFrequency :: [((Strategy,Bool), Int)] -> IO ()
-- showFrequency xs = sequence_  $ putStrLn (show (totalFromFreq xs) ++
--     ", True: " ++
--        show (count True xs) ++ ", False: " ++ show (count False xs)) :
--     [putStrLn (show x ++ " = " ++ show y ++ ", " ++ show z) |
--     ((x, y), z) <- xs]

-- totalFromFreq :: [((Strategy,Bool), Int)] -> Int 
-- totalFromFreq = foldl (\ a (_, x) -> a + x) 0

-- count :: Bool -> [((Strategy,Bool), Int)] -> Int
-- count b =  foldl (\ a ((str,bl), x) -> if bl==b then a + x else a+0) 0



-- -- hintGenD1 :: list of single edits -> is the hint generated for the edit the correct solution? flfrf flfr flf fl f
-- hintGenD1 :: IO ()
-- hintGenD1 = do
--     -- read file
--     gt <- readFile (outFile ++ "GS.txt")
--     -- obtain Map from data 
--     -- let filteredEmptyStrings = map (map checkEmptyString) $ map (words) $ lines gt
--     let observationMap = Map.fromList $ map (Aux.toTuple . splitOn " ") $ lines gt

--     -- 1EditDistanceKeys 
--     let observedKeys = filter (`Map.member` observationMap) Aux.singleEditsFilteredL
--     -- Hints given to 1EditDistanceKeys
--     let foundValues = zip5  observedKeys 
--                             (lookupValues observedKeys observationMap) 
--                             (map isOnPath (lookupValues observedKeys observationMap))
--                             (hintPred observedKeys)
--                             (Aux.checkIfSame (hintPred observedKeys) (lookupValues observedKeys observationMap))
    
--     print foundValues
--     -- check whether single edits are always directed to OptSoln


--     -- putStrLn $ show $ zip3 (observedKeys) (lookupValues observedKeys observationMap) (map (isSegmentOf) $ (lookupValues observedKeys observationMap))
--     where
--         lookupValues k m = map (MB.fromMaybe "XXX" . \ x -> Map.lookup x m) k
--         hintPred = map predictHint
--     -- MB.fromJust . \x -> Map.lookup x mapped) foundTrue -- show $ Map.lookup "flfr" mapped
--     -- putStrLn "" -- $ show $ current -- show $ filter (foundTrue!!0) $ map (head) res
