module GroundTruth where

import Data.List as L
import Data.List.Split as L

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

import Expressions as E
import qualified JsonReader as J
import qualified Auxiliary as Aux

import qualified System.Directory as SD

import Database

-- Paths
laptopDropboxPath, pcDropboxPath, gtHOC4 :: String
laptopDropboxPath = "C:\\Users\\Milo\\"
pcDropboxPath = "H:\\Documents\\"
smallProjectPath = "Dropbox\\Study\\E-Learning\\Small project\\"
masterProjectPath = "Dropbox\\Study\\E-Learning\\Master Thesis\\Project\\Assignment 18\\"

dataPath = pcDropboxPath ++ "Dropbox\\Study\\E-Learning\\Master Thesis\\data"

gtHOC4 =  pcDropboxPath++ smallProjectPath ++"data\\hoc4\\groundTruth"
gtHOC18 = dataPath ++ "\\hoc18"

gtUnseen :: FilePath
gtUnseen = gtHOC18 ++ "\\" ++ "unseen\\"

gtFile :: FilePath
gtFile = gtHOC18 ++ "\\" ++ "groundTruth_data.txt"
-- gtFile = gtHOC4 ++ "\\" ++ "groundTruth_data.txt"

outFile = pcDropboxPath++ masterProjectPath ++ "output\\"

-- outFile :: IO FilePath
-- outFile = do 
--   currentDir <- SD.getCurrentDirectory
--   return $ currentDir ++ "\\output\\"


-----------------------------------------------------------
--          hint gen
-----------------------------------------------------------


printGT18Line :: (Maybe J.AST, Maybe J.AST) -> (E.Expr, E.Expr)
printGT18Line ((Just firstAST),(Just hint)) = 
    (E.parseAST firstAST, E.parseAST hint)


testRun :: String -> IO ()
testRun astID = do
    crnt <- J.readJSON astID
    print $ E.parseAST $ fromJust crnt

readGT :: IO ()
readGT = do
    gt <- readFile gtFile
    res <- mapM (\(a,b) -> (,) <$> J.readJSON a <*> J.readJSON b) $ Aux.splitted gt
    print (map (printGT18Line) res)


printEdits :: [(Expr,Expr)] -> IO ()
printEdits tmp = Aux.writeLines (outFile ++ "tmp.hs") $ Aux.lookupRight (-2) $ zip (map (\(x,y)-> E.exprCounter x - E.exprCounter y) tmp) (zip [1..] tmp)
-- stripI :: String -> String
-- stripI s
--   | head s == 'i' = tail s
--   | otherwise = s

-- What do experts suggest for a input
-- gsHint :: String -> Maybe String
-- gsHint s = Map.lookup s $ Map.fromList $ map (Aux.toTuple . splitOn " ") $ lines Aux.gshintL


-- chainer :: Int -> [[(String,String)]] --[Int]
-- chainer i = Aux.merge ( map (Aux.recursiveSearch Aux.tmpIDsL) $ Aux.snds lkp ) lookup
--     where lkp = Aux.lookupRight (show i) Aux.tmpIDsL



-- same as main2 only shorter. Now Aux.splitted is applied and then Monadified
-- main2short :: IO ()
-- main2short = print =<< (reverse . Aux.splitted <$> readFile gtFile)

-- chainer i = do
--     gt <- readFile gtFile
--     res <- mapM (\(a,b) -> (,) <$> J.readJSON (read a) <*> J.readJSON (read b)) $ Aux.splitted gt

--     let str = displayHint (fst (res!!(i-1))) (snd (res!!(i-1)))
--     putStrLn str
--     writeFile  (outFile++"hintCompOutput.hs") $ str  ++ "\n#####\n#####\n##.g#\n#s.##\n#####"

-- compareGoldStandard :: IO ()
-- compareGoldStandard = do 
--     gt <- readFile gtFile
--     -- putStrLn $ "["++ intercalate "," ((astIDsToX 0) (Aux.splitted gt)) ++ "]"
--     res <- mapM (\(a,b) -> (,) <$> J.readJSON (read a) <*> J.readJSON (read b)) $ Aux.splitted gt

--     -- fromAST <- J.readJSON (read (fst $ head $ Aux.splitted gt)::Int)
--     -- toAST <- J.readJSON (read (snd $ head $ Aux.splitted gt)::Int)
--     -- print (map (\ (fromAST, toAST) -> (compHint fromAST toAST)) res)
--     print (map (uncurry compHint) res)
--     -- map (\(fromAST,toAST) -> (snd $ compHint fromA  $ ST toAST)) res

-- testFile :: IO ()
-- testFile = do 
--     res1 <- J.readJSON (read "25")
--     res2 <- J.readJSON (read "10")
--     print (displayHint res1 res2) 

-- -- runList :: Int -> IO ()
-- -- runList i = do
-- --     lst <- chainer i

-- printTuple :: (String,String) -> IO ()
-- printTuple (fromID,toID) = do
--     firstAST <- J.readJSON (read fromID)
--     hint <- J.readJSON (read toID)
--     putStrLn $ displayHint firstAST hint 

-- printList :: Int -> Bool -> IO ()
-- printList i append = do
--     let allFsts = map Aux.fsts $ chainer i 
--     res <- mapM (mapM (J.readJSON . read)) allFsts 
--     -- myHints <- mapM (mapM (myHint . J.readJSON . read) ) allFsts 
--     let zippedRes = zip (concatMap ("-1" :) allFsts) (concatMap (Nothing :) res)
--     if append then putStr "" else writeFile (outFile ++ "printList.txt") ""   
--     mapM_ (\(idI,x) -> case x of
--         Nothing -> do
--             appendFile "printList.txt" "\n"
--             putStrLn ""
--         Just ast -> do 
--             appendFile (outFile ++ "printList.txt") $ idI ++ ": " ++ show (parseAST ast) ++ "\n"
--             putStr $ idI ++ ": "
--             print (parseAST ast) ) zippedRes
--                 where parseAST = E.parseAST

-- myHint :: Maybe J.AST -> IO E.Expr
-- myHint (Just toCheck) = return $E.SEQ (snd $ E.hint $ E.evalState (E.parseAST toCheck) (E.getState,[]) )
-- myHint Nothing = return E.TERMINATE

--     -- appendFile "printList.txt" "1"


-- printFsts :: Int -> IO ()
-- printFsts i = do
--     let allFsts = Aux.fsts $ head $ chainer i 
--     res <- mapM (J.readJSON . read) allFsts 
--     myHints <- mapM myHint res
--     let zippedRes = zip3 allFsts res myHints
--     putStrLn ""
--     mapM_ (\(idI,x,y) -> case x of
--         Nothing -> putStrLn "error"
--         Just ast -> do 
--             putStr $ idI ++ ": "
--             print (E.parseAST ast)
--             print y) zippedRes
--     putStrLn ""

--     -- let gsHint = E.parseAST hint
--     -- let myHint = E.SEQ (snd $ E.hint $ E.evalState (E.parseAST firstAST) (E.getState,[]) )
--     -- if gsHint == myHint 
--     -- then "True: " ++ (show gsHint)
--     -- else "False: (\nIn:    " ++(show $ E.parseAST firstAST)++"\nPiech: "++ (show gsHint)++"\nMijn:  "++(show myHint)++")\n" 

-- -- displayHint (Just firstAST) (Just hint) = do
-- --     let gsHint = E.parseAST hint
-- --     let myHint = E.SEQ (snd $ E.hint $ E.evalState (E.parseAST firstAST) (E.getState,[]) )
-- --     if gsHint == myHint 
-- --     then "True: " ++ (show gsHint) 
-- --     else "False: (\nIn:    " ++(show $ E.parseAST firstAST)++"\nPiech: "++ (show gsHint)++"\nMijn:  "++(show myHint)++")\n"    

-- runLine :: Int -> IO ()
-- runLine i = do
--     gt <- readFile gtFile

--     res <- mapM (\(a,b) -> (,) <$> J.readJSON (read a) <*> J.readJSON (read b)) $ Aux.splitted gt
--     -- let str = displayHint (fst (res!!(i-1))) (snd (res!!(i-1)))
--     let str = uncurry displayHint (res !! (i - 1))
--     putStrLn str
--     writeFile  (outFile++"hintCompOutput.hs") $ str  ++ "\n#####\n#####\n##.g#\n#s.##\n#####" -- displayHint (fst (res!!(i-1))) (snd (res!!(i-1))) ++ "\n#####\n#####\n##.g#\n#s.##\n#####"
--     -- putStrLn $ displayHint (fst item) (snd item)
--     -- writeFile  (outFile++"hintCompOutput.hs") $ res!!(i-1)


-- quickTest :: Int -> Int -> IO () 
-- quickTest x y = do
--     res1 <- J.readJSON x
--     res2 <- J.readJSON y
--     -- gridV <- E.display E.getState 
--     writeFile  (outFile++"hintCompOutput.hs") $ displayHint res1 res2 ++ "\n#####\n#####\n##.g#\n#s.##\n#####"
--     -- appendFile (outFile++"hintCompOutput.hs") "\n#####\n#####\n##.g#\n#s.##\n#####"

-- printFalse :: (Bool, [E.Expr]) -> IO ()
-- printFalse (b, x) = if b then appendFile "hintCompOutput.hs" "True\n" else
--     appendFile "hintCompOutput.hs" (show x)


-- type OutputComp = (([E.Expr],[E.Expr]), (Bool, [E.Expr], [E.Expr]))

-- compHint :: Maybe J.AST -> Maybe J.AST -> OutputComp
-- compHint (Just firstAST) (Just hint) = ((removeSEQ (parsed firstAST), removeSEQ (parsed hint)), compareLists (snd $ E.hint $ E.evalState (parsed firstAST) (E.getState,[]) )  (removeSEQ $ E.parseAST hint) ) where
--     parsed = E.parseAST
-- compHint _ _ = (([],[]), (False,[], []))


-- -- compHint :: Maybe J.AST -> Maybe J.AST -> Bool
-- -- compHint (Just firstAST) (Just hint) = (E.SEQ (snd $ E.hint $ E.evalState (E.parseAST firstAST) (E.getState,[]) )) == E.parseAST hint
-- -- compHint _ _ = False


-- -- compare two lists and return if equal and if not what differs
--     -- first list is first input, second list second input
-- -- compareLists :: (Eq a) => [a] -> [a] -> (Bool,[a],[a])
-- -- compareLists [] [] = (True, [], [])
-- -- compareLists [] y  = (True, [], y)
-- -- compareLists x [] = (False, x, [])
-- -- -- compareLists [x] [y] = if x == y then True else False
-- -- compareLists (x:xs) (y:ys) = if x==y then compareLists xs ys else (False, x : xs, y : ys) 

-- -- compHint :: Maybe J.AST -> Maybe J.AST -> Bool
-- -- compHint (Just firstAST) (Just hint) = (E.SEQ (snd $ E.hint $ E.evalState (E.parseAST firstAST) (E.getState,[]) )) == E.parseAST hint
-- -- compHint _ _ = False


-- displayHint :: Maybe J.AST -> Maybe J.AST -> String
-- displayHint (Just firstAST) (Just hint) = do
--     let gsHint = E.parseAST hint
--     let myHint = E.SEQ (snd $ E.hint $ E.evalState (E.parseAST firstAST) (E.getState,[]) )
--     if gsHint == myHint 
--     then "True: " ++ show gsHint 
--     else "False: (\nIn:    " ++show (E.parseAST firstAST) ++
--         "\nPiech: " ++
--             show gsHint ++ "\nMijn:  " ++ show myHint ++ ")\n"

-- stringifyGS :: IO ()
-- stringifyGS = do
--     gt <- readFile gtFile
--     splitList <- mapM (\(a,b) -> (,) <$> J.readJSON (read a) <*> J.readJSON (read b)) $ Aux.splitted gt
--     let res = map lineGS splitList
--     writeFile (outFile ++ "GS.txt") $ unlines res 

-- lineGS :: (Maybe J.AST, Maybe J.AST) -> String
-- lineGS (Just firstAST, Just hint) = 
--     show (E.parseAST firstAST) ++ "   " ++ show (E.parseAST hint)
-- lineGS _ = ""

--     -- writeFile (outFile ++ "GS.txt") ""
--     -- mapM_ (\(idI,x) -> case x of
--     --     Nothing -> do
--     --         appendFile "printList.txt" "\n"
--     --         putStrLn ""
--     --     Just ast -> do 
--     --         appendFile (outFile ++ "printList.txt") $ idI ++ ": " ++ (show $ parseAST ast) ++ "\n"
--     --         putStr $ idI ++ ": "
--     --         putStrLn $ show $ parseAST ast ) zippedRes
--     --             where parseAST ast = E.parseAST ast


-- removeSEQ (E.SEQ x) = x 
-- removeSEQ _ = []