{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS -Wall #-} 

module Strategy where

-- import              Debug.Trace

-- import qualified    System.Directory      as SD

import              Prelude
import              Data.List             as L
import              Data.List.Split       as L

-- import              Data.Map (Map)
import qualified    Data.Map              as Map
-- import qualified    Data.Tuple            as T
-- import qualified    Data.Set              as S
-- import qualified    Data.Ord              as O
import qualified    Data.MultiSet         as MultiSet

import qualified    Data.Maybe            as MB

-- import qualified Expressions           as E
import              Expressions
-- import qualified    JsonReader            as J
import qualified    Auxiliary             as Aux

-- import qualified    Data.Vector           as V
-- import qualified    Data.Vector.Distance  as Vd

import              System.IO
-- import qualified    Data.ByteString.Char8 as B

laptopDropboxPath, pcDropboxPath, smallProjectPath, masterProjectPath :: String
laptopDropboxPath = "C:\\Users\\Milo\\"
pcDropboxPath = "H:\\Documents\\"
smallProjectPath = "Dropbox\\Study\\E-Learning\\Small project\\"
masterProjectPath = "Dropbox\\Study\\E-Learning\\Master Thesis\\Project\\Assignment 18\\"

outFile :: String
outFile = pcDropboxPath++ masterProjectPath ++ "output\\"

data Heuristic = 
    OnTrack
  | SingleEdit
  | DelErr
  | InsStart
  | SubstErr
    deriving (Show, Eq, Ord)

data Edit = 
    Insert 
  | Delete
  | Substitute deriving (Eq, Show, Ord, Enum)

type ExprPair = (Expr, Expr) -- (in, out)



--------------------------------------------
--    IDENTIFICATION / PATTERN FINDING
--------------------------------------------
nonOccurring :: [IDTag]
nonOccurring = [S, F, L, R, PR]

solution :: Expr
solution = WHILE (IF PathAhead Forward TurnLeft)

solutionPaths :: [Expr]
solutionPaths = [Empty, WHILE Empty, WHILE (IF PathAhead Empty Empty), WHILE (IF PathAhead Forward Empty), WHILE (IF PathAhead Empty TurnLeft)] 

type Direction    = Int
type Directions   = [Direction]
type BreadCrumbs  = [Direction]
type History      = [IDTag]
-- data Crumb        = Crumb Direction Expr

type SE = (Expr, Expr, Expr, Directions, Direction,[Int], History) 

type Hint     = Expr
type Crumb  = [IDTag]

type ExprState = (Parent,(Current, Child), Hint, Crumb)


showES :: ExprState           -> String
showES    (x,(y1, y2), z, cs) =  show x ++ "\n(" ++ show y1 ++", " ++show y2 ++ ")\n" ++ show z ++ "\n" ++ show (cs)

type CompSE = (Expr, Expr, Directions, History, Hint, [Int])

initCompSE :: Expr -> (CompSE,[CompSE])
initCompSE    x     = (c,[c]) where
  c = (x, x,[],[], VOID,[-2])

showSRun :: [CompSE] -> IO ()
showSRun    xs        = do
  mapM_ (showS) (reverse xs) 
  putStrLn (show $ MB.fromMaybe "None" $ 
    Map.lookup 
    (let gf (x, _, _, _, _, _) = x 
     in encodeExpr (gf $ head xs)) $ Map.fromList Aux.hoc18SingleEdit) 
  where
    showS :: CompSE                          -> IO ()
    showS    (inp, expr, ds, hist, hint, idfs) = do
      let encEx     = encodeExpr expr
          -- sizum     = length encEx
          rect x y  = if (length x) < y then x ++ "\t\t" else x ++ "\t"
      putStr ((encodeExpr inp)   ++ " | " ++ rect encEx 6 ++ "@ ")
      putStr (rect (show ds) 6   ++ ", ")
      putStr (rect (show hist) 6 ++ "| ")
      putStr $ (encodeExpr hint) ++ " | "
      putStrLn (rect (show $ head idfs) 6)

-- | Main function that traverse the input program.
{-   CompSE consists of:
      input, currentExpr, directions, history, current hint, function identifier
-}
sRun :: (CompSE,[CompSE])                                 -> (CompSE,[CompSE])
sRun    se@(crnt@(inp, expr, ds, hist, hint, _), trace) = 
  let 
    (_, child, ds', hist', _hint', idfs) = ssE crnt
    -- crntID                              = exprID expr
    -- crntParent                          = Aux.safeHead hist V
    (parentID, parentExpr)              =
      if ds == [] 
        then (exprID expr, expr) 
        else travE inp (init ds) 
    
    -- nearSol inp                         = filter (\x-> distance inp x ==1 ) intermediaryCorrect

    -- continue another cycle and take the hint with you
    continue h c                        = if elem c idfs then (head trace, trace) else sRun (new, new:trace) 
      where
        new = (inp, child, ds', hist', h, c:idfs)
    
    -- select hint based on selectBetterHint
    -- continueSBH h c                        = sRun (new, new:trace) 
      -- where
        -- new = (inp, child, ds', hist', sba [hint,h], c)
    
    -- goto is a specifix form of continue where you skip some cycles and go to a specific location
    goto h i c                          =
      if i >= (size expr)
        then error "Too Large " -- ((inp, expr, ds, hist, hint, c),(inp, expr, ds, hist, hint, c):trace)
        else sRun (new, new:trace) 
          where
            newDs                              = {-error $ show  $ -} ds ++ [i]
            (_, expr'', newDs'', hist'', _, _) = stepWhile (inp, expr, newDs, hist, hint, c:idfs) newDs
            new                                = (inp, expr'', newDs'', hist'', h, c:idfs)

    -- final exits the loop and outputs the hint. 
    final h c                           = (new, new:trace) 
      where
        new = (inp, child, ds', hist', h, c:idfs)

    -- final' h c                           = (new, new:trace) 
    --   where
    --     new = (inp, child, ds', hist', h, c)
    --     checkSingleEdit x = if ses == VOID then x else ses
    --     ses = selectSingleEdit $ genSES inp 


    res x                               = updateE inp (ds) x
    -- hintRes h x                         = updateE h ds x

    k                                   = length trace

  -- there is sweetspot at max 15 cycles. There exist cases of infinite loops, which is a bit probablematic. 
  in if k > 15 then se else case expr of
    VOID        -> (crnt, trace)  -- end of run produces the output
    -- x | distanceSp solution inp == 1 -> final solution (-1)


    WHILE wX -> handleWHILE wX where 
      handleWHILE :: Expr -> (CompSE,[CompSE])
      handleWHILE    wExpr
       | wExpr == Forward 
        && (hist == [] || head hist == S || head hist == I)
          = final (res (WHILE Empty))                                                 10 -- DelErr 10
       | leaf wExpr 
          = final (res $ WHILE Empty)                                                 11 -- DelErr 8
        | otherwise 
          = continue (res (WHILE Empty))                                              110 -- {-DelErr-} 0/167
          -- where sel x = selectSingleEdit $ genPoss x
   
    IF _c _l _r -> handleIF expr where
      handleIF  :: Expr -> (CompSE,[CompSE])
      handleIF     crntIF@(IF c l r) 
        | parentID == W 
        && condID c /= PA 
        && condID c /= PR
        && (not . contains S) expr
        && distanceSp crntIF (getClosestInterMedCorr crntIF) == 1
          = final (res $ getClosestInterMedCorr crntIF)                               20 -- SingleEdit 1/1 
        | parentID == W 
        && condID c /= PA 
          = final (res $ IF PathAhead l r)                                            21 -- SubstErr 17/17
        | c == PathLeft && contains S l && contains S r 
          = final (deleteToNextShortDistance inp (ds++[1]) )                          22 -- DelErr 2/2
        |  hist == []
        && parentID /= S
        && (not . contains W) expr
        && not (idInExpr (condID c) solution) 
        && (not . contains S) crntIF
        && partialCorrect crntIF
          = final (res (WHILE crntIF))                                                23 -- InsStart 1
        |  hist == []
        && not (contains W expr)
        && not (idInExpr (condID c) solution) && (not $ contains S crntIF)
          = final (res (IF PathAhead l r))                                            24 -- SubstErr 9
        | contains S l && contains S r 
          = final (deleteToNextShortDistance inp (ds++[0]) )                          25 -- DelErr 3
        | parentID == S 
        && partialCorrect crntIF 
        && (countOccurences I inp) < 2
          = continue (hint)                                                           250 -- {-PREVIOUS-} 0/16
        | c == PathRight
          = final (res $ IF PathAhead l r )                                           26 -- SubstErr 10
        | distanceSp crntIF (getClosestInterMedCorr crntIF) == 1 
          = final (res $ getClosestInterMedCorr crntIF)                               27 -- SingleEdit 26
        |  parentID /= S && hist == [] 
        && not (contains W expr) -- && notElem W (map exprID $ tokenizer expr) 
        && (not $ contains S crntIF)
          = final (res $ WHILE (crntIF))                                              28 -- InsStart 8
       | first inp == S
        && startsCorrect inp
        && (size parentExpr) == 2
        && (contains W inp)
          = final (tmp (res $ WHILE (crntIF)))                                              295 -- DelErr when it obtains the result "wa.." 1
        | (first inp == S 
        && startsCorrect inp) 
        && (size parentExpr) == 2
        && not (contains W expr)
          = final (res $ WHILE (crntIF))                                              29 -- InsStart 6
        | parentID == S 
        && (Aux.xor (contains S l) (contains S r))
        && contains R crntIF 
          = final (deleteToNextShortDistance inp (ds ++ [findInIF crntIF S]))         30 -- DelErr 3
        | l /= Forward 
        && not ( partialCorrect l )
        && partialCorrect crntIF
        && (countOccurences I inp) >= 2
          = final (selectDeletion)                                                    305 -- DelErr 
{-spcl-}| not $ idInExpr (condID c) solution 
          = final (res (IF PathAhead l r))                                            31 -- SubstErr 12
        | l /= Forward 
        && (countOccurences I inp) >= 2
          = final (selectDeletion)                                                    309 -- DelErr 
        | l /= Forward 
        && not ( partialCorrect l )
          = continue (res (IF PathAhead Empty r))                                     310 -- {-DelErr-} <- actually does 11/57
        -- | not (correctOrientation r)
        | r == TurnRight
          = final (res (IF c l TurnLeft))                                             32 -- SubstErr 1
         | r /= TurnLeft 
          = continue (res (IF PathAhead Forward Empty))                               320 -- {DelErr} 2 /16
        | otherwise 
          = final (res (IF PathAhead Forward TurnLeft) )                              33 -- DelErr awFL -> aFL 1
        where
          tmp x = if null deletionToCorrectStart then x else fst $ head deletionToCorrectStart
          deletionToCorrectStart = Aux.getMaxFromTuple $  zip (delErrSEQ inp (init ds)) $ map (isInfixOf "wa" . encodeExpr) (delErrSEQ inp (init ds)) -- check if there is a delErr that leads to "wa...." without an InsStart
          selectDeletion = selectBetterHint (deletions inp ds) [pathDistanceToGoal]

      handleIF _ = final hint (-1)
    (SEQ _xs)    -> handleSEQonebyone expr where
      handleSEQonebyone :: Expr -> (CompSE,[CompSE])
      handleSEQonebyone    (SEQ xs)

        | programStartsCorrect -- programStartsCorrect-- starts correct 
        -- && any (not . leaf) xs
        -- && (not . leaf ) (last xs) 
        && singleEditToSol /= VOID
          = final (singleEditToSol)                                           401  -- SingleEdit 
        | Aux.safeHead hist V == W -- programStartsCorrect-- starts correct 
        && all leaf xs 
        && length xs == 2
          = final (deleteToNextShortDistance inp ds)                                           402  -- DelErr 49
        | Aux.safeHead hist V == W -- programStartsCorrect-- starts correct 
        && leaf (last xs) 
          = final (deleteToNextPath inp ds)                                           403  -- DelErr 49
        | (countOccurences I inp) >= 2 
          = goto (res $ flattenSEQ $ SEQ []) (findLocLastNonLeaf xs) 408
          -- = final (selectBetterHint (deletions inp (ds++[(findLocLastNonLeaf xs)])) [pathDistanceToGoal])                 408 -- 
        | length (filter (not.leaf) xs) == 2
        && onPath inp 
          = goto (res $ flattenSEQ $ SEQ []) (findLocLastNonLeaf xs)                 4085
        | length (filter (not.leaf) xs) == 2 
          = goto (res $ flattenSEQ $ SEQ []) (findLocFirstNonLeaf xs)                 409 -- {-DelErr-} 0/23
        | slStartsCorrect
        && first inp == W
        && not (startsCorrect (xs!!1))
        && partialCorrect (xs!!1) 
          = final (updateE inp (ds ++ [1]) (fixStartIF (xs!!1)))                      41  -- SubstErr 2
        | slStartsCorrect
        && partialCorrect (xs!!1) 
          = final (deleteToNextPath inp ds)                                           42 -- DelErr 8
        | slStartsCorrect
        && (not $ leaf $ xs!!1) 
        && onPath inp 
          = goto (deleteToNextPath inp ds) 1                                          420 -- {-DelErr-} 0/7
        | slStartsCorrect
        && (case (xs!!1) of IF PathAhead _ _ -> True; _ -> False ) 
          = goto (deleteToNextPath inp ds) 1                                          421 -- {-DelErr-} 0/41
        | (not . contains W) inp
        && contains PA inp
        -- && hist == [] 
        && (pathDistanceToGoal (WHILE inp) == 0)
          = final (WHILE inp)                                           43 -- InsStart 1
        | any onPath options 
        && hist == [] 
          = final (deleteToNextPath inp ds)                                           44 -- DelErr 20
        | all leaf xs 
          = final (deleteToNextShortDistance inp ds)                                  45  -- DelErr 11
        | slStartsCorrect
        && (not $ leaf $ xs!!1) 
          = goto (res $ flattenSEQ $ SEQ (deleteLastLeaf xs)) 1                       450 -- {-DelErr-} 0/35
        | Aux.safeHead xs VOID == Forward 
        && (Aux.safeIndex xs 1 VOID == TurnLeft) 
        && (exprID (Aux.safeIndex xs 2 VOID) == I) 
        && contains S (Aux.safeIndex xs 2 VOID) 
          = goto (deleteToNextPath inp ds) 2                                          451 -- {-DelErr-} 0/2
        | slStartsCorrect
        && (Aux.safeIndex xs 1 VOID == TurnLeft) 
        && (exprID (Aux.safeIndex xs 2 VOID) == W) 
          = goto (res $ flattenSEQ $ SEQ (deleteLastLeaf xs)) 2                       452 -- {-DelErr-} 0/7
        | correctOrientation inp
        && (not.leaf) (last xs)
        && programStartsIncorrect
          = goto (res $ flattenSEQ $ SEQ []) (findLocLastNonLeaf xs)                  453 -- {-DelErr-} 0/2
        | {-any leaf xs-} not $ leaf $ last xs  -- = contains a non-leaf
          = final (res $ flattenSEQ $ SEQ (deleteLastLeaf xs))                        46 -- DelErr 25
        | otherwise 
          = goto (res $ flattenSEQ $ SEQ (deleteLastLeaf xs)) (findLocLastNonLeaf xs) 460 -- {-DelErr-} 0
          where
            -- patterns
            programStartsCorrect   = first inp == W
            programStartsIncorrect = not programStartsCorrect
            slStartsCorrect        = Aux.safeHead xs VOID == Forward
            singleEditToSol        = selectSingleEdit (genPoss inp)
            -- cctScan = filter (\x -> pathDistanceToGoal x ==0) ({-(WHILE inp):-}{-(res (WHILE expr)):-}{-substs-} genSESs)
            -- genSESs                = genSES inp
            -- substs                 = map (res . SEQ) $ findSubs xs
            options                = (delErrSEQ inp ds)
            -- loc                    = findLocX xs I
            -- deletionToCorrectStart = fst $ head $ Aux.getMaxFromTuple $  zip options $ map (isInfixOf "wa" . encodeExpr) (options) -- Check if there is a deletion such that wa
      handleSEQonebyone _ = final hint (-1)

    Empty       -> continue (WHILE Empty)                                             0 -- OnTrack 1
    Forward     -> if inp == Forward 
      then         final    Empty                                                     1 -- DelErr {-Forward-}  1
      else         continue hint                                                      2 -- {-PREVIOUS-} 8/44
    TurnRight   -> continue hint                                                      3 -- {-PREVIOUS-} 1/18
    TurnLeft    -> continue hint                                                      4 -- {-PREVIOUS-} 2/42
    TERMINATE -> final hint (-1)

type Score = Expr -> Int 
type Scores = [Score]

sba :: [Expr] -> Expr
sba xs = selectBetterHint xs [distanceSp solution]

selectBetterHint :: [Expr] -> Scores -> Expr 
selectBetterHint xs fs = snd $ Aux.safeHead (sort $ zip (map (\x -> sum $ map ($ x) fs) xs) xs) (0,VOID)

selectSingleEdit :: [Expr] -> Expr
selectSingleEdit xs = snd $ Aux.safeHead (sort $ zip (map (distance solution) toppers) $ toppers) (0,VOID)
  where
    toppers = filter ((==0) . pathDistanceToGoal) xs

-- genPoss' :: Expr -> Directions -> Heuristic -> [(Expr,Directions,Heuristic)]
-- genPoss' x ds OnTrack    = 
--   let tracks = [Empty, WHILE Empty, WHILE (IF PathAhead Empty Empty), WHILE (IF PathAhead Forward Empty), WHILE (IF PathAhead Forward TurnLeft) ]
--   in case Aux.next (Just x) tracks of
--     Nothing -> []
--     Just y  -> [(y,[],OnTrack)]
-- -- genPoss' x ds SingleEdit =
-- genPoss' x ds DelErr     = map (\x-> (x,ds,DelErr)) (delErrSEQ x ds)
-- genPoss' x ds InsStart   = [(WHILE x, ds, InsStart)]
-- genPoss' x ds SubstErr   = 

genPoss :: Expr -> [Expr]
genPoss crnt@(SEQ _) = reverse $ findSubs' crnt ++ delErrSEQ crnt [] ++ concatMap (\(xs,d) -> (map (\x-> updateE crnt [d] x) xs)) ifss
  where
    ifss     = map (\(x, y) -> (genPoss x, y)) ifers
    ifers    = filtLocsX I $ findLocNonLeaf $ childrenSEQ crnt
genPoss (IF c l r) = allPoss ++ map (\x-> IF c x r) (genPoss l) ++ map (\x-> IF c l x) (genPoss r)
  where
    -- possCond = [PathAhead .. PathRight] 
    -- possSubst = [Empty,Forward,TurnLeft,TurnRight]
    allPoss   = ({-possIns:-}possSubst++possDels)
    -- possIns   = WHILE crnt
    possDels  = if exprID l /= S 
      then IF c Empty r : (if exprID r /= S then [IF c l Empty] else [] )
      else []
    possSubst = map (\x-> IF c x r) (findSubs' l) ++ (map (\x -> IF c l x) (findSubs' r))
genPoss (WHILE x) = map WHILE $ genPoss x
genPoss _ = [] 


deletions :: Expr -> Directions -> [Expr]
deletions inp ds = 
  let (_crntID, crntE) = travE inp ds
      siz = size crntE
      dels = map (\y -> deleteE inp (ds ++ [y] )) ([0..(siz-1)])
  in dels


getDecidingHeuristic :: CompSE            -> Int
getDecidingHeuristic    (_, _, _, _, _, []) = -1
getDecidingHeuristic    (_, _, _, _, _, x:_xs) = x

first :: Expr -> IDTag
first    x     = exprID $ fst $ headExpr x 

fixStartIF :: Expr      -> Expr
fixStartIF    (IF _ l r) = IF PathAhead l r
fixStartIF    (WHILE x)  = WHILE (fixStartIF x)
fixStartIF    (SEQ xs )  = SEQ (fixStartIF (head xs):tail xs)
fixStartIF    x          = x

startsCorrect :: Expr              -> Bool
startsCorrect    (IF PathAhead _ _) = True
startsCorrect    (IF _ _ _)         = False
startsCorrect    (WHILE Forward)    = True
startsCorrect    (WHILE x)          = startsCorrect x
startsCorrect    (SEQ (Forward:_xs)) = True
startsCorrect    (SEQ _)            = False
startsCorrect    x                  = onPath x && correctOrientation x


-- numbErrors :: Expr -> Directions -> Int
-- numbErrors    inp     ds          = 
--   let crnt           = travE inp ds
--       nearestPartial = getClosestInterMedCorr (snd crnt)
--       errorsNumb     = distanceSp (snd crnt) nearestPartial
--   in errorsNumb

deleteLastLeaf :: [Expr] -> [Expr] 
deleteLastLeaf    xs        = 
  let splitted = split (whenElt (not.leaf)) xs
  in case (last splitted) of 
    [] -> (Aux.safeInit (Aux.safeHead splitted []) []) ++ concat (tail splitted)
    _  -> (concat $ init splitted) ++ (init $ last splitted) 
  -- init $ concat $ splitWhen (not.leaf) xs
  -- in (init $ head i) ++ concat (tail i)

-- findExpr :: Expr -> IDTag -> Directions
-- findExpr inp idT = reverse $ go inp idT where
--   -- go :: Expr -> IDTag -> Maybe Directions
--   go inp idT = case inp of
--     (WHILE x)  | idT == W       -> [] 
--     (WHILE x)                   -> (go x idT)++[0]
--     (IF c l r) | idT == I       -> []
--     (IF c l r) | idT == c       -> []
--     (IF c l r) 
--             | idT == (exprID l) -> [0]
--     (IF c l r) 
--             | idT == (exprID r) -> [1]
--     (IF c l r) | contains idT l -> (go l y) ++ [0]
--     (IF c l r) | contains idT r -> (go r y) ++ [1]
--     (IF c l r)                  -> [-2]
--     (SEQ xs)   | idT == S       -> []
--     (SEQ xs)                    -> map (`go` idT) xs

findInIF :: Expr ->    IDTag -> Direction
findInIF    (IF _c l r) idT
  | exprID l == idT = 0
  | exprID r == idT = 1
  | otherwise       = -1
findInIF    _ _ = -1

filtLocsX :: IDTag -> [(Expr, Direction)] -> [(Expr, Direction)]
filtLocsX i = filter ((==i) . exprID . fst)

findLocNonLeaf :: [Expr] -> [(Expr, Direction)]
findLocNonLeaf    xs      = concat $ splitWhen (\(x, _) -> leaf x) $ zip xs [0..]

findLocX :: [Expr] -> IDTag -> Maybe Direction
findLocX    xs        idT    = elemIndex idT  $ map exprID xs

findLocLastNonLeaf :: [Expr] -> Direction
findLocLastNonLeaf    xs      = if null result then error "-5" else snd $ last result 
  where result = findLocNonLeaf xs

findLocFirstNonLeaf :: [Expr] -> Direction
findLocFirstNonLeaf    xs = if null result then error "-5" else snd $ head result 
  where result = findLocNonLeaf xs


-------------------------
-- let tmp xs = (length (filter (not.leaf) xs) == 2) in Aux.printTuples4Ln $ applyAffected $ checkAffectedSEQ (\xs -> tmp xs )
-------------------------

-- getNearestSolution :: Expr -> Expr -> Expr
applyAffected :: (Expr -> Bool) -> [(Bool,(String, String), String)]
applyAffected    f               = 
  let t x      = getHintFromSE (sRun $ initCompSE (decodeExpr x)) 
      tmp      = filter (\(x, _) -> (f . decodeExpr) x) Aux.hoc18SingleEdit
      equality = (zipWith (==) (map (decodeExpr . snd) tmp) (map (\(inp, _gs)-> t inp) tmp) )
      res      = (map (\(inp, _gs)-> encodeExpr $ t inp) tmp)
  in zip3 equality tmp res

cA :: Expr -> Bool
cA    xpr   = checkAffectedSEQ (\x -> movementsOnTrack (x) && any (not.leaf) (x) ) xpr

checkAffected :: (Expr -> Bool) -> Expr -> Bool -- [(String, String)]
checkAffected    f                 x     = case x of
  y | f y     -> True           
  (WHILE y)   -> checkAffected f y
  (IF _ l r)  -> checkAffected f l || checkAffected f r
  (SEQ xs )   -> any (checkAffected f) xs
  _           -> False

checkAffectedSEQ :: ([Expr] -> Bool) -> Expr -> Bool
checkAffectedSEQ    fs                  x     = case x of
  (SEQ xs)    -> fs xs || any (checkAffectedSEQ fs) xs 
  (WHILE y)   -> checkAffectedSEQ fs y
  (IF _ l r)  -> checkAffectedSEQ fs l || checkAffectedSEQ fs r
  _           -> False


stepWhile :: CompSE ->                             Directions -> CompSE
stepWhile    crnt@(inp, expr, ds, hist, hint, idf) newDs
  | ds == newDs   = (inp, snd $ travE inp ds, ds, hist, hint, idf)
  | (not.null) ds 
  && expr == VOID = crnt
  | otherwise     = stepWhile (ssE crnt) newDs

ssE :: CompSE                          -> CompSE
ssE    (inp, expr, ds, hist, hint, idfs) = (i, e, d, his, hint, idfs) where
  addC (i', e', p', d', ptr', pSs', h') = (i', e', p', d', ptr', pSs', h', c)
  (i, e, _p, d, _pp, _psp, his, c)  = 
    if ds == [-1] 
      then (inp, VOID, VOID, ds, 0,[], hist, c) 
      else addC $ sE (inp, expr, VOID, ds, 0,[], hist)

sE :: SE                                         -> SE
sE    (inp, _expr, _parent, ds, _pointer, _pSs, hist) = 
  let (crntID, crntExpr)     = travE inp (ds) 
      (_nextID, nextExpr)     = travE inp (fst nextDir)
      (_parentID, parentExpr) =
        if ds == [] 
          then (crntID, crntExpr) 
          else travE inp (init (fst nextDir))

      dpt                    = depths inp (ds)
      newDirBP               = backProp ds dpt

      nextDir :: (Directions, Direction)
      nextDir  = if
        | leaf (crntExpr)     -> (newDirBP                , ((last newDirBP) + 1))
        | not (leaf crntExpr) -> (ds ++ [0]               , 0)
        | otherwise           -> error "" -- ((pointer:(tail ds) )    , pointer+1)

      lastExpr               = ds == [-1]
      continue               = (inp, nextExpr, parentExpr, fst nextDir, snd nextDir, dpt,(exprID parentExpr:hist))

  in if 
    | lastExpr          -> (inp, VOID, VOID,[-1],-1,[-1], hist) 
    | otherwise         -> continue 

depths :: Expr -> Directions -> [Int]
depths    inp     []          = [size inp]
depths    inp     xs          = go inp xs 0 where
  go :: Expr -> Directions -> Int -> [Int]
  go    _x      ds            i
    | i == (length ds)  = [] 
    | otherwise         = (size (getToCrnt (take i ds)):go inp ds (i+1))  
    where
      getToCrnt y = snd $ travE inp y

size :: Expr -> Int
size    x     = case x of
  (WHILE _ ) -> 1
  (IF _ _ _) -> 2
  (SEQ xs  ) -> (length xs)
  _          -> 0 

-- | backprop brings the pointer back to the next possible position (only works with d=1)
backProp :: Directions -> [Int] -> Directions
backProp    ds            sizes  = 
  let combis = reverse (zip ds sizes)
      newDs' = (map fst $ dropWhile (\(x, y) -> (x+1)>=y) combis)
      newDs  = if newDs' == [] then [-1] else reverse (head newDs' + 1:tail newDs')
  in newDs

showSE :: SE                    -> IO ()
showSE    (_x, y, z, ds, d, i, h) = do
  prettyPrint y
  prettyPrint z
  print h
  print $ concat $ intersperse " , " ["ds: " ++ show ds, "ptr: " ++ show d, "parentSize: " ++ show i]


initSE :: Expr -> SE
initSE    x       = (x, x, VOID,[], 0,[],[]) 

stepE :: Expr ->    Direction -> (IDTag, Expr)
stepE    x          (-2)       = (exprID x, x) 
stepE    (WHILE x)  _          = (exprID x, x)
-- stepE (IF x l r) 0          = (condID x, IF x l r)
stepE    (IF _ l _) 0          = (exprID l, l)
stepE    (IF _ _ r) 1          = (exprID r, r)
stepE    (SEQ _)    (-1)       = (V,   VOID)
stepE    (SEQ xs)   y          = if (length xs) < (y+1) then (V, VOID) else (exprID (xs!!y), xs!!y)
stepE    _x         _          = (V, VOID)

travE :: Expr -> Directions -> (IDTag, Expr)
travE    e       []          = (exprID e, e)
travE    e       [dir]       = stepE e dir
travE    e       (dir:dirs)  = travE (snd $ stepE e dir) dirs

updateE :: Expr ->    Directions -> Expr        -> Expr
updateE    _          []            new          = new
-- updateE [d] x                new    = snd $ stepE x d
updateE    (WHILE x)  (_:ds)        new          = WHILE (updateE x ds new)
updateE    (IF _ l r) (-1:_)        (IF c' _ _)  = IF c' l r
updateE    (IF c l r) (0:ds)        new          = IF c (updateE l ds new) r
updateE    (IF c l r) (1:ds)        new          = IF c l (updateE r ds new)
updateE    (SEQ xs )  (d:ds)        new          = SEQ mergins 
  where
    splitted = splitAt d xs
    altered  = updateE (head $ snd $ splitted) ds new
    mergins  = (fst splitted)++(altered:(tail $ snd splitted))
updateE     _x        _ds          _new          = error "Beyond a leaf"
  -- SEQ (updateE ds (xs!!d))

-- upE :: Expr -> Direction -> Expr -> Expr
-- upE (WHILE x)  _ new = (WHILE new)
-- upE (IF c l r) 0 new = new
-- upE (IF c l r) 1 new = (IF c new r)
-- upE (IF c l r) 2 new = (IF c l new)
-- upE (SEQ xs )  d new = SEQ mergins where
--   splitted = splitAt d xs
--   mergins = (fst splitted)++(new:(tail $ snd splitted))
-- upE x          _ new = Empty


delE :: Expr ->    Direction -> Expr
delE    (WHILE _)  _          = (WHILE Empty)
delE    (IF _ l r) (-1)       = (IF GoalReached l r)
delE    (IF c _ r) 0          = (IF c Empty r)
delE    (IF c l _) 1          = (IF c l Empty)
delE    (SEQ xs )  d          = (SEQ ( remAt d xs) )
delE    _          _          = Empty

-- deleteE :: Expr -> Directions -> Expr
-- deleteE x           []     = Empty
-- deleteE (WHILE x)   (d:ds) = WHILE (deleteE x ds) 
-- deleteE (IF c l r)  (d:ds) = IF c (deleteE l ds) (deleteE r ds)
-- deleteE (SEQ xs )   (d:ds) = SEQ (remAt d xs)
-- deleteE x           ds     = error "Beyond a leaf"

deleteE :: Expr ->    Directions -> Expr
deleteE    _          []     = Empty
deleteE    x          [d]    = delE x d
deleteE    (WHILE x)  (_:ds) = WHILE (deleteE x ds)
-- deleteE (IF c l r) (-1:ds)= IF c' l r
deleteE    (IF c l r) (0:ds) = IF c (deleteE l ds) r
deleteE    (IF c l r) (1:ds) = IF c l (deleteE r ds)
deleteE    (SEQ xs )  (d:ds) = SEQ mergins where
  splitted = splitAt d xs
  altered  = deleteE (head $ snd $ splitted) ds
  mergins  = (fst splitted)++(altered:(tail $ snd splitted))
deleteE    _           _     = error "Beyond a leaf"

remAt :: Int -> [a]   -> [a]
remAt   _       []     = []
remAt   i       (a:as)
  | i == 0    = as
  | otherwise = a : remAt (i-1) as

elemAt :: Directions -> Expr      -> Expr
elemAt    [] x                     = x
elemAt    (_:ds)        (WHILE x)  = (elemAt ds x)
elemAt    (d:ds)        (IF c l r) = case d of
  0 -> IF c VOID VOID
  1 -> elemAt ds l
  2 -> elemAt ds r
  _ -> error ""
elemAt    (d:ds)        (SEQ xs )  = if (length xs) < (d+1) then error "2" else elemAt ds (xs!!d)
elemAt    _             x          = x -- error beyond leaf

intermediaryCorrect :: [Expr]
intermediaryCorrect = [WHILE Forward, IF PathLeft TurnLeft Forward, IF PathAhead Forward TurnLeft]

getClosestInterMedCorr :: Expr              -> Expr
getClosestInterMedCorr    (IF PathLeft _ _)  = IF PathLeft TurnLeft Forward
getClosestInterMedCorr    (IF PathAhead _ _) = IF PathAhead Forward TurnLeft
getClosestInterMedCorr    (WHILE _)          = WHILE Forward
getClosestInterMedCorr    inp                = fst $ head $ Aux.getMinFromTuple scores 
  where
    scores = zip intermediaryCorrect $ map (distance inp) intermediaryCorrect


partialCorrect :: Expr              -> Bool
partialCorrect    (WHILE x)
  | x == Forward                     = True
  | otherwise                        = partialCorrect x
partialCorrect    (IF PathAhead l r) =
  (l == Forward || l == (WHILE Forward)) 
  && (r == TurnLeft || r == WHILE TurnLeft) 
partialCorrect    (IF PathLeft l r)  =
  (l == TurnLeft || l == WHILE TurnLeft) 
  && (r == Forward || r == (WHILE Forward))
partialCorrect    _                  = False

deleteLastNonOccurring :: [Expr] -> [Expr]
deleteLastNonOccurring    xs      = reverse $ go (reverse xs) where
  go [] = []
  go (x:ys)
    | (elem (exprID x) nonOccurring) = ys
    | otherwise                      = x:go ys
 
getHintFromSE :: (CompSE,[CompSE])        -> Expr
getHintFromSE     ((_, _, _, _, x, _), _)  = x
 
-- | Get hint and getDecidingHeuristic
getHintFromSE' :: (CompSE,[CompSE])      -> (Expr, Int)
getHintFromSE'    ((_, _, _, _, x, []), _) = (x, -1)
getHintFromSE'    ((_, _, _, _, x, y:_ys), _) = (x, y)

mainHist :: String -> IO ()
mainHist    infixS = do
  let output   = mainPrintTrajectories
      true     = length $ filter (\(a,(_, _), _, _) -> a==True) output
      false    = length output - true
      accuracy = ((fromIntegral true) / (fromIntegral $ length output))::Float
  outh         <- openFile (outFile ++ "hPA"++ infixS ++ ".hs") WriteMode
  sequence_   [hPutStrLn outh $ showoff x | x <- output]
  hPutStrLn   outh $ (show true) ++ " / " ++ (show false) ++ " : " ++ (show accuracy)
  hClose outh
  putStrLn  $ (show true) ++ " / " ++ (show false) ++ " : " ++ (show accuracy) 
  where
    showoff (a,(b, c), d, e) = show (a, e,(b, c), d)
    mainPrintTrajectories :: [(Bool, (String, String), String, [Int])]
    mainPrintTrajectories  = 
      let t x      = getHintFromSEinclID (sRun $ initCompSE (decodeExpr x)) 
          dataSet  = Aux.hoc18SingleEdit
          output   = remDups $ Aux.zip4''
                  equality 
                  (dataSet) 
                  (map (\(inp, _)-> (encodeExpr $ fst $ t inp, snd $ t inp)) dataSet)
          equality = (zipWith (==) (map (decodeExpr . snd) dataSet) (map (\(inp, _)-> fst $ t inp) dataSet) )

          getHintFromSEinclID :: (CompSE,[CompSE])      -> (Expr, [Int])
          getHintFromSEinclID ((_, _, _, _, x, ii),_z) = (x, init ii) --concatMap (\(_, _, _, _, _, i) -> (i:[])) (init z))

      in output


-- PRINTING TAKES LONG
mainResult :: [(Bool, (String, String), String, Int)]
mainResult  = 
  let t x      = getHintFromSE' (sRun $ initCompSE (decodeExpr x)) 
      dataSet  = Aux.hoc18SingleEdit
      output   = {-remDups $-} Aux.zip4''
              equality 
              (dataSet) 
              (map (\(inp, _)-> (encodeExpr $ fst $ t inp, snd $ t inp)) dataSet)
      equality = (zipWith (==) (map (decodeExpr . snd) dataSet) (map (\(inp, _)-> fst $ t inp) dataSet) )
  in output

main :: String -> IO ()
main    infixS = do
  let output   = mainResult
      true     = length $ filter (\(a,(_, _), _, _) -> a==True) output
      false    = length output - true
      accuracy = ((fromIntegral true) / (fromIntegral $ length output))::Float
  outh         <- openFile (outFile ++ "hPA"++ infixS ++ ".hs") WriteMode
  sequence_   [hPutStrLn outh $ showoff x | x <- output]
  hPutStrLn   outh $ (show true) ++ " / " ++ (show false) ++ " : " ++ (show accuracy)
  hClose outh
  putStrLn  $ (show true) ++ " / " ++ (show false) ++ " : " ++ (show accuracy) 
  where
    showoff (a,(b, c), d, e) = show (a, e,(b, c), d)
  -- mapM_ (putStrLn.show) output 
  -- putStrLn  $ (show true) ++ " / " ++ (show false) ++ " : " ++ (show accuracy)
  -- d <- getCurrentDirectory
  -- Aux.writeLines (d ++"/tmp" mapM_ (putStrLn.show) output 
  -- putStrLn  $ (show true) ++ " / " ++ (show false) ++ " : " ++ (show accuracy)

remDups :: [(Bool,(String, String), String, a)] -> [(Bool,(String, String), String, a)]
remDups    []                                     = []
remDups    [x]                                    = [x]
remDups 
  (outp1@(eqOutHint1,(inp1, _hnt1), _out1, _i1):
  outp2@(eqOutHint2,(inp2, _hnt2), _out2, _I2):xs)   =
    if inp1 == inp2 
      then if eqOutHint1 
            then remDups (outp1 : xs) 
            else 
              if eqOutHint2 
                then remDups (outp2 : xs) 
                else outp1 : outp2 : remDups xs
      else outp1 : remDups (outp2:xs)

-- | Given current input, calculate the next hint based on Expr calculations
-- applyExpressionEvaluation :: String -> String
-- applyExpressionEvaluation    s       =
--   let e = fromString s 
--   in toString $ SEQ $ snd $ hintE $ evalCharacter e (getCharacter,[])



-- onPathSEQ :: [Expr] -> [Bool]
-- onPathSEQ    xs      =
--   let l    = length xs
--       calc = map (\x -> SEQ (take x xs) ) [1..l] 
-- {--}  in map onPath calc

-- onOrientationSEQ xs =
  -- let l = length xs
      -- calc = map (\x -> SEQ (take x xs) ) [1..l]

  -- in map correctOrientation calc



----------------------------------------------------
------------- STRATEGIES
----------------------------------------------------
-- delErrSEQf :: (Expr -> Bool) -> Expr -> Directions -> [Expr]
-- delErrSEQf f inp ds = 
--   let (crntID, crntE) = travE inp ds
--       crntSize = size crntE 
--       filter (leaf ) $ stepE crntE [0..(crntSize-1)]
--       -- newDels = map (\x -> ds ++ [x]) [0..(crntSize-1)]

findSubs' :: Expr -> [Expr]
findSubs' inp = go inp [] where
  go :: Expr -> [Expr] -> [Expr]
  go TurnLeft _ = [TurnRight]
  go TurnRight _ = [TurnLeft]
  go (SEQ (x:xs)) ys
    | x == TurnLeft = (SEQ $ (reverse ys)++(TurnRight:xs)):(go (SEQ xs) (x:ys))
    | x == TurnRight = (SEQ $ (reverse ys)++(TurnLeft:xs)):(go (SEQ xs) (x:ys))
    | otherwise = go (SEQ xs) (x:ys)
  go _ _ = []

findSubs :: [Expr] -> [[Expr]]
findSubs inp = go inp [] where
  go :: [Expr] -> [Expr] -> [[Expr]] 
  go [] _ = []
  go (x:xs) ys
    | x == TurnLeft = ((reverse ys)++(TurnRight:xs)):(go xs (x:ys))
    | x == TurnRight = ((reverse ys)++(TurnLeft:xs)):(go xs (x:ys))
    | otherwise = go (xs) (x:ys)

-- checkF :: [Expr] -> [Expr] -> Bool -- "flf" "flr" False
-- checkF [] [] = True
-- checkF _  [] = False 
-- checkF [] _  = False 
-- checkF (x:xs) (y:ys) 
--     | x == Forward && x /= y = False  
--     | y == Forward && x /= y = False  
--     | otherwise = checkF xs ys


-- findSubstitutions :: [Expr] -> [Expr]
-- findSubstitutions s = map SEQ $ 
--     filter ((&&) <$> (\ x -> distance (SEQ s) (SEQ x) == 1) <*> checkF s) expanded
--     where   expanded = mapM (const o) [1 .. (length s)]
--             o = [Forward,TurnLeft,TurnRight, IF PathAhead Forward TurnLeft]

delErrSEQ :: Expr -> Directions -> [Expr]
delErrSEQ    inp     ds          = 
  let (_crntID, crntE) = travE inp ds
      crntSize         = size crntE
      newDels          = filter ((leaf.snd.stepE crntE.last)) $
        map (\x -> ds ++ [x]) [0..(crntSize-1)]

  in map (flattenSEQ . deleteE inp) newDels

getClosest :: [Expr] -> Expr
getClosest    xs      = 
  let scores = zip xs (map (`distance` solution) xs)

  in fst $ head $ Aux.getMinFromTuple scores

-- selectBestForParent :: Expr -> {-Expr ->-} Directions -> Expr
-- selectBestForParent inp ds = 
--   let crnt = travE inp ds
--       parent = travE inp (Aux.safeInit [] ds)
--   in case parent of
--     IF c l r -> in case c of
--       PathAhead -> IF PathAhead Forward TurnLeft
deleteToNextShortDistance :: Expr -> Directions -> Expr
deleteToNextShortDistance    x       ds          = 
  let crnt           = travE x (Aux.safeInit ds [])
      nearestPartial = getClosestInterMedCorr (snd crnt)
      poss           = delErrSEQ x ds
      scores         = zip poss (map (distanceSp nearestPartial) poss)
  in fst $ Aux.safeHead (Aux.getMinFromTuple $ scores) (VOID, 0)

checkLocal :: Expr -> Directions -> Expr
checkLocal    inp     ds          = 
  let poss    = delErrSEQ inp ds
      scores  = zip poss (map partialCorrect poss)
  in  fst $ head $ Aux.getMaxFromTuple $ scores

deleteToNextPath :: Expr -> Directions -> Expr
deleteToNextPath inp ds = 
  let poss                 = delErrSEQ inp ds 
      options              = map (\x -> snd $ travE x ds) poss
      onPathScores         = zip poss (map onPath options)
      onPathFilt           = filter snd onPathScores
      rightDirectionScores = zip (map fst onPathFilt) $ map (\(x, _) -> correctOrientation (snd $ travE x ds) && startsCorrect (snd $ travE x ds)) $ onPathFilt
      -- calcNearest          = [(deleteToNextShortDistance inp ds,True)]
      -- allFalse = all (\(x, y) not y) scores 
  in fst $ head $ Aux.getMaxFromTuple 
    (if all (not . snd) rightDirectionScores
      then onPathScores 
      else rightDirectionScores )

applyGlobalSEQ :: ([Expr] -> [Expr]) -> Expr -> Directions -> Expr
applyGlobalSEQ    f                     inp     ds          = applyGlobalSEQ' inp ds $ applyLocalSEQ f inp ds where
  -- | take output of an applied function and update the expression
  applyGlobalSEQ' :: Expr -> Directions -> [Expr] -> Expr
  applyGlobalSEQ'    x       ds'            new     = updateE x ds' (SEQ new) 
  
  -- | Traverse to a specific location in the tree and apply the function there.
  applyLocalSEQ :: ([Expr] -> [Expr]) -> Expr -> Directions -> [Expr]
  applyLocalSEQ    f'                     x       ds'          = 
    let (_, crntE)  = travE x ds'
        target      = case crntE of 
          (SEQ xs)  -> xs
          _         -> error "Not A SEQ!"
    in f' target


movementsOnTrack :: [Expr] -> Bool
movementsOnTrack    xs      = 
  let stripped = filter (leaf) xs 
  in if (Aux.safeHead (getLastLeaf_S xs) Empty) == Forward 
      then (onPath . SEQ) stripped 
      else (correctOrientation . SEQ) stripped

deleteToNextPath_S :: [Expr] -> [Expr]
deleteToNextPath_S    xs      = 
  let poss      = delErrSEQ_S xs
      scores    = zip (poss) (map movementsOnTrack poss)
      -- lastL ys  = head $ getLastLeaf_S ys
  in fst $ head $ Aux.getMaxFromTuple $ scores

delErrSEQ_S :: [Expr] -> [[Expr]]
delErrSEQ_S    xs      = 
  let crntSize = length xs

      newDels = filter (not.null) (map (\x -> join $ splitAt x xs) [0..crntSize-1])
      join (xs', (y:ys)) = if leaf y then concat [xs', ys] else []
      join (_, []) = []

  in newDels

getLastLeaf_S :: [Expr] -> [Expr]
getLastLeaf_S    xs      = (dropWhile (not.leaf) (reverse xs)) -- leaf is in head
-- matchCondition :: Expr -> Expr -> Int
-- matchCondition (IF c l r) crnt = 

--------------------------------
--   ACCURACY MEASURER 
--
-- > Aux.printTuples3Ln $ sort $ accMeasure t
--------------------------------

accMeasure :: [(Double, Int, (MultiSet.Occur, MultiSet.Occur))]
accMeasure =
  let results            = {-MultiSet.toOccurList $-} MultiSet.fromList $ map extract mainResult
      extract (b,_,_,i)  = (i, b)
      elemz              = nub $ map (fst . fst) $ MultiSet.toOccurList results
      trues x xs         = MultiSet.occur (x, True) xs
      falses x xs        = MultiSet.occur (x, False) xs
      accur (x, y)       = rounder $ (fromIntegral x) / ((fromIntegral x)+(fromIntegral y)::Double)::Double
      rounder x          = ((fromInteger $ round $ x * 1000) / 1000.0)::Double

  in map (\x -> (accur (trues x results, falses x results)
                ,x
                ,(trues x results, falses x results))
          ) elemz

---------------------
-- ANALYSIS
--------------------
type AnalysisScore = (Bool,Integer,(String,String),String)
type AnalysisScores = [AnalysisScore]

scoreMeasurer :: [(AnalysisScore, (Int, Int, Int))]
scoreMeasurer {-xs-} {-((b, i, (inp, gs), my):xs) -} = 
  let res = Aux.analysisResults
  in scoreMeasureFilter (\(_,(x,y,z)) -> x==0||y==0||z==0 ) $ map (applyScore $ pathDistanceToGoal) res
      
applyScore :: (Expr -> a) -> (AnalysisScore) -> (AnalysisScore,(a,a,a))
applyScore f (b,i,(inp,gs),my) = ((b,i,(inp,gs),my),(g inp, g gs, g my)) where
        g = f.decodeExpr


scoreMeasureFilter :: Eq a => ((AnalysisScore,(a,a,a)) -> Bool) -> [(AnalysisScore,(a,a,a))] -> [(AnalysisScore,(a,a,a))] 
scoreMeasureFilter f = filter f

      -- tmp      = filter (\(x, y) -> (f . decodeExpr) x) Aux.hoc18SingleEdit
      -- equality = (zipWith (==) (map (decodeExpr . snd) tmp) (map (\(inp, gs)-> t inp) tmp) )
      -- res      = (map (\(inp, gs)-> encodeExpr $ t inp) tmp)
-- accMeasure :: [(Bool, Int,(String, String), String)] -> [(Int, Int)]
-- accMeasure resS = 
--   allFalses = filter (\(x, y) -> y /= 0) $ zip [1..999] $ map (\i -> countFalse i resS) [1..999]
--   allTrues = filter (\(x, y) -> y /= 0) $ zip [1..999] $ map (\i -> countTrue i resS) [1..999]
--   countTrue i xs = length . filter (\x -> (extract x) == (i, True)) $ xs
--   countFalse i xs = length . filter (\x -> (extract x) == (i, False)) $ xs