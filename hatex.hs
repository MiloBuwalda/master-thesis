{-# LANGUAGE OverloadedStrings #-}

import qualified Expressions as E
import Auxiliary
import Data.List
import Data.List.Split

import Data.Text (pack)
import Text.LaTeX.Base
import Text.LaTeX.Packages.Trees.Qtree
import qualified Forest as F
import qualified Dirtree as D
import Text.LaTeX.Packages.Inputenc

-- treeExample1 :: Tree String
-- treeExample1 = Node (Just "Root") [Leaf "Leaf 1",Leaf "Leaf 2"]

-- treeExample2 :: Tree Int
-- treeExample2 = Node (Just 0) [Node Nothing [Leaf 1,Leaf 2] , Leaf 3]

treeExtended :: Tree LaTeX
treeExtended = Node 
  (Just $ "repeat until (goal reached)")
    [ Node (Just $ textit "if (PathAhead)")
      [ Leaf "Forward", Leaf "TurnLeft"
      ]
    ]

treeShortened :: Tree LaTeX
treeShortened = Node 
  (Just $ "repeat")
    [ Node (Just $ textit "(PathAhead)")
      [ Leaf "Forward", Leaf "TurnLeft"
      ]
    ]


forestTMP :: F.Tree LaTeX
forestTMP = Node 
  (Just $ "repeat")
  [ Node (Just $ textit "(PathAhead)")
    [ Leaf "Forward", Leaf "TurnLeft"
    ]
  ]
-- Main



-- concat multiple Results 
resultsPrinter :: [(Bool,Int,(String,String),String)] -> IO ()
resultsPrinter xs = renderFile "tmp.tex" $ mconcat $ map resultToTrees xs

resultPrinter :: (Bool,Int,(String,String),String) -> IO ()
resultPrinter x = renderFile "tmp.tex" (resultToTrees x)

resultToTrees :: (Bool,Int,(String,String),String) -> LaTeX
resultToTrees (_,_,(a,b),c) = (ress) where
  ress :: LaTeX
  ress = forest (strExprTex a) <> raw "&" <> "\n"  <> forest (strExprTex b) <> raw "&" <> "\n"  <> forest (strExprTex c) <> (raw "&&\\\\") <> "\n\n" where
    forest x = raw "\\" <> "Forest" <> raw "{" <> "fff-compact" <> x <> raw "} "


-- concat multiple Results 
resultsDirPrinter :: [(Bool,Int,(String,String),String)] -> IO ()
resultsDirPrinter xs = renderFile "tmp-dir.tex" $ mconcat $ map resultToDirtree xs

resultDirPrinter :: (Bool,Int,(String,String),String) -> IO ()
resultDirPrinter x = renderFile "tmp-dir.tex" (resultToDirtree x)


resultToDirtree :: (Bool,Int,(String,String),String) -> LaTeX
resultToDirtree (_,_,(a,b),c) = (tableEnc) where
  tableEnc = ress -- tablePre <> ress <> tablePost
  ress :: LaTeX
  ress = dirtree a <> raw "&" <> "\n"  <> dirtree b <> raw "&" <> "\n"  <> dirtree c <> (raw "&&\\\\[1.5em]") <> "\n\n" where
    dirtree x = raw "\\scriptsize\n" <>dtHeight <> "\n" <> dtSetlength <> "\n" <> (encapsulateWith ["minipage", "[t]", raw ".13\\textwidth"] True $ raw "\\" <> "dirtree" <> raw "{%\\{" <> "\n" <> strExprTex' x <> raw "\n} ")

inconsistencies :: [(Bool,Int,(String,String),String)] -> IO ()
inconsistencies xs = renderFile "inconsistencies.tex" $ mconcat $ map inconsistency xs

inconsistency :: (Bool,Int,(String,String),String) -> LaTeX
inconsistency (_,_,(a,b),c) = (ress) where
  ress = dirtree a <> raw "&" <> "\n"  <> dirtree b <> raw "-->" <> "\n" where
    dirtree x = raw "\\scriptsize\n" <> dtHeight <> "\n" <> dtSetlength <> "\n" <> (encapsulateWith ["minipage", "[t]", raw ".13\\textwidth"] True $ raw "\\" <> "dirtree" <> raw "{%\\{" <> "\n" <> strExprTex' x <> raw "\n} ")

dtHeight :: LaTeX
dtHeight = command ["setlength",raw "\\DTbaselineskip","1.2em"] False

dtSetlength :: LaTeX
dtSetlength = command ["DTsetlength","0.2em",".4em","0.1em",".3pt","1.5pt"] False

tablePre :: LaTeX
tablePre = raw "\\begin{table*}[t]\n\\centering\n\\caption{Internal inconsistencies}\n\\label{tab:ass8-internal-incons}\n% \n\\scriptsize\n\\begin{tabularx}{\\textwidth}{lllp{.2\\textwidth}rr}\n\\hspace{1em}\\textit{Input} & \\hspace{1em}\\textit{GS hint} & \\hspace{1em}\\textit{Our hint} & \\hspace{1em}\\textit{Comment} & \\multicolumn{2}{l}{\\hspace{1em}\\textit{Internal inconsistencies}} \\\\[1ex] \\hline \\\\[-1.5ex]\n"

tablePost :: LaTeX
tablePost = raw "\\end{tabularx}\n\\end{table*}\n\n"

-- first is the command rest are options
command :: [LaTeX] -> Bool -> LaTeX
command args bracketArg = raw "\\" <> (head args) <> 
  if bracketArg 
    then (safeIndex args 1 "") <> mconcat (map opt (tail (tail args)))
    else mconcat (map opt (tail args)) where
  opt :: LaTeX -> LaTeX
  opt x = raw "{" <> x <> raw "}" 

encapsulateWith :: [LaTeX] -> Bool -> LaTeX -> LaTeX
encapsulateWith args bracketArg inp = raw "\\" <> "begin" <> raw "{" <> head args <> raw "}" <> options <> "\n" <> inp <> "\n" <> raw "\\" <> "end" <> raw "{" <> head args <> raw "}\n" where
  options = 
    if bracketArg 
      then safeIndex args 1 "" <> mconcat (map opt (tail (tail args)))
      else mconcat $ map opt (tail args)
  opt x = raw "{" <> x <> raw "}"
  -- option :: Int -> LaTeX
  -- option x = if (not . null) (get x) then raw "{" <> fromString (get x) <> raw "}" else fromString ""
  -- get x = safeIndex args x []

dirTreeTMP :: D.Tree LaTeX
dirTreeTMP = Node 
  (Just $ "repeat")
  [ Node (Just $ textit "(PathAhead)")
    [ Leaf "Forward", Leaf "TurnLeft"
    ]
  ]

dExample :: IO ()
dExample = renderFile "dirtree.tex" (F.tree id forestTMP <> "\n\n" <> D.tree id dirTreeTMP)

main :: IO ()
main = renderFile "tree.tex" example

mainX :: E.Expr -> IO ()
mainX e = renderFile "tree.tex" (expressionTex e)

mainS :: String -> IO ()
mainS s = renderFile "tree.tex" (strExprTex s)

mainDS :: String -> IO ()
mainDS s = renderFile "dirtree.tex" (encapsulateWith ["minipage", "[t]", "4cm"] True $ strExprTex' s)

example :: LaTeX
example = {-document-} theBody

expressionTex :: E.Expr -> LaTeX
expressionTex e = tree id (convertE e)

strExprTex :: String -> LaTeX
strExprTex s = F.tree id (convertE' (E.decodeExpr s))


strExprTex' :: String -> LaTeX
strExprTex' s = D.tree id (convertE'' (E.decodeExpr s))


convertE :: E.Expr      -> Tree LaTeX --String
convertE    (E.WHILE x)  = Node (Just $ "repeat") [convertE x]
convertE    (E.IF c l r) = Node (Just $ aBlock $ show c) [convertE l, convertE r] where
  aBlock x = mconcat [ "if " , textit $ fromString x] 
convertE    (E.SEQ xs)   = Node (Just $ "list") $map convertE xs
convertE    E.Forward    = Leaf "Forward"
convertE    E.TurnLeft   = Leaf "TurnLeft"
convertE    E.TurnRight  = Leaf "TurnRight"
convertE    E.Empty      = Leaf "Empty"

convertE' :: E.Expr      -> F.Tree LaTeX --String
convertE'    (E.WHILE x)  = Node (Just $ "repeat") [convertE' x]
convertE'    (E.IF c l r) = Node (Just $ aBlock $ show c) [convertE' l, convertE' r] where
  aBlock x = mconcat [ "if " , textit $ fromString x] 
convertE'    (E.SEQ xs)   = Node (Just $ "list") $map convertE' xs
convertE'    E.Forward    = Leaf "Forward"
convertE'    E.TurnLeft   = Leaf "TurnLeft"
convertE'    E.TurnRight  = Leaf "TurnRight"
convertE'    E.Empty      = Leaf "Empty"

convertE'' :: E.Expr      -> D.Tree LaTeX --String
convertE''    (E.WHILE x)  = Node (Just $ "repeat") [convertE'' x]
convertE''    (E.IF c l r) = Node (Just $ aBlock $ show c) [convertE'' l, convertE'' r] where
  aBlock x = mconcat [ "if " , textit $ fromString x] 
convertE''    (E.SEQ xs)   = Node (Just $ "list") $map convertE'' xs
convertE''    E.Forward    = Leaf "Forward"
convertE''    E.TurnLeft   = Leaf "TurnLeft"
convertE''    E.TurnRight  = Leaf "TurnRight"
convertE''    E.Empty      = Leaf "Empty"

-- convEmphDraw :: E.Expr -> 

theBody :: LaTeX
theBody =
    -- maketitle
 -- <> tree fromString treeExample1
 -- <> rendertree treeExample2
 {-<>-} tree id treeExtended
    <> tree id treeShortened

{-thePreamble :: LaTeX
thePreamble =
    documentclass [] article
 <> usepackage [] qtree
 <> usepackage [utf8] inputenc
 <> title "Examples with trees"
 <> author "Daniel Díaz"-}
