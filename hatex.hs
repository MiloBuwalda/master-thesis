{-# LANGUAGE OverloadedStrings #-}

import qualified Expressions as E
import Auxiliary
import Data.List

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
resultsPrinter xs = renderFile "tree.tex" $ mconcat $ map resultToTrees xs

resultPrinter :: (Bool,Int,(String,String),String) -> IO ()
resultPrinter x = renderFile "tree.tex" (resultToTrees x)

resultToTrees :: (Bool,Int,(String,String),String) -> LaTeX
resultToTrees (_,_,(a,b),c) = (ress) where
  ress :: LaTeX
  ress = forest (strExprTex a) <> raw "&" <> "\n"  <> forest (strExprTex b) <> raw "&" <> "\n"  <> forest (strExprTex c) <> (raw "&&\\\\") <> "\n\n" where
    forest x = raw "\\" <> "Forest" <> raw "{" <> "fff-compact" <> x <> raw "} "


-- concat multiple Results 
resultsDirPrinter :: [(Bool,Int,(String,String),String)] -> IO ()
resultsDirPrinter xs = renderFile "dirtree.tex" $ mconcat $ map resultToDirtree xs

resultDirPrinter :: (Bool,Int,(String,String),String) -> IO ()
resultDirPrinter x = renderFile "dirtree.tex" (resultToTrees x)


resultToDirtree :: (Bool,Int,(String,String),String) -> LaTeX
resultToDirtree (_,_,(a,b),c) = (ress) where
  ress :: LaTeX
  ress = dirtree a <> raw "&" <> "\n"  <> dirtree b <> raw "&" <> "\n"  <> dirtree c <> (raw "&&\\\\") <> "\n\n" where
    dirtree x = encapsulateWith ["minipage", "4cm"] $ raw "\\" <> "dirtree" <> raw "{%\\{" <> "\n" <> strExprTex' x <> raw "\n} "


setOptions :: [String] -> LaTeX
setOptions args = raw "\\" <> fromString (head args) <> mconcat $ map opt (tail args) where
  opt x = raw "{" <> fromString x <> raw "}" 

encapsulateWith :: [String] -> LaTeX -> LaTeX
encapsulateWith args inp = raw "\\" <> "begin" <> raw "{" <> fromString (head args) <> raw "}" <> options <> "\n" <> inp <> "\n" <> raw "\\" <> "end" <> raw "{" <> fromString (head args) <> raw "}\n" where
  options = mconcat $ map option' (tail args)
  option :: Int -> LaTeX
  option' x = raw "{" <> fromString x <> raw "}"
  option x = if (not . null) (get x) then raw "{" <> fromString (get x) <> raw "}" else fromString ""
  get x = safeIndex args x []

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
mainDS s = renderFile "dirtree.tex" (encapsulateWith ["minipage", "4cm"] $ strExprTex' s)

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
