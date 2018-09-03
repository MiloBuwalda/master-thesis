{-# LANGUAGE OverloadedStrings #-}

import qualified Expressions as E

import Text.LaTeX.Base
import Text.LaTeX.Packages.Trees.Qtree
import qualified Forest as F
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
  ress = fff (strExprTex a) <> fff (strExprTex b) <> fff (strExprTex c) where
    fff x = raw "\\" <> "fff" <> raw "{" <> x <> raw "} " <> raw "&" <> "\n" 

main :: IO ()
main = renderFile "tree.tex" example

mainX :: E.Expr -> IO ()
mainX e = renderFile "tree.tex" (expressionTex e)

mainS :: String -> IO ()
mainS s = renderFile "tree.tex" (strExprTex s)

example :: LaTeX
example = {-document-} theBody

expressionTex :: E.Expr -> LaTeX
expressionTex e = tree id (convertE e)

strExprTex :: String -> LaTeX
strExprTex s = F.tree id (convertE' (E.decodeExpr s))

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
