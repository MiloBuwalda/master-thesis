{-# LANGUAGE OverloadedStrings, CPP #-}
#if __GLASGOW_HASKELL__ >= 801
{-# OPTIONS_GHC -Wno-orphans #-}
#else
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

-- | Tree interface using the @forest@ package.
--   An example of usage is provided in the /examples/ directory of
--   the source distribution.
module Dirtree (
    -- * Tree re-export
    module Text.LaTeX.Packages.Trees
    -- * DirTree package
  , dirtree
    -- * Tree to LaTeX rendering
  , tree
  , rendertree
  ) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Trees
--
import Data.List (intersperse)

-- | The 'forest' package.
dirtree :: PackageName
dirtree = "dirtree"

tree_ :: LaTeXC l => (a -> l) -> Tree a -> Int -> l
tree_ f (Leaf x) i = mconcat [fromString (concat (replicate i "\t")),".", fromString (show i), " ", f x,"."]
tree_ f (Node mx ts) i =
  mconcat [ fromString (concat (replicate i "\t")), ".", fromString (show i), " " , maybe mempty (f) mx
          , ".\n"
          , mconcat $ intersperse ({-fromString $ -}"\n" {-++ show i ++ ". "-})$ fmap (\x -> tree_ f x (i+1)) ts 
          -- , "\n"
            ]

-- | Given a function to @LaTeX@ values, you can create a @LaTeX@ tree from a
--   Haskell tree. The function specifies how to render the node values.
tree :: LaTeXC l => (a -> l) -> Tree a -> l
tree f t = tree_ f t 1

-- | Instance defined in "Text.LaTeX.Packages.Trees.Qtree".
instance Texy a => Texy (Tree a) where
 texy = tree texy

-- | This function works as 'tree', but use 'render' as rendering function.
rendertree :: (Render a, LaTeXC l) => Tree a -> l
rendertree = tree (raw . protectText . render)