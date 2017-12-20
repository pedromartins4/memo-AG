module Shared where

-- Tree for ZAG1, ZAG2, ZAG3 and ZAG4

data Tree = Fork Tree Tree | Leaf Int
 deriving Show

t = Fork (Fork (Leaf 1)
               (Leaf 2))
         (Fork (Leaf 3)
               (Leaf 4))
