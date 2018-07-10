--  Copyright (C) 2017 Joao Saraiva, Joao P. Fernandes, Pedro Martins,
--             Alberto Pardo, Marcos Viera
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE GADTs,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances
#-}

module Memo where

import Shared

{- memoized version -}

data Cons = CRoot | CFork | CLeaf Int

-- MemoAG --------------------------------------------

-- MemoTree

data Tree_m m 
  =  Fork_m m (Tree_m m) (Tree_m m)
  |  Leaf_m m Int 
 deriving Show

buildMemoTree :: m -> Tree -> Tree_m m
buildMemoTree m (Fork l r)
  = Fork_m m (buildMemoTree m l) (buildMemoTree m r) 
buildMemoTree m (Leaf i  )
  = Leaf_m m i                  

updMemoTable :: (m -> m) -> Tree_m m -> Tree_m m
updMemoTable f (Leaf_m m i)   = Leaf_m (f m) i
updMemoTable f (Fork_m m l r) = Fork_m (f m) l r

getMemoTable :: Tree_m m -> m
getMemoTable (Leaf_m m _)   = m
getMemoTable (Fork_m m _ _) = m

-- Memo Zipper

data Cxt_m m = Root_m 
             | Top_m 
             | L_m m (Cxt_m m)  (Tree_m m) 
             | R_m m (Tree_m m) (Cxt_m m)
 deriving Show

type Zipper_m m = (Tree_m m, Cxt_m m)

mkAG_m :: Tree_m m -> Zipper_m m
mkAG_m t = (t, Root_m)

tree_m :: Zipper_m m -> Zipper_m m
tree_m (t, Root_m) = (t, Top_m)  

left_m :: Zipper_m m -> Zipper_m m
left_m (Fork_m m l r, c) = (l, L_m m c r)
 
right_m :: Zipper_m m -> Zipper_m m
right_m (Fork_m m l r, c) = (r, R_m m l c)
 
up_m :: Zipper_m m -> Zipper_m m 
up_m (t, Top_m    ) = (t,       Root_m)
up_m (t, L_m m c r) = (Fork_m m t r, c)
up_m (t, R_m m l c) = (Fork_m m l t, c)
  
modify_m :: Zipper_m m -> (Tree_m m -> Tree_m m) -> Zipper_m m
modify_m (t, Root_m) f = (t, Root_m)
modify_m (t, c)      f = (f t, c)

constructor_m :: Zipper_m m -> Cons 
constructor_m (_,        Root_m) = CRoot
constructor_m (Leaf_m _ l,   _)  = CLeaf l
constructor_m (Fork_m _ _ _, _)  = CFork

-- MemoAG

type AGTree_m m a = Zipper_m m -> (a, Zipper_m m) 

eval .@. t = let (v,t') = eval t
             in  (v, up_m t') 

atParent eval t = let (v,t') = eval (up_m t)
                  in  (v, (back t) t')

back (_, Top_m    ) = tree_m 
back (_, L_m _ _ _) = left_m 
back (_, R_m _ _ _) = right_m

class Memo att m a where
  mlookup :: att -> m -> Maybe a
  massign :: att -> a -> m -> m

memo :: Memo attr m a => attr -> AGTree_m m a -> AGTree_m m a
memo attr eval z@(t,_) =
  case mlookup attr (getMemoTable t) of
        Just v   -> (v,z)
        Nothing  -> let (v,z') = eval z
                    in  (v, modify_m z' (updMemoTable (massign attr v)))

-- Repmin

data Globmin = Globmin
data Locmin  = Locmin
data Replace = Replace

type MemoTable = ( Maybe Int     -- Globmin
                 , Maybe Int     -- Locmin
                 , Maybe Tree    -- Replace
                 )
                 
emptyMemo = (Nothing,Nothing,Nothing)

instance Memo Globmin MemoTable Int where
  mlookup _   (g,_,_) = g
  massign _ v (g,l,r) = (Just v,l,r)

instance Memo Locmin MemoTable Int where
  mlookup _   (_,l,_) = l
  massign _ v (g,l,r) = (g,Just v,r)

instance Memo Replace MemoTable Tree where
  mlookup _   (_,_,r) = r
  massign _ v (g,l,r) = (g,l,Just v)


-- Inherited
globmin :: (Memo Globmin m Int, Memo Locmin m Int) => AGTree_m m Int
globmin = memo Globmin $ \z -> case constructor_m z of
                                 CRoot   -> locmin .@. tree_m z
                                 CLeaf _ -> globmin `atParent` z 
                                 CFork   -> globmin `atParent` z

-- Synthesized
locmin :: (Memo Locmin m Int) => AGTree_m m Int
locmin  = memo Locmin $ \z -> case constructor_m z of     
                                CLeaf v -> (v,z)
                                CFork   -> let (left ,z' ) = locmin .@. left_m  z  
                                               (right,z'') = locmin .@. right_m z' 
                                           in  (min left right, z'')

replace :: (Memo Replace m Tree, Memo Globmin m Int, Memo Locmin m Int) =>
           AGTree_m m Tree
replace = memo Replace $ \z -> case constructor_m z of     
                                 CRoot   -> replace .@. tree_m z
                                 CLeaf _ -> let (mini, z') = globmin z 
                                            in  (Leaf mini, z')
                                 CFork   -> let (l,z')  = replace .@. left_m  z  
                                                (r,z'') = replace .@. right_m z' 
                                            in  (Fork l r, z'')

semantics :: Tree -> Tree
semantics t = t'
  where (t',_) = replace z
        z :: Zipper_m MemoTable
        z = mkAG_m (buildMemoTree emptyMemo t)
