{-# LANGUAGE GADTs #-}

module Memo2 where

import Shared

{- memoized version -}

data Cons = CRoot | CFork | CLeaf Int


-- MemoAG --------------------------------------------

-- MemoTree

data Tree_m  
  =  Fork_m MemoTable  Tree_m Tree_m
  |  Leaf_m MemoTable  Int 
 deriving Show


data Att a where
 Globmin :: Att Int
 Locmin  :: Att Int
 Replace :: Att Tree

type MemoTable = ( Maybe Int )      -- Globmin
                 --, Maybe Int      -- Locmin
                 --, Maybe Tree )   -- Replace

emptyMemo = (Nothing) -- ,Nothing,Nothing)



lookupAttr :: Att a -> MemoTable -> Maybe a
lookupAttr Globmin v   = v  -- (v,_,_) = v
--lookupAttr Locmin     (_,v,_) = v
--lookupAttr Replace    (_,_,v) = v
lookupAttr  _ _ = Nothing

updAttr :: Att a -> Maybe a -> MemoTable -> MemoTable
updAttr Globmin v _ = v -- v   (_,l,r) = (v,l,r)
--updAttr Locmin  v   (g,_,r) = (g,v,r)
--updAttr Replace v   (g,l,_) = (g,l,v)
updAttr _ _ m = m
 
updMemoTable :: (MemoTable -> MemoTable) -> Tree_m -> Tree_m
updMemoTable f (Fork_m m l r) = Fork_m (f m) l r
updMemoTable f (Leaf_m m i)   = Leaf_m (f m) i 


 
buildMemoTree :: MemoTable -> Tree -> Tree_m
buildMemoTree m (Fork l r)
  = Fork_m m (buildMemoTree m l) (buildMemoTree m r) 
buildMemoTree m (Leaf i  )
  = Leaf_m m i                  


-- Memo Zipper

data Cxt_m = Root_m 
           | Top_m 
           | L_m   MemoTable Cxt_m Tree_m 
           | R_m   MemoTable Tree_m Cxt_m
 deriving Show

type Loc_m = (Tree_m, Cxt_m)

mkAG_m :: Tree_m -> Loc_m 
mkAG_m t = (t, Root_m)


tree_m :: Loc_m -> Loc_m
tree_m (t, Root_m) = (t, Top_m)  

left_m :: Loc_m -> Loc_m 
left_m (Fork_m m l r, c) = (l, L_m m c r)
 
right_m :: Loc_m -> Loc_m
right_m (Fork_m m l r, c) = (r, R_m m l c)
 
 
up_m :: Loc_m -> Loc_m 
up_m (t, Top_m    ) = (t,       Root_m)
up_m (t, L_m m c r) = (Fork_m m t r, c)
up_m (t, R_m m l c) = (Fork_m m l t, c)

  
modify_m :: Loc_m -> (Tree_m -> Tree_m) -> Loc_m
modify_m (t, Root_m) f = (t, Root_m)
modify_m (t, c)      f = (f t, c)

constructor_m (_,        Root_m) = CRoot
constructor_m (Leaf_m _ l,   _) = CLeaf l
constructor_m (Fork_m _ _ _, _) = CFork


-- MemoAG

type MemoAGTree a = Loc_m -> (a, Loc_m) 

eval .@. t = let (v,t') = eval t
             in  (v, up_m t') 

atLhs eval t = let (v,t') = eval (up_m t)
               in  (v, (back t) t')

back (_, Top_m    ) = tree_m 
back (_, L_m _ _ _) = left_m 
back (_, R_m _ _ _) = right_m

memo :: Att a -> MemoAGTree a -> MemoAGTree a
memo attr eval t =
  case lookupAttr attr (getMemoTable t) of
        Just v   -> (v,t)
        Nothing  -> let (v,t') = eval t
                    in  (v, modifyMemoTable (updAttr attr (Just v)) t')

getMemoTable t = case fst t of
                      Fork_m  m  _ _  -> m
                      Leaf_m  m  _    -> m

modifyMemoTable f t = modify_m t (updMemoTable f)

-- Repmin

locmin_m :: MemoAGTree Int
locmin_m  = memo Locmin $
    \t -> case constructor_m t of     
     CLeaf v -> (v,t)
     CFork   -> let (left,t')   = locmin_m .@. left_m  t  
                    (right,t'') = locmin_m .@. right_m t' 
                in  (min left right, t'')


globmin_m :: MemoAGTree Int
globmin_m = memo Globmin $
    \t -> case constructor_m t of
     CRoot   -> locmin_m .@. tree_m t
     CLeaf _ -> globmin_m `atLhs` t  
     CFork   -> globmin_m `atLhs` t

replace_m :: MemoAGTree Tree
replace_m = memo Replace $
    \t -> case constructor_m t of     
     CRoot   -> replace_m .@. tree_m t
     CLeaf _ -> let (mini, t') = globmin_m t 
                in  (Leaf mini, t')
     CFork   -> let (l,t')  = replace_m .@. left_m  t  
                    (r,t'') = replace_m .@. right_m t' 
                in  (Fork l r, t'')

semantics :: Tree -> Tree
semantics t = fst (replace_m zt)
           where zt = mkAG_m (buildMemoTree emptyMemo t)
