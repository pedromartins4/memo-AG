module Original where

import Shared

{- The original approach -}

-- Zipper

data Cxt = Root | L Cxt Tree | R Tree Cxt
 deriving Show

type Zipper = (Tree, Cxt)

--tree :: Zipper -> Zipper
--tree (t, Root) = (t, Top)  

left :: Zipper -> Zipper 
left (Fork l r, c) = (l, L c r)
 
right :: Zipper -> Zipper
right (Fork l r, c) = (r, R l c)
 
mkAG :: Tree -> Zipper 
mkAG t = (t, Root)
 
up :: Zipper -> Zipper 
--up (t, Top  ) = (t,   Root)
up (t, L c r) = (Fork t r, c)
up (t, R l c) = (Fork l t, c)

  
modify :: Zipper -> (Tree -> Tree) -> Zipper
modify (t, Root) _ = (t, Root)
modify (t, c)    f = (f t, c)


-- AG

type AGTree a = Zipper -> a

data Cons = CRoot | CFork | CLeaf Int
     deriving Show

constructor :: Zipper -> Cons
constructor (_,      Root) = CRoot
constructor (Leaf l,   _)  = CLeaf l
constructor (Fork _ _, _)  = CFork


-- Repmin

globmin :: AGTree Int
globmin t = case constructor t of
            CRoot   -> locmin t -- (tree t)
            CLeaf _ -> globmin (up t)
            CFork   -> globmin (up t)

locmin :: AGTree Int
locmin t = case constructor t of
            CLeaf l -> l 
            CFork   -> min (locmin (left  t))
                           (locmin (right t))


replace :: AGTree Tree
replace t = case constructor t of
            CRoot   -> replace t -- (tree t)
            CLeaf _ -> Leaf (globmin t)
            CFork   -> Fork (replace (left  t))
                            (replace (right t))


semantics tree = replace (mkAG tree)
