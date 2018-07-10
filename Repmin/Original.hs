--  Copyright (C) 2017 Joao Saraiva, Joao P. Fernandes, Pedro Martins,
--                     Alberto Pardo, Marcos Viera
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

module Original where

import Shared

{- The original approach -}

-- Zipper

data Cxt = Root | Top | L Cxt Tree | R Tree Cxt
 deriving Show

type Zipper = (Tree, Cxt)

tree :: Zipper -> Zipper
tree (t, Root) = (t, Top)  

left :: Zipper -> Zipper 
left (Fork l r, c) = (l, L c r)
 
right :: Zipper -> Zipper
right (Fork l r, c) = (r, R l c)
 
mkAG :: Tree -> Zipper 
mkAG t = (t, Root)
 
up :: Zipper -> Zipper 
up (t, Top  ) = (t,   Root)
up (t, L c r) = (Fork t r, c)
up (t, R l c) = (Fork l t, c)

  
modify :: Zipper -> (Tree -> Tree) -> Zipper
modify (t, Root) _ = (t, Root)
modify (t, c)    f = (f t, c)


-- AG

type AGTree a = Zipper -> a

data Cons = CRoot | CFork | CLeaf Int

constructor :: Zipper -> Cons
constructor (_,      Root) = CRoot
constructor (Leaf l,   _)  = CLeaf l
constructor (Fork _ _, _)  = CFork


-- Repmin

globmin :: AGTree Int
globmin t = case constructor t of
            CRoot   -> locmin  (tree t)
            CLeaf _ -> globmin (up t)
            CFork   -> globmin (up t)

locmin :: AGTree Int
locmin t = case constructor t of
            CLeaf l -> l 
            CFork   -> min (locmin (left  t))
                           (locmin (right t))


replace :: AGTree Tree
replace t = case constructor t of
            CRoot   -> replace (tree t)
            CLeaf _ -> Leaf (globmin t)
            CFork   -> Fork (replace (left  t))
                            (replace (right t))


semantics tree = replace (mkAG tree)