--  Copyright (C) 2017 Joao Saraiva, Joao P. Fernandes, Pedro Martins,
--  				   Alberto Pardo, Marcos Viera
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
--	along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveDataTypeable #-}

import Shared
import Original
import Memo
import Memo2
-- import Criterion.Main

testTree :: Int -> Tree
testTree n = buildTree [1..n]
   where buildTree :: [Int] -> Tree
         buildTree [a,b]   = Fork (Leaf a) (Leaf b)
         buildTree [a,b,c] = Fork (Fork (Leaf a) (Leaf b)) (Leaf c)
         buildTree list    = Fork (buildTree $ take half list) (buildTree $ drop half list)
                           where half =  div (length list) 2

sumTree :: Tree -> Int
sumTree (Leaf l  ) = l
sumTree (Fork l r) = sumTree l + sumTree r

---- Memory Profiling Benchmark ----
original  = (Original.semantics) (testTree 5)
memoized  = (Memo.semantics)     (testTree 5)
memoized2 = (Memo2.semantics)    (testTree 5)
main :: IO()
main = putStrLn . show $ (Original.semantics) (Leaf 3)
-- main = putStrLn . show $ memoized
-- main = putStrLn . show $ memoized2


---- Speed Benchmark ----
--main :: IO()
--main = defaultMain [
--  bgroup "Repmin" [ bench "Original 2" $ nf (sumTree . (Original.semantics)) (testTree 2)
--                  , bench "Memo 2"     $ nf (sumTree . (Memo.semantics))     (testTree 2)
--                  , bench "Memo2 2"    $ nf (sumTree . (Memo2.semantics))    (testTree 2)
--
--                  , bench "Original 4" $ nf (sumTree . (Original.semantics)) (testTree 4)
--                  , bench "Memo 4"     $ nf (sumTree . (Memo.semantics))     (testTree 4)                  
--                  , bench "Memo2 4"    $ nf (sumTree . (Memo2.semantics))    (testTree 4)                  
--
--                  , bench "Original 6" $ nf (sumTree . (Original.semantics)) (testTree 6)
--                  , bench "Memo 6"     $ nf (sumTree . (Memo.semantics))     (testTree 6)
--                  , bench "Memo2 6"    $ nf (sumTree . (Memo2.semantics))    (testTree 6)
--
--                  , bench "Original 8" $ nf (sumTree . (Original.semantics)) (testTree 8)
--                  , bench "Memo 8"     $ nf (sumTree . (Memo.semantics))     (testTree 8)
--                  , bench "Memo2 8"    $ nf (sumTree . (Memo2.semantics))    (testTree 8)
--
--                  , bench "Original 10" $ nf (sumTree . (Original.semantics)) (testTree 10)
--                  , bench "Memo 10"     $ nf (sumTree . (Memo.semantics))     (testTree 10)
--                  , bench "Memo2 10"    $ nf (sumTree . (Memo2.semantics))    (testTree 10)
--
--                  , bench "Original 12" $ nf (sumTree . (Original.semantics)) (testTree 12)
--                  , bench "Memo 12"     $ nf (sumTree . (Memo.semantics))     (testTree 12)
--                  , bench "Memo2 12"    $ nf (sumTree . (Memo2.semantics))    (testTree 12)
--
--                  , bench "Original 14" $ nf (sumTree . (Original.semantics)) (testTree 14)
--                  , bench "Memo 14"     $ nf (sumTree . (Memo.semantics))     (testTree 14)
--                  , bench "Memo2 14"    $ nf (sumTree . (Memo2.semantics))    (testTree 14)
--
--                  , bench "Original 16" $ nf (sumTree . (Original.semantics)) (testTree 16)
--                  , bench "Memo 16"     $ nf (sumTree . (Memo.semantics))     (testTree 16)
--                  , bench "Memo2 16"    $ nf (sumTree . (Memo2.semantics))    (testTree 16)
--
--                  , bench "Original 18" $ nf (sumTree . (Original.semantics)) (testTree 18)
--                  , bench "Memo 18"     $ nf (sumTree . (Memo.semantics))     (testTree 18)
--                  , bench "Memo2 18"    $ nf (sumTree . (Memo2.semantics))    (testTree 18)
--
--                  , bench "Original 20" $ nf (sumTree . (Original.semantics)) (testTree 20)
--                  , bench "Memo 20"     $ nf (sumTree . (Memo.semantics))     (testTree 20)
--                  , bench "Memo2 20"    $ nf (sumTree . (Memo2.semantics))    (testTree 20)
--                  ]
--  ]

