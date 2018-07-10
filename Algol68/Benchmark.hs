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

import Language.Grammars.ZipperAG
import Data.Generics.Zipper
-- import Criterion.Main
import Data.Maybe
import Data.Data

import Shared
import Original
import Memo

t = ConsIts (Decl "y") (ConsIts (Decl "x") (ConsIts (Use "x") (ConsIts (Use "y") (NilIts))))

test_NestedLevel :: Int -> Root
test_NestedLevel n = Root $ repeeat n
  where
    repeeat 0 = t
    repeeat x = ConsIts (Decl "y") (ConsIts (Decl "x") (ConsIts (Block $ repeeat (x-1)) (ConsIts (Use "x") (ConsIts (Use "y") (NilIts)))))

test_UseTopLevelDecl :: Int -> Root
test_UseTopLevelDecl n = Root (ConsIts  (Decl "varRoot") (repeeat n))
  where
    repeeat 0 = t
    repeeat x = ConsIts (Decl "y") (ConsIts (Decl "x") (ConsIts (Block $ repeeat (x-1)) (ConsIts (Use "varRoot") (ConsIts (Use "y") (NilIts)))))

testTree a = Root $ repeeat a
  where
    repeeat 0 = t
    repeeat x = ConsIts (Decl "y") (ConsIts (Decl "x") (ConsIts (Block $ repeeat (x-1)) (ConsIts (Use "varRoot") (ConsIts (Use "y") (NilIts)))))

-- Change the argument of testTree according to the
-- desired test input
original = (Original.semantics) (testTree 10)
memoized = (Memo.semantics)     (testTree 10)

---- Mempory consumption ----
main :: IO()
main = putStrLn . show . head $ original
-- main = putStrLn . show . head $ memoized


---- Performance usage ----
--main :: IO()
--main = defaultMain [
--  bgroup "Algol" [ bench "Original 10" $ whnf (Original.semantics) (testTree 10)
--                 , bench "Memo 10"     $ whnf (Memo.semantics)     (testTree 10)
--                 , bench "Original 20" $ whnf (Original.semantics) (testTree 20)
--                 , bench "Memo 20"     $ whnf (Memo.semantics)     (testTree 20)
--                 , bench "Original 30" $ whnf (Original.semantics) (testTree 30)
--                 , bench "Memo 30"     $ whnf (Memo.semantics)     (testTree 30)
--                 , bench "Original 40" $ whnf (Original.semantics) (testTree 40)
--                 , bench "Memo 40"     $ whnf (Memo.semantics)     (testTree 40)
--                 , bench "Original 50" $ whnf (Original.semantics) (testTree 50)
--                 , bench "Memo 50"     $ whnf (Memo.semantics)     (testTree 50)
--                 , bench "Original 60" $ whnf (Original.semantics) (testTree 60)
--                 , bench "Memo 60"     $ whnf (Memo.semantics)     (testTree 60)
--                 , bench "Original 70" $ whnf (Original.semantics) (testTree 70)
--                 , bench "Memo 70"     $ whnf (Memo.semantics)     (testTree 70)
--                 , bench "Original 80" $ whnf (Original.semantics) (testTree 80)
--                 , bench "Memo 80"     $ whnf (Memo.semantics)     (testTree 80)
--                 , bench "Original 90" $ whnf (Original.semantics) (testTree 90)
--                 , bench "Memo 90"     $ whnf (Memo.semantics)     (testTree 90)
--                 , bench "Original 100" $ whnf (Original.semantics) (testTree 100)
--                 , bench "Memo 100"     $ whnf (Memo.semantics)     (testTree 100)
--                 ]
--  ]

