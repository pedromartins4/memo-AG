{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics.Zipper
-- import Criterion.Main
import Data.Maybe
import Data.Data

import Shared
import Original
import Memo

testTree2 :: Int -> Int -> Root
testTree2 r e = Root $ RootTable (rows e r)
 where 
       rows e 0 = NoRow
       rows e n = ConsRow (OneRow $ rep e n e) (rows e (n-1)) 

       rep _ _ 0 = NoElem
       rep e n 1 = ConsElem (NestedTable $ RootTable (rows (div e 2) (div n 2)))  NoElem
       rep e n m = ConsElem (TableText "elem") (rep e n (m-1))


-- Adjust the arguments of testTree according to desired input size
original = (Original.semantics) (testTree2 15 15)
memoized = (Memo.semantics) (testTree2 30 30)

---- Memory Profiling Benchmark ----
main :: IO()
main = putStrLn . show . last  $ original
-- main = putStrLn . show . last  $ memoized

---- Speeed Benchmark ----
--main :: IO()
--main = defaultMain [
--  bgroup "HTML" [ bench "HTML Original 1 1"   $ whnf (last . Original.semantics) (testTree2 1 1) 
--                , bench "HTML Memo 1 1"       $ whnf (last . Memo.semantics)     (testTree2 1 1)
--                , bench "HTML Original 2 2"   $ whnf (last . Original.semantics) (testTree2 2 2)
--                , bench "HTML Memo 2 2"       $ whnf (last . Memo.semantics)     (testTree2 2 2)
--                , bench "HTML Original 3 3"   $ whnf (last . Original.semantics) (testTree2 3 3) 
--                , bench "HTML Memo 3 3"       $ whnf (last . Memo.semantics)     (testTree2 3 3)
--                , bench "HTML Original 4 4"   $ whnf (last . Original.semantics) (testTree2 4 4) 
--                , bench "HTML Memo 4 4"       $ whnf (last . Memo.semantics)     (testTree2 4 4)
--                , bench "HTML Original 5 5"   $ whnf (last . Original.semantics) (testTree2 5 5) 
--                , bench "HTML Memo 5 5"       $ whnf (last . Memo.semantics)     (testTree2 5 5)
--                , bench "HTML Original 6 6"   $ whnf (last . Original.semantics) (testTree2 6 6) 
--                , bench "HTML Memo 6 6"       $ whnf (last . Memo.semantics)     (testTree2 6 6)
--                , bench "HTML Original 7 7"   $ whnf (last . Original.semantics) (testTree2 7 7) 
--                , bench "HTML Memo 7 7"       $ whnf (last . Memo.semantics)     (testTree2 7 7)
--                , bench "HTML Original 8 8"   $ whnf (last . Original.semantics) (testTree2 8 8) 
--                , bench "HTML Memo 8 8"       $ whnf (last . Memo.semantics)     (testTree2 8 8)
--                , bench "HTML Original 9 9"   $ whnf (last . Original.semantics) (testTree2 9 9) 
--                , bench "HTML Memo 9 9"       $ whnf (last . Memo.semantics)     (testTree2 9 9)
--                , bench "HTML Original 10 10" $ whnf (last . Original.semantics) (testTree2 10 10) 
--                , bench "HTML Memo 10 10"     $ whnf (last . Memo.semantics)     (testTree2 10 10)
--                ]
--                ]
--