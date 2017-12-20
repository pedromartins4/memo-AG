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
main = putStrLn . show $ original
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

