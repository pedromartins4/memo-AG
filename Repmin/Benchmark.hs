{-# LANGUAGE DeriveDataTypeable #-}

import Shared
import Original
import Memo
import Memo2
import Criterion

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

-- Code for memory consumption
original = (Original.semantics) (testTree 150000)
memoized = (Memo.semantics)     (testTree 150000)
memoized2 = (Memo2.semantics)     (testTree 150000)

main :: IO()
main = putStrLn . show $ original

-- main = defaultMain [
--   bgroup "Repmin" [ bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 40000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 50000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 60000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 70000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 80000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 90000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 100000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 120000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 130000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 140000)
--                   , bench "Original" $ nf (sumTree . (Original.semantics)) (testTree 150000)
--                   ]
--   ]

{-
main :: IO()
main = putStrLn . show $ original
-}
-- Code for speed test
{-
main = defaultMain [
  bgroup "Repmin" [ bench "Original 2300" $ nf (sumTree . (Original.semantics)) (testTree 2300)
                  , bench "Memo 2300" $ nf (sumTree . (Memo.semantics)) (testTree 2300)
                  , bench "Memo2 2300" $ nf (sumTree . (Memo2.semantics)) (testTree 2300)

                  , bench "Original 2600" $ nf (sumTree . (Original.semantics)) (testTree 2600)
                  , bench "Memo 2600" $ nf (sumTree . (Memo.semantics)) (testTree 2600)                  
                  , bench "Memo2 2600" $ nf (sumTree . (Memo2.semantics)) (testTree 2600)                  

                  , bench "Original 2900" $ nf (sumTree . (Original.semantics)) (testTree 2900)
                  , bench "Memo 2900" $ nf (sumTree . (Memo.semantics)) (testTree 2900)
                  , bench "Memo2 2900" $ nf (sumTree . (Memo2.semantics)) (testTree 2900)

                  , bench "Original 3200" $ nf (sumTree . (Original.semantics)) (testTree 3200)
                  , bench "Memo 3200" $ nf (sumTree . (Memo.semantics)) (testTree 3200)
                  , bench "Memo2 3200" $ nf (sumTree . (Memo2.semantics)) (testTree 3200)

                  , bench "Original 3500" $ nf (sumTree . (Original.semantics)) (testTree 3500)
                  , bench "Memo 3500" $ nf (sumTree . (Memo.semantics)) (testTree 3500)
                  , bench "Memo2 3500" $ nf (sumTree . (Memo2.semantics)) (testTree 3500)

                  , bench "Original 3800" $ nf (sumTree . (Original.semantics)) (testTree 3800)
                  , bench "Memo 3800" $ nf (sumTree . (Memo.semantics)) (testTree 3800)
                  , bench "Memo2 3800" $ nf (sumTree . (Memo2.semantics)) (testTree 3800)

                  , bench "Original 4100" $ nf (sumTree . (Original.semantics)) (testTree 4100)
                  , bench "Memo 4100" $ nf (sumTree . (Memo.semantics)) (testTree 4100)
                  , bench "Memo2 4100" $ nf (sumTree . (Memo2.semantics)) (testTree 4100)

                  , bench "Original 4400" $ nf (sumTree . (Original.semantics)) (testTree 4400)
                  , bench "Memo 4400" $ nf (sumTree . (Memo.semantics)) (testTree 4400)
                  , bench "Memo2 4400" $ nf (sumTree . (Memo2.semantics)) (testTree 4400)

                  , bench "Original 4700" $ nf (sumTree . (Original.semantics)) (testTree 4700)
                  , bench "Memo 4700" $ nf (sumTree . (Memo.semantics)) (testTree 4700)
                  , bench "Memo2 4700" $ nf (sumTree . (Memo2.semantics)) (testTree 4700)

                  , bench "Original 5000" $ nf (sumTree . (Original.semantics)) (testTree 5000)
                  , bench "Memo 5000" $ nf (sumTree . (Memo.semantics)) (testTree 5000)
                  , bench "Memo2 5000" $ nf (sumTree . (Memo2.semantics)) (testTree 5000)
                  ]
  ]
-}

