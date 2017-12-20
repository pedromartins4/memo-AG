{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Language.Grammars.ZipperAG
import Data.Generics.Zipper
import Criterion.Main
import Data.Maybe
import Data.Data
import Data.Dynamic

import  LetShared
import  LetSem
import  LetSem_Algol68_mBIn_Rec
import  LetSem_Memo_Algol68_mBIn_Rec
import  LetSem_Algol68_mBIn_Rec_FullMemo

--let a = b + 3
--    c = 8
--    w = let  z = a * b
--        in   z * b   
--    b = (c * 3) - c
--in  (c * w) - a
t = (Let
        (ConsAssign "a" (Plus (Variable "b") (Constant 3)) 
        (ConsAssign "c" (Constant 8) 
        (ConsLet    "w" (Let (ConsAssign "z" (Time (Variable "a") (Variable "b")) EmptyList) 
                        (In  (Time (Variable "z") (Variable "b"))))
        (ConsAssign "b" (Minus (Time (Variable "c") (Constant 3)) (Variable "c")) EmptyList))))
    (In (Minus (Time (Variable "e") (Variable "w")) (Variable "a"))))

testTree a = Root $ repeeat a
  where
  	repeeat 0 = t
  	repeeat x = (Let
                    (ConsAssign "a" (Plus (Variable "b") (Constant 3)) 
                    (ConsAssign "c" (Constant 8) 
                    (ConsLet    "w" (repeeat (x-1))
                    (ConsAssign "b" (Minus (Time (Variable "c") (Constant 3)) (Variable "c")) EmptyList))))
                (In (Minus (Time (Variable "c") (Variable "w")) (Variable "a"))))
		
testTree' :: Int -> Root
testTree' a = Root (Let (aux a) (In (Variable ("a_" ++ (show a)))))
  where aux :: Int -> List
        aux 0 =  ConsAssign "a_0" (Constant 10) EmptyList
        aux n = ConsAssign ("a_" ++ show n)
	                   (Plus (Variable ("a_" ++ show (n-1))) (Constant 1))
			   (aux (n - 1))
        
testTree'' :: Int -> Root
testTree'' n = Root (Let
                         (ConsAssign "a" (Plus (Variable "b") (Constant 3)) 
                         (ConsAssign "c" (Constant 8) 
                         (ConsLet    "w" (Let ((letGeneratorAux n) EmptyList) 
                                         (In  (Time (Variable "z") (Variable "b"))))
                         (ConsAssign "b" (Minus (Time (Variable "c") (Constant 3)) (Variable "c")) EmptyList))))
                     (In (Minus (Time (Variable "c") (Variable "w")) (Variable "a"))))

letGeneratorAux 0 = ConsAssign "a" (Plus (Variable "b") (Constant 3))
letGeneratorAux n = ConsLet    "w" (Let ((letGeneratorAux (n-1)) EmptyList)
                                   (In  (Time (Variable "z") (Variable "b"))))




genLetProg :: Int -> Root
genLetProg n = Root (Let (genLetTree n)
                     (In (Variable ("n_" ++ show n)))
                    )

genLetTree :: Int -> List
genLetTree n = addList (genNestedLets n)
 where addList (ConsLet s l ll) = ConsLet s l (addList' ll)

       addList' EmptyList = ConsAssign "va" (Constant 10)
                             (ConsAssign "vb" (Constant 20) EmptyList)
       addList' (ConsLet s l ll) = ConsLet s l (addList' ll)
       addList' (ConsAssign s a ll) = ConsAssign s a (addList' ll)


genNestedLets :: Int -> List
genNestedLets 0 = EmptyList
genNestedLets n
  | n == 1 =  ConsLet ("n_"++(show 1))
                          ( Let oneList 
			    (In (Plus (Variable ("z_" ++ (show 10)))
			              (Variable ("z_" ++ (show 9)))
		                )
		            )
			  )
			  (genListAssign (n*10)) 
  | n > 1 = ConsLet ("n_"++(show n))
                          ( Let oneList 
			    (In (Plus (Variable ("n_" ++ (show (n-1))))
			              (Variable ("z_" ++ (show ((n*10)-1))))
		                )
		            )
			  )
			  (genListAssign (n*10))
  where
        oneList = ConsAssign ("zz_" ++ (show n)) (Constant 10)
                   (ConsAssign ("zz_"++ (show (n-1))) (Variable "va")
                      (genNestedLets (n-1))
                   )

		

genListAssign :: Int -> List
genListAssign 0 = ConsAssign "z_0" (Constant 10) EmptyList
genListAssign n
  | n `mod` 9 == 0 = ConsAssign ("z_" ++ show n) (Variable "va")
                           (genListAssign (n - 1))
  | otherwise       = ConsAssign ("z_" ++ show n)
	                   (Plus (Variable ("z_" ++ show (n-1))) (Constant 1))
			   (genListAssign (n - 1))



-- For profiling individual executions

-- LetSem (no memo)
-- main :: IO()
-- main = putStrLn . show . (LetSem.semantics) $ (genLetProg 5)

-- LetSem HO Algol68 (no memo)
-- main :: IO ()
-- main = putStrLn . show . (LetSem_Algol68_mBIn_Rec.semantics) $ (genLetProg 5)

-- LetSem HO Full Memo
-- main :: IO()
-- main = putStrLn . show . (LetSem_Algol68_mBIn_Rec_FullMemo.semantics) $ (genLetProg 30)

-- LetSem HO (Algol Memo)
-- main :: IO()
-- main = putStrLn . show . (LetSem_Memo_Algol68_mBIn_Rec.semantics) $ (genLetProg 30)

main :: IO ()
main = defaultMain 
  [ bgroup "Let"
      [ bench "LetSem (no memo) 6" $ nf ((LetSem.semantics)) (genLetProg 6)
      , bench "LetSem HO Algol68 (no memo) 6" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 6)
      , bench "LetSem HO (Algol Memo) 6" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 6)
      , bench "LetSem HO Full Memo 6" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 6)
      
      , bench "LetSem (no memo) 7" $ nf ((LetSem.semantics)) (genLetProg 7)
      , bench "LetSem HO Algol68 (no memo) 7" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 7)
      , bench "LetSem HO (Algol Memo) 7" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 7)
      , bench "LetSem HO Full Memo 7" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 7)

      , bench "LetSem (no memo) 8" $ nf ((LetSem.semantics)) (genLetProg 8)
      , bench "LetSem HO Algol68 (no memo) 8" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 8)
      , bench "LetSem HO (Algol Memo) 8" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 8)
      , bench "LetSem HO Full Memo 8" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 8)

      , bench "LetSem (no memo) 9" $ nf ((LetSem.semantics)) (genLetProg 9)
      , bench "LetSem HO Algol68 (no memo) 9" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 9)
      , bench "LetSem HO (Algol Memo) 9" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 9)
      , bench "LetSem HO Full Memo 9" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 9)

      , bench "LetSem (no memo) 10" $ nf ((LetSem.semantics)) (genLetProg 10)
      , bench "LetSem HO Algol68 (no memo) 10" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 10)
      , bench "LetSem HO (Algol Memo) 10" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 10)
      , bench "LetSem HO Full Memo 10" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 10)

      , bench "LetSem (no memo) 11" $ nf ((LetSem.semantics)) (genLetProg 11)
      , bench "LetSem HO Algol68 (no memo) 11" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 11)
      , bench "LetSem HO (Algol Memo) 11" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 11)
      , bench "LetSem HO Full Memo 11" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 11)

      , bench "LetSem (no memo) 12" $ nf ((LetSem.semantics)) (genLetProg 12)
      , bench "LetSem HO Algol68 (no memo) 12" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 12)
      , bench "LetSem HO (Algol Memo) 12" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 12)
      , bench "LetSem HO Full Memo 12" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 12)

      , bench "LetSem (no memo) 13" $ nf ((LetSem.semantics)) (genLetProg 13)
      , bench "LetSem HO Algol68 (no memo) 13" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 13)
      , bench "LetSem HO (Algol Memo) 13" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 13)
      , bench "LetSem HO Full Memo 13" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 13)

      , bench "LetSem (no memo) 14" $ nf ((LetSem.semantics)) (genLetProg 14)
      , bench "LetSem HO Algol68 (no memo) 14" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 14)
      , bench "LetSem HO (Algol Memo) 14" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 14)
      , bench "LetSem HO Full Memo 14" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 14)

      , bench "LetSem (no memo) 15" $ nf ((LetSem.semantics)) (genLetProg 15)
      , bench "LetSem HO Algol68 (no memo) 15" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 15)
      , bench "LetSem HO (Algol Memo) 15" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 15)
      , bench "LetSem HO Full Memo 15" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 15)


      , bench "LetSem (no memo) 16" $ nf ((LetSem.semantics)) (genLetProg 16)
      , bench "LetSem HO Algol68 (no memo) 16" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 16)
      , bench "LetSem HO (Algol Memo) 16" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 16)
      , bench "LetSem HO Full Memo 16" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 16)


      , bench "LetSem (no memo) 17" $ nf ((LetSem.semantics)) (genLetProg 17)
      , bench "LetSem HO Algol68 (no memo) 17" $ nf ((LetSem_Algol68_mBIn_Rec.semantics)) (genLetProg 17)
      , bench "LetSem HO (Algol Memo) 17" $ nf ((LetSem_Memo_Algol68_mBIn_Rec.semantics)) (genLetProg 17)
      , bench "LetSem HO Full Memo 17" $ nf ((LetSem_Algol68_mBIn_Rec_FullMemo.semantics)) (genLetProg 17)

      ]
  ]

