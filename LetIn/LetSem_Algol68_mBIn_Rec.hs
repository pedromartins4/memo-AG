
{-# LANGUAGE DeriveDataTypeable, ImpredicativeTypes, GADTs #-}


--  Let 
--    Name Analysis: Algol68
--              (recursive lookup funs)
--



module LetSem_Algol68_mBIn_Rec where
import Language.Grammars.ZipperAG
import Control.Monad.State.Lazy
import Data.Generics.Zipper
import Data.Dynamic
import Data.Maybe
import Data.Data

import LetShared as Let

import AlgolShared as Algol hiding ((.$>),(.$<))
import AlgolSem_mBIn_Rec hiding (Constructor, constructor, C_Root, Symboltable)

lexeme_ConsAssign_1 :: Zipper a -> String
lexeme_ConsAssign_1 ag = case (getHole ag :: Maybe List) of
                            Just(ConsAssign v _ _) -> v
                            _ -> error "Error in lexeme_ConsAssign_1!"

lexeme_ConsLet_1 :: Zipper a -> String
lexeme_ConsLet_1 ag = case (getHole ag :: Maybe List) of
                         Just(ConsLet v _ _) -> v
                         _ -> error "Error in lexeme_ConsLet!"

lexeme_Variable :: Zipper a -> String
lexeme_Variable ag = case (getHole ag :: Maybe A) of
                       Just (Variable s) -> s
                       _ -> error "Error in lexeme_Variable!"

lexeme_Constant :: Zipper a -> Int
lexeme_Constant ag = case (getHole ag :: Maybe A) of
                       Just (Constant s) -> s
                       _ -> error "Error in lexeme_Constant!"

data Constructor = C_Root
                 | C_Let
                 | C_In
                 | C_ConsLet
                 | C_ConsAssign
                 | C_EmptyList
                 | C_Plus
                 | C_Minus
                 | C_Time
                 | C_Divide
                 | C_Constant
                 | C_Variable

constructor :: Zipper a -> Constructor
constructor ag =  case (getHole ag :: Maybe Let.Root) of
                    Just (Let.Root _) -> C_Root
                    _ -> case (getHole ag :: Maybe Let.Let) of
                           Just (Let.Let _ _) -> C_Let
                           _ -> case (getHole ag :: Maybe Let.In) of
                                  Just (Let.In _) -> C_In
                                  _ -> case (getHole ag :: Maybe Let.List) of
                                         Just (Let.ConsLet _ _ _   ) -> C_ConsLet
                                         Just (Let.ConsAssign _ _ _) -> C_ConsAssign
                                         Just (Let.EmptyList       ) -> C_EmptyList
                                         _ -> case (getHole ag :: Maybe Let.A) of
                                                Just (Let.Plus _ _  ) -> C_Plus
                                                Just (Let.Minus _ _ ) -> C_Minus
                                                Just (Let.Time _ _  ) -> C_Time
                                                Just (Let.Divide _ _) -> C_Divide
                                                Just (Let.Constant _) -> C_Constant
                                                Just (Let.Variable _) -> C_Variable
                                                _ -> error "Error in constructor!!"

---- Synthesized Attributes ----
-- Name Analysis via Algol68 scope rules


algol68 :: Zipper Let.Root -> Algol.Its
algol68 ag = case (constructor ag) of
          C_Root       -> algol68 $ ag.$1
          C_Let        -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_In         -> algol68 $ ag.$1
          C_ConsAssign ->
           concatIts
            (Algol.ConsIts (Algol.Decl (lexeme_ConsAssign_1 ag))
                           (algol68 $ ag.$2))
            (algol68 $ ag.$3)
          C_ConsLet    -> Algol.ConsIts
                        (Algol.Decl (lexeme_ConsLet_1 ag))
                    (Algol.ConsIts  
                           (Algol.Block (algol68 $ ag.$2))
                   (algol68 $ ag.$3) 
                   )

          C_EmptyList  -> Algol.NilIts
          C_Plus       -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_Divide     -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_Minus      -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_Time       -> concatIts (algol68 $ ag.$1) (algol68 $ ag.$2)
          C_Variable   -> Algol.ConsIts (Algol.Use (lexeme_Variable ag)) Algol.NilIts 
          C_Constant   -> Algol.NilIts



errsAlgol :: Zipper Let.Root -> [String]
errsAlgol ag = case (constructor ag) of
                 C_Root       -> AlgolSem_mBIn_Rec.semantics (Algol.Root (algol68 ag))


---- Calculate the result of evaluating Let ----
calculate :: Zipper Let.Root -> Int
calculate ag = case (constructor ag) of
              C_Root       -> let errors = errsAlgol ag
                              in if (length errors /= 0)
                                 then 0 -- error $ "Errors found -> " ++ (show errors)
                                 else  calculate $ ag.$1
              C_Let        -> calculate $ ag.$2
              C_In         -> calculate $ ag.$1
              C_Plus       -> (calculate $ ag.$1) + (calculate $ ag.$2)
              C_Divide     -> div (calculate $ ag.$1) (calculate $ ag.$2)
              C_Minus      -> (calculate $ ag.$1) - (calculate $ ag.$2)
              C_Time       -> (calculate $ ag.$1) * (calculate $ ag.$2)
              C_Variable   -> upGetVarValue (lexeme_Variable ag) ag
              C_Constant   -> lexeme_Constant ag

upGetVarValue :: String -> Zipper Let.Root -> Int
upGetVarValue name ag = case (constructor ag) of
                       C_Root -> downGetVarValue name (ag.$1)
                       C_Let  -> downGetVarValue name ag
                       _ -> upGetVarValue name (parent ag)

downGetVarValue :: String -> Zipper Let.Root -> Int
downGetVarValue name ag = case (constructor ag) of
                          C_Let        -> downGetVarValue name (ag.$1)
                          C_ConsAssign -> if   (lexeme_ConsAssign_1 ag == name)
                                          then (calculate $ ag.$2)
                                          else (downGetVarValue name (ag.$3))
                          C_ConsLet    -> if   (lexeme_ConsLet_1 ag == name)
                                          then (calculate $ ag.$2)
                                          else (downGetVarValue name (ag.$3))
                          C_EmptyList  -> oneUpGetVarValue name ag

oneUpGetVarValue :: String -> Zipper Let.Root -> Int
oneUpGetVarValue name ag = case (constructor ag) of
                       C_Let -> upGetVarValue name (parent ag)
                       _   -> oneUpGetVarValue name (parent ag)


semantics :: Let.Root -> Int
semantics p = calculate $ (toZipper p)

nameAnalysis :: Let.Root -> [String]
nameAnalysis p = errsAlgol $ (toZipper p)


evaluator :: Let.Root -> (Int, [String])
evaluator p = (LetSem_Algol68_mBIn_Rec.semantics p , nameAnalysis p)


