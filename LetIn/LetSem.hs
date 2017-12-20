
{-# LANGUAGE DeriveDataTypeable #-}

module LetSem where
import Language.Grammars.ZipperAG
import Data.Generics.Zipper
import Data.Dynamic
import Data.Maybe
import Data.Data
import LetShared

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
constructor ag =  case (getHole ag :: Maybe Root) of
                    Just (Root _) -> C_Root
                    _ -> case (getHole ag :: Maybe Let) of
                           Just (Let _ _) -> C_Let
                           _ -> case (getHole ag :: Maybe In) of
                                  Just (In _) -> C_In
                                  _ -> case (getHole ag :: Maybe List) of
                                         Just (ConsLet _ _ _   ) -> C_ConsLet
                                         Just (ConsAssign _ _ _) -> C_ConsAssign
                                         Just (EmptyList       ) -> C_EmptyList
                                         _ -> case (getHole ag :: Maybe A) of
                                                Just (Plus _ _  ) -> C_Plus
                                                Just (Minus _ _ ) -> C_Minus
                                                Just (Time _ _  ) -> C_Time
                                                Just (Divide _ _) -> C_Divide
                                                Just (Constant _) -> C_Constant
                                                Just (Variable _) -> C_Variable
                                                _ -> error "Error in constructor!!"

---- Synthesized Attributes ----
dclo :: Zipper Root -> SymbolTable
dclo ag = case (constructor ag) of
           C_Root       -> dclo $ ag.$1
           C_Let        -> dclo $ ag.$1
           C_ConsLet    -> dclo $ ag.$3
           C_ConsAssign -> dclo $ ag.$3
           C_EmptyList  -> dcli ag

errs :: Zipper Root -> [String]
errs ag = case (constructor ag) of
           C_Root       -> errs $ ag.$1
           C_Let        -> (errs $ ag.$1) ++ (errs $ ag.$2)
           C_In         -> (errs $ ag.$1)
           C_ConsAssign -> mNBIn (lexeme_ConsAssign_1 ag, lev ag) (dcli ag) ++ (errs $ ag.$2) ++ (errs $ ag.$3)
           C_ConsLet    -> mNBIn (lexeme_ConsLet_1    ag, lev ag) (dcli ag) ++ (errs $ ag.$2) ++ (errs $ ag.$3)
           C_EmptyList  -> []
           C_Plus        -> (errs $ ag.$1) ++ (errs $ ag.$2)
           C_Divide      -> (errs $ ag.$1) ++ (errs $ ag.$2)
           C_Minus       -> (errs $ ag.$1) ++ (errs $ ag.$2)
           C_Time        -> (errs $ ag.$1) ++ (errs $ ag.$2)
           C_Variable    -> mBIn (lexeme_Variable ag) (env ag)
           C_Constant    -> []

---- Inheritted Attributes ----
dcli :: Zipper Root -> SymbolTable
dcli ag = case (constructor ag) of
           C_Root -> []
           C_Let  -> case (constructor $ parent ag) of
                             C_Root    -> dcli $ parent ag
                             C_ConsLet -> env $ parent ag
           _       -> case (constructor $ parent ag) of
                             C_ConsAssign -> (dcli $ parent ag) ++ [(lexeme_ConsAssign_1 $ parent ag, lev $ parent ag)]
                             C_ConsLet    -> (dcli $ parent ag) ++ [(lexeme_ConsLet_1 $ parent ag, lev $ parent ag)]
                             _             -> dcli $ parent ag

env :: Zipper Root -> SymbolTable
env ag = case (constructor ag) of
           C_Root       -> dclo ag
           C_Let        -> case (constructor $ parent ag) of
                             C_ConsLet -> dclo ag
                             _          -> env $ parent ag
           -- autocopy, ow yeah
           _             -> env $ parent ag

lev :: Zipper Root -> Int
lev ag = case (constructor ag) of
           C_Root       -> 0
           C_Let        -> case (constructor $ parent ag) of
                             C_ConsLet -> (lev $ parent ag) + 1
                             _          -> 0
           _             -> lev $ parent ag

---- Calculate the result of evaluating Let ----
calculate :: Zipper Root -> Int
calculate ag = case (constructor ag) of
              C_Root       -> let errors = errs ag
                              in if (length errors /= 0)
                                 then 0 -- error $ "Errors found -> " ++ (show errors)
                                 else calculate $ ag.$1
              C_Let        -> calculate $ ag.$2
              C_In         -> calculate $ ag.$1
              C_Plus       -> (calculate $ ag.$1) + (calculate $ ag.$2)
              C_Divide     -> div (calculate $ ag.$1) (calculate $ ag.$2)
              C_Minus      -> (calculate $ ag.$1) - (calculate $ ag.$2)
              C_Time       -> (calculate $ ag.$1) * (calculate $ ag.$2)
              C_Variable   -> upGetVarValue (lexeme_Variable ag) ag
              C_Constant   -> lexeme_Constant ag

upGetVarValue :: String -> Zipper Root -> Int
upGetVarValue name ag = case (constructor ag) of
                       C_Root -> downGetVarValue name (ag.$1)
                       C_Let  -> downGetVarValue name ag
                       _ -> upGetVarValue name (parent ag)

downGetVarValue :: String -> Zipper Root -> Int
downGetVarValue name ag = case (constructor ag) of
                          C_Let        -> downGetVarValue name (ag.$1)
                          C_ConsAssign -> if   (lexeme_ConsAssign_1 ag == name)
                                          then (calculate $ ag.$2)
                                          else (downGetVarValue name (ag.$3))
                          C_ConsLet    -> if   (lexeme_ConsLet_1 ag == name)
                                          then (calculate $ ag.$2)
                                          else (downGetVarValue name (ag.$3))
                          C_EmptyList  -> oneUpGetVarValue name ag

oneUpGetVarValue :: String -> Zipper Root -> Int
oneUpGetVarValue name ag = case (constructor ag) of
                       C_Let -> upGetVarValue name (parent ag)
                       _   -> oneUpGetVarValue name (parent ag)

{- Environment lookup functions -}
mBIn :: String -> SymbolTable -> [String]
mBIn name [] = [name]
mBIn name ((n,l):es) = if (n==name) then [] else mBIn name es

mNBIn :: (String, Int) -> SymbolTable -> [String]
mNBIn tuple [] = [] 
mNBIn (a1,r1) ((a2,r2):es) = if (a1==a2) && (r1 == r2) then [a1] else mNBIn (a1,r1) es



semantics :: Root -> Int
semantics p = calculate $ (toZipper p)


nameAnalysis :: Root -> [String]
nameAnalysis p = errs $ (toZipper p)


evaluator :: Root -> (Int, [String])
evaluator p = (semantics p , nameAnalysis p)