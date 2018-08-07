{-# LANGUAGE DeriveDataTypeable, GADTs #-}
module Desk.Old.Memo where

import Language.Grammars.ZipperAG
import Control.Monad.Trans.State.Lazy
import Data.Generics.Zipper
import Data.Maybe
import Data.Data

import Desk


data Root = Root Program
  deriving (Show, Typeable, Data)

-- New structure to keep the calculated values
-- and supporting functions

data MemoTree = MemoRoot    MemoTree          MemoTable
              | MemoPRINT   MemoTree MemoTree MemoTable
              | MemoAdd     MemoTree MemoTree MemoTable
              | MemoFact    MemoTree          MemoTable
              | MemoName    MemoTree          MemoTable
              | MemoNumber  String            MemoTable
              | MemoId      String            MemoTable
              | MemoEmptyConstPart            MemoTable
              | MemoWHERE   MemoTree          MemoTable
              | MemoComma   MemoTree MemoTree MemoTable
              | MemoDef     MemoTree          MemoTable
              | MemoEqual   MemoTree String   MemoTable
  deriving (Show, Typeable, Data)

data Att a where
 Envi_attr  :: Att SymbolTable
 Envs_attr  :: Att SymbolTable
 Name_attr  :: Att String
 Value_attr :: Att String
 Ok_attr    :: Att Bool
 Code_attr  :: Att String

type MemoTable = ( Maybe SymbolTable   -- envi
                 , Maybe SymbolTable   -- envs
                 , Maybe String        -- name
                 , Maybe String        -- value
                 , Maybe Bool          -- ok
                 , Maybe String     )  -- code

emptyMemoTable :: MemoTable
emptyMemoTable = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

data Dir = Parent | Child Int | Local
type MemoAG = State (Zipper MemoTree)

lexemeMemoTable :: Zipper MemoTree -> MemoTable
lexemeMemoTable memo_ag = case (getHole memo_ag :: Maybe MemoTree) of
                            Just (MemoRoot    _       table) -> table
                            Just (MemoPRINT   _   _   table) -> table
                            Just (MemoAdd     _   _   table) -> table
                            Just (MemoFact    _       table) -> table
                            Just (MemoName    _       table) -> table
                            Just (MemoNumber  _       table) -> table
                            Just (MemoId      _       table) -> table
                            Just (MemoEmptyConstPart  table) -> table
                            Just (MemoWHERE   _       table) -> table
                            Just (MemoComma   _  _    table) -> table
                            Just (MemoDef     _       table) -> table
                            Just (MemoEqual   _ _     table) -> table

count m n = case left m of
                     Nothing  -> n
                     Just m'  -> count m' (n+1)
                          
updateMemoTree :: MemoTree -> Att res -> Maybe res -> MemoTree
updateMemoTree (MemoRoot    l      table) attr res = MemoRoot    l      (updateAttr attr res table)
updateMemoTree (MemoPRINT   l  r   table) attr res = MemoPRINT   l  r   (updateAttr attr res table)
updateMemoTree (MemoAdd     l  r   table) attr res = MemoAdd     l  r   (updateAttr attr res table)
updateMemoTree (MemoFact    l      table) attr res = MemoFact    l      (updateAttr attr res table)
updateMemoTree (MemoName    l      table) attr res = MemoName    l      (updateAttr attr res table)
updateMemoTree (MemoNumber  l      table) attr res = MemoNumber  l      (updateAttr attr res table)
updateMemoTree (MemoId      l      table) attr res = MemoId      l      (updateAttr attr res table)
updateMemoTree (MemoEmptyConstPart table) attr res = MemoEmptyConstPart (updateAttr attr res table)
updateMemoTree (MemoWHERE   l      table) attr res = MemoWHERE   l      (updateAttr attr res table)
updateMemoTree (MemoComma   l  r   table) attr res = MemoComma   l  r   (updateAttr attr res table)
updateMemoTree (MemoDef     l      table) attr res = MemoDef     l      (updateAttr attr res table)
updateMemoTree (MemoEqual   l  r   table) attr res = MemoEqual   l  r   (updateAttr attr res table)

buildMemoTree :: Root -> MemoTree
buildMemoTree  (Root program)                = MemoRoot (buildMemoTree2 program) emptyMemoTable
buildMemoTree2 (PRINT expression constPart)  = MemoPRINT (buildMemoTree3 expression) (buildMemoTree6 constPart) emptyMemoTable
buildMemoTree3 (Add  expression factor)      = MemoAdd (buildMemoTree3 expression) (buildMemoTree4 factor) emptyMemoTable
buildMemoTree3 (Fact factor)                 = MemoFact (buildMemoTree4 factor) emptyMemoTable
buildMemoTree4 (Name constName)              = MemoName (buildMemoTree5 constName) emptyMemoTable
buildMemoTree4 (Number string)               = MemoNumber string emptyMemoTable
buildMemoTree5 (Id string)                   = MemoId string emptyMemoTable
buildMemoTree6 (EmptyConstPart)              = MemoEmptyConstPart emptyMemoTable
buildMemoTree6 (WHERE constDefList)          = MemoWHERE (buildMemoTree7 constDefList) emptyMemoTable
buildMemoTree7 (Comma constDefList constDef) = MemoComma (buildMemoTree7 constDefList) (buildMemoTree8 constDef) emptyMemoTable
buildMemoTree7 (Def constDef)                = MemoDef (buildMemoTree8 constDef) emptyMemoTable
buildMemoTree8 (Equal constName string)      = MemoEqual (buildMemoTree5 constName) string emptyMemoTable

data Constructor = C_Root
                 | C_PRINT
                 | C_Add
                 | C_Fact
                 | C_Name
                 | C_Number
                 | C_Id
                 | C_EmptyConstPart
                 | C_WHERE
                 | C_Comma
                 | C_Def
                 | C_Equal

constructor :: MemoAG Constructor
constructor = do 
               ag <- get
               case (getHole ag :: Maybe MemoTree) of
                  Just (MemoRoot    _      _) -> return C_Root
                  Just (MemoPRINT   _   _  _) -> return C_PRINT
                  Just (MemoAdd     _   _  _) -> return C_Add
                  Just (MemoFact    _      _) -> return C_Fact
                  Just (MemoName    _      _) -> return C_Name
                  Just (MemoNumber  _      _) -> return C_Number
                  Just (MemoId      _      _) -> return C_Id
                  Just (MemoEmptyConstPart _) -> return C_EmptyConstPart
                  Just (MemoWHERE   _      _) -> return C_WHERE
                  Just (MemoComma   _  _   _) -> return C_Comma
                  Just (MemoDef     _      _) -> return C_Def
                  Just (MemoEqual   _ _    _) -> return C_Equal
                  _                           -> error  "Error on constructor!"

lexeme :: Zipper MemoTree -> String
lexeme ag = case (getHole ag :: Maybe MemoTree) of
                    Just (MemoId     x   _) -> x
                    Just (MemoEqual  _ x _) -> x
                    Just (MemoNumber x   _) -> x

lookupAttr :: Att a -> MemoTable -> Maybe a
lookupAttr Envi_attr  (v,_,_,_,_,_) = v
lookupAttr Envs_attr  (_,v,_,_,_,_) = v
lookupAttr Name_attr  (_,_,v,_,_,_) = v
lookupAttr Value_attr (_,_,_,v,_,_) = v
lookupAttr Ok_attr    (_,_,_,_,v,_) = v
lookupAttr Code_attr  (_,_,_,_,_,v) = v

updateAttr :: Att a -> Maybe a -> MemoTable -> MemoTable
updateAttr Envi_attr  v (_,b,c,d,e,f) = (v,b,c,d,e,f)
updateAttr Envs_attr  v (a,_,c,d,e,f) = (a,v,c,d,e,f)
updateAttr Name_attr  v (a,b,_,d,e,f) = (a,b,v,d,e,f)
updateAttr Value_attr v (a,b,c,_,e,f) = (a,b,c,v,e,f)
updateAttr Ok_attr    v (a,b,c,d,_,f) = (a,b,c,d,v,f)
updateAttr Code_attr  v (a,b,c,d,e,_) = (a,b,c,d,e,v)

memo :: Typeable res => Att res -> MemoAG res -> Dir -> MemoAG res
memo attr eval dir = do
  memo_ag <- get

  let memo_ag' = case dir of 
                       Parent  -> parent memo_ag
                       Child n -> memo_ag .$ n 
                       Local   -> memo_ag
  put memo_ag'
  
  res <- case lookupAttr attr (lexemeMemoTable memo_ag') of
           Just v -> return v
           Nothing  -> do val   <- eval
                          memo_ag'' <-get 
                          let  subtree       = fromJust (getHole memo_ag'' :: Maybe MemoTree)
                          let  new_memo_tree = updateMemoTree subtree attr (Just val)
                          modify $ setHole new_memo_tree
                          return val

  memo_out <- get
  let memo_out' = case dir of
                       Parent  -> let n = count memo_ag 1
                                  in  memo_out .$ n
                       Child n -> parent memo_out 
                       Local   -> memo_out
  put memo_out'
  
  return res

---- Inherited -----
envi :: Dir -> MemoAG SymbolTable
envi = memo Envi_attr $ do 
        constr <- constructor
        case constr of
               C_PRINT -> envs (Child 2)
               _       -> envi Parent

---- Synthesized ----
envs :: Dir -> MemoAG SymbolTable
envs = memo Envs_attr $ do
        constr <- constructor
        case constr of
            C_EmptyConstPart -> return $ []
            C_WHERE -> envs (Child 1)
            C_Comma -> do st <- envs (Child 1)
                          n  <- name (Child 2)
                          v  <- value (Child 2)
                          return $ (st ++ [(n,v)])
            C_Def -> do n <- name (Child 1)
                        v <- value (Child 1)
                        return $ [( n, v )]

name :: Dir -> MemoAG String
name = memo Name_attr $ do
        constr <- constructor
        case constr of
          C_Id    -> do ag <- get
                        return $ lexeme ag
          C_Equal -> name (Child 1)

value :: Dir -> MemoAG String
value = memo Value_attr $ do
        constr <- constructor
        case constr of
               C_Name   -> do n  <- name ( Child 1 )
                              st <- envi Local
                              return $ getValue n st
               C_Number -> do ag <- get
                              return $ lexeme ag
               C_Equal  -> do ag <- get
                              return $ lexeme ag

ok :: Dir -> MemoAG Bool
ok = memo Ok_attr $ do
        constr <- constructor
        case constr of
              C_Name -> do n <- name ( Child 1 )
                           st <- envi Local
                           return $ isInST n st
              C_Number -> return $ True
              C_EmptyConstPart -> return $ True
              C_WHERE -> ok ( Child 1 )
              C_Comma -> do b <- ok ( Child 1 )
                            n <- name ( Child 2 )
                            st <- envs ( Child 1 )
                            return $ b && (not (isInST n st))
              C_Def -> return $ True

code :: Dir -> MemoAG String
code = memo Code_attr $ do
        constr <- constructor
        case constr of
           C_Root -> code ( Child 1 )
           C_PRINT -> do b <- ok ( Child 2 )
                         if b then do s <- code ( Child 1 )
                                      return $ s ++ "PRINT, 0\n" ++ "HALT,  0\n"
                              else return $ "HALT,  0\n"
           C_Add -> do b <- ok ( Child 2 )
                       if b then do s1 <- code ( Child 1 )
                                    s2 <- value ( Child 2 )
                                    return $ s1 ++ "ADD,   " ++ s2 ++ "\n"
                            else return $ "HALT,  0\n"
           C_Fact -> do b <- ok ( Child 1 )
                        if b then do s <- value ( Child 1 )
                                     return $ "LOAD,  " ++ s ++ "\n"
                             else return $ "HALT,  0\n"

{-Semantic Functions-}
isInST :: String -> SymbolTable -> Bool
isInST _ [] = False
isInST c ((a,b):xs) = if (c==a) then True else isInST c xs

getValue :: String -> SymbolTable -> String
getValue c ((a,b):xs) = if (c==a) then b else (getValue c xs)

semantics :: Root -> String
semantics t = let ag = toZipper $ buildMemoTree t
              in  fst $ runState (code Local) ag

