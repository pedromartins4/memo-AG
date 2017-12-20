{-# LANGUAGE DeriveDataTypeable, GADTs #-}
module Memo where

import Language.Grammars.ZipperAG
import Control.Monad.State.Lazy
import Data.Generics.Zipper
import Data.Maybe
import Data.Data
import Shared

-- New structure to keep the calculated values
-- and supporting functions
data MemoTree = MemoRoot    MemoTree          MemoTable
              | MemoConsIts MemoTree MemoTree MemoTable
              | MemoNilIts                    MemoTable
              | MemoDecl    String            MemoTable
              | MemoUse     String            MemoTable
              | MemoBlock   MemoTree          MemoTable
  deriving (Typeable, Data)

data Att a where
   Attr_dclo :: Att SymbolTable
   Attr_errs :: Att Error
   Attr_dcli :: Att SymbolTable
   Attr_lev  :: Att Int
   Attr_env  :: Att SymbolTable

type MemoTable = ( Maybe SymbolTable
                 , Maybe Error
                 , Maybe SymbolTable
                 , Maybe Int
                 , Maybe SymbolTable
                 )

emptyMemoTable :: MemoTable
emptyMemoTable = (Nothing,Nothing,Nothing,Nothing,Nothing)

data Dir = Parent | Child Int | Local | Child_Right Int | Child_Left Int
type MemoAG = State (Zipper MemoTree)

lexemeMemoTable :: Zipper MemoTree -> MemoTable
lexemeMemoTable memo_ag = case (getHole memo_ag :: Maybe MemoTree) of
                            Just (MemoRoot    _   table) -> table
                            Just (MemoConsIts _ _ table) -> table
                            Just (MemoNilIts      table) -> table
                            Just (MemoDecl    _   table) -> table
                            Just (MemoUse     _   table) -> table
                            Just (MemoBlock   _   table) -> table

count m n = case left m of
                     Nothing  -> n
                     Just m'  -> count m' (n+1)

updateMemoTree :: MemoTree -> Att res -> Maybe res -> MemoTree
updateMemoTree (MemoRoot    l   table) attr res = MemoRoot    l   (updateAttr attr res table)
updateMemoTree (MemoConsIts l r table) attr res = MemoConsIts l r (updateAttr attr res table)
updateMemoTree (MemoNilIts      table) attr res = MemoNilIts      (updateAttr attr res table)
updateMemoTree (MemoDecl    l   table) attr res = MemoDecl    l   (updateAttr attr res table)
updateMemoTree (MemoUse     l   table) attr res = MemoUse     l   (updateAttr attr res table)
updateMemoTree (MemoBlock   l   table) attr res = MemoBlock   l   (updateAttr attr res table)

buildMemoTree :: Root -> MemoTree
buildMemoTree (Root its)        = MemoRoot (buildMemoTree2 its) emptyMemoTable
buildMemoTree2 (ConsIts it its) = MemoConsIts (buildMemoTree3 it) (buildMemoTree2 its) emptyMemoTable
buildMemoTree2 (NilIts        ) = MemoNilIts  emptyMemoTable
buildMemoTree3 (Decl string )   = MemoDecl  string emptyMemoTable
buildMemoTree3 (Use  string )   = MemoUse   string emptyMemoTable
buildMemoTree3 (Block its   )   = MemoBlock (buildMemoTree2 its) emptyMemoTable

lookupAttr :: Att a -> MemoTable -> Maybe a
lookupAttr Attr_dclo (v,_,_,_,_) = v
lookupAttr Attr_errs (_,v,_,_,_) = v
lookupAttr Attr_dcli (_,_,v,_,_) = v
lookupAttr Attr_lev  (_,_,_,v,_) = v
lookupAttr Attr_env  (_,_,_,_,v) = v

updateAttr :: Att a -> Maybe a -> MemoTable -> MemoTable
updateAttr Attr_dclo v (_,b,c,d,e) = (v,b,c,d,e)
updateAttr Attr_errs v (a,_,c,d,e) = (a,v,c,d,e)
updateAttr Attr_dcli v (a,b,_,d,e) = (a,b,v,d,e)
updateAttr Attr_lev  v (a,b,c,_,e) = (a,b,c,v,e)
updateAttr Attr_env  v (a,b,c,d,_) = (a,b,c,d,v)

data Constructor = C_Root
                 | C_ConsIts
                 | C_NilIts
                 | C_Decl
                 | C_Use
                 | C_Block

constructor :: MemoAG Constructor
constructor = do 
               ag <- get
               case (getHole ag :: Maybe MemoTree) of
                 Just (MemoRoot    _   _) -> return C_Root
                 Just (MemoConsIts _ _ _) -> return C_ConsIts
                 Just (MemoNilIts      _) -> return C_NilIts
                 Just (MemoDecl    _   _) -> return C_Decl
                 Just (MemoUse     _   _) -> return C_Use
                 Just (MemoBlock   _   _) -> return C_Block
                 _                      -> error  "Error on constructor!"

constructor_parent :: MemoAG Constructor
constructor_parent = do ag <- get
                        case (getHole (parent ag) :: Maybe MemoTree) of
                          Just (MemoRoot    _   _) -> return C_Root
                          Just (MemoConsIts _ _ _) -> return C_ConsIts
                          Just (MemoNilIts      _) -> return C_NilIts
                          Just (MemoDecl    _   _) -> return C_Decl
                          Just (MemoUse     _   _) -> return C_Use
                          Just (MemoBlock   _   _) -> return C_Block
                          _                      -> error  "Error on constructor!"

lexeme :: Zipper MemoTree -> String
lexeme ag = case (getHole ag :: Maybe MemoTree) of
               Just (MemoUse x  _) -> x
               Just (MemoDecl x _) -> x
               _                 -> error "Error on lexeme"

memo :: Typeable res => Att res -> MemoAG res -> Dir -> MemoAG res
memo attr eval dir = do
  memo_ag <- get

  let memo_ag' = case dir of 
                       Parent        -> parent memo_ag
                       Child n       -> memo_ag .$ n 
                       Local         -> memo_ag
                       Child_Left  n -> memo_ag .$< n
                       Child_Right n -> memo_ag .$> n
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
                       Parent        -> let n = count memo_ag 1
                                        in  memo_out .$ n
                       Child      n  -> parent memo_out 
                       Local         -> memo_out
                       Child_Left n  -> memo_out.$>n
                       Child_Right n -> memo_out.$<n
  put memo_out'
  
  return res


---- Synthesized Attributes ----
dclo :: Dir -> MemoAG SymbolTable
dclo = memo Attr_dclo $ do
    constr <- constructor
    case constr of
      C_ConsIts -> dclo (Child 2)
      C_NilIts  -> dcli Local
      C_Use     -> dcli Local
      C_Decl    -> do st   <- dcli Local
                      name <- lev Local
                      ag   <- get
                      return $ (lexeme ag, name) : st
      C_Block   -> dcli Local

errs :: Dir -> MemoAG Error
errs = memo Attr_errs $ do
    constr <- constructor
    case constr of
      C_Root   -> errs (Child 1)
      C_NilIts -> return []
      C_ConsIts -> do st1 <- errs (Child 1)
                      st2 <- errs (Child 2)
                      return $ st1 ++ st2
      C_Use -> do st <- env Local
                  ag <- get
                  return $ mBIn (lexeme ag) st
      C_Decl -> do l  <- lev  Local
                   st <- dcli Local
                   ag <- get
                   return $ mNBIn (lexeme ag, l) st
      C_Block -> errs (Child 1)

---- Inheritted Attributes ----

dcli :: Dir -> MemoAG SymbolTable
dcli = memo Attr_dcli $ do
    constr <- constructor
    case constr of
      C_Root -> return []
      C_NilIts -> do const_parent <- constructor_parent
                     case const_parent of
                         C_ConsIts -> dclo (Child_Left 1)
                         C_Block   -> env Parent
                         C_Root    -> return []
      C_ConsIts -> do const_parent <- constructor_parent
                      case const_parent of
                         C_ConsIts -> dclo (Child_Left 1)
                         C_Block   -> env Parent
                         C_Root    -> return []
      C_Block -> dcli Parent
      C_Use   -> dcli Parent
      C_Decl  -> dcli Parent

lev :: Dir -> MemoAG Int
lev = memo Attr_lev $ do
    constr <- constructor
    case constr of
      C_Root -> return 0
      C_NilIts -> do const_parent <- constructor_parent
                     case const_parent of
                        C_Block   -> do l <- lev Parent
                                        return $ 1+l
                        C_ConsIts -> lev Parent
                        C_Root    -> return 0
      C_ConsIts -> do const_parent <- constructor_parent
                      case const_parent of
                        C_Block -> do l <- lev Parent
                                      return $ 1+l
                        C_ConsIts -> lev Parent
                        C_Root    -> return 0
      C_Block -> lev Parent
      C_Use   -> lev Parent
      C_Decl  -> lev Parent

env :: Dir -> MemoAG SymbolTable
env = memo Attr_env $ do
    constr <- constructor
    case constr of
      C_NilIts -> do const_parent <- constructor_parent
                     case const_parent of
                        C_Block   -> dclo Local
                        C_ConsIts -> env Parent
                        C_Root    -> dclo Local
      C_ConsIts -> do const_parent <- constructor_parent
                      case const_parent of
                        C_Block   -> dclo Local
                        C_ConsIts -> env Parent
                        C_Root    -> dclo Local
      C_Block -> env  Parent
      C_Use   -> env  Parent
      C_Decl  -> env  Parent
      C_Root  -> dclo Local

{- Environment lookup functions -}
mBIn :: String -> SymbolTable -> Error
mBIn name [] = [name]
mBIn name ((n,l):es) = if (n==name) then [] else mBIn name es

mNBIn :: (String, Int) -> SymbolTable -> Error
mNBIn tuple [] = [] 
mNBIn pair (pl:es) = if pair==pl then [fst pair] else mNBIn pair es

semantics :: Root -> Error
semantics t = let ag           = toZipper $ buildMemoTree t
              in  fst $ runState (errs Local) ag


















































