--  Copyright (C) 2017 Joao Saraiva, Joao P. Fernandes, Pedro Martins,
--             Alberto Pardo, Marcos Viera
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
--  along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveDataTypeable, GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Memo where

import Language.Grammars.ZipperAG
import Control.Monad.State.Lazy
import Data.Generics.Zipper
import Data.Dynamic
import Data.Maybe
import Data.Data
import Shared

-- New structure to keep the calculated values
-- and supporting functions

data MemoTree = MemoRoot        MemoTree          MemoTable
              | MemoRootTable   MemoTree          MemoTable
              | MemoNoRow                         MemoTable
              | MemoConsRow     MemoTree MemoTree MemoTable
              | MemoOneRow      MemoTree          MemoTable
              | MemoNoElem                        MemoTable
              | MemoConsElem    MemoTree MemoTree MemoTable
              | MemoTableText   String            MemoTable
              | MemoNestedTable MemoTree          MemoTable
  deriving (Typeable, Data)

data Att a where
    Attr_n_Syn     :: Att Int
    Attr_ns_Syn    :: Att [Int]
    Attr_ane_Inh   :: Att Int
    Attr_mh_Syn    :: Att Int
    Attr_mw_Syn    :: Att Int
    Attr_mws_Syn   :: Att [Int]
    Attr_lmw_Local :: Att Int
    Attr_ah_Inh    :: Att Int
    Attr_aws_Inh   :: Att [Int]
    Attr_aw_Inh    :: Att Int
    Attr_lines_Syn :: Att [String]

-- A tuple can only be an instance of Typeable and Data if it has at MOST 7 elements.
-- so I am splitting it here
type MemoTable = (( Maybe Int
                  , Maybe [Int]
                  , Maybe Int
                  , Maybe Int
                  , Maybe Int
                  , Maybe [Int]
                  , Maybe Int )
                  ,
                  ( Maybe Int
                  , Maybe [Int]
                  , Maybe Int
                  , Maybe [String]
                  )
                 )

emptyMemoTable :: MemoTable
emptyMemoTable = ((Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing),(Nothing, Nothing, Nothing, Nothing))

data Dir = Parent | Child Int | Local | Child_Right Int | Child_Left Int
type MemoAG = State (Zipper MemoTree)

lexemeMemoTable :: Zipper MemoTree -> MemoTable
lexemeMemoTable memo_ag = case (getHole memo_ag :: Maybe MemoTree) of
                           Just( MemoRoot        _   table) -> table
                           Just( MemoRootTable   _   table) -> table
                           Just( MemoNoRow           table) -> table
                           Just( MemoConsRow     _ _ table) -> table
                           Just( MemoOneRow      _   table) -> table
                           Just( MemoNoElem          table) -> table
                           Just( MemoConsElem    _ _ table) -> table
                           Just( MemoTableText   _   table) -> table
                           Just( MemoNestedTable _   table) -> table

count m n = case left m of
                     Nothing  -> n
                     Just m'  -> count m' (n+1)

updateMemoTree :: MemoTree -> Att res -> Maybe res -> MemoTree
updateMemoTree (MemoRoot        l   table) attr res = MemoRoot        l   (updateAttr attr res table)
updateMemoTree (MemoRootTable   l   table) attr res = MemoRootTable   l   (updateAttr attr res table)
updateMemoTree (MemoNoRow           table) attr res = MemoNoRow           (updateAttr attr res table)
updateMemoTree (MemoConsRow     l r table) attr res = MemoConsRow     l r (updateAttr attr res table)
updateMemoTree (MemoOneRow      l   table) attr res = MemoOneRow      l   (updateAttr attr res table)
updateMemoTree (MemoNoElem          table) attr res = MemoNoElem          (updateAttr attr res table)
updateMemoTree (MemoConsElem    l r table) attr res = MemoConsElem    l r (updateAttr attr res table)
updateMemoTree (MemoTableText   l   table) attr res = MemoTableText   l   (updateAttr attr res table)
updateMemoTree (MemoNestedTable l   table) attr res = MemoNestedTable l   (updateAttr attr res table)

buildMemoTree :: Root -> MemoTree
buildMemoTree (Root table)            = MemoRoot (buildMemoTree2 table) emptyMemoTable
buildMemoTree2 (RootTable rows)       = MemoRootTable (buildMemoTree3 rows) emptyMemoTable
buildMemoTree3 NoRow                  = MemoNoRow emptyMemoTable
buildMemoTree3 (ConsRow row rows)     = MemoConsRow (buildMemoTree4 row) (buildMemoTree3 rows) emptyMemoTable
buildMemoTree4 (OneRow elems)         = MemoOneRow (buildMemoTree5 elems) emptyMemoTable
buildMemoTree5 NoElem                 = MemoNoElem emptyMemoTable
buildMemoTree5 (ConsElem eleem elems) = MemoConsElem (buildMemoTree6 eleem) (buildMemoTree5 elems) emptyMemoTable
buildMemoTree6 (TableText string)     = MemoTableText string emptyMemoTable
buildMemoTree6 (NestedTable table)    = MemoNestedTable (buildMemoTree2 table) emptyMemoTable

lookupAttr :: Att a -> MemoTable -> Maybe a
lookupAttr Attr_n_Syn     ((v,_,_,_,_,_,_),(_,_,_,_)) = v
lookupAttr Attr_ns_Syn    ((_,v,_,_,_,_,_),(_,_,_,_)) = v
lookupAttr Attr_ane_Inh   ((_,_,v,_,_,_,_),(_,_,_,_)) = v
lookupAttr Attr_mh_Syn    ((_,_,_,v,_,_,_),(_,_,_,_)) = v
lookupAttr Attr_mw_Syn    ((_,_,_,_,v,_,_),(_,_,_,_)) = v
lookupAttr Attr_mws_Syn   ((_,_,_,_,_,v,_),(_,_,_,_)) = v
lookupAttr Attr_lmw_Local ((_,_,_,_,_,_,v),(_,_,_,_)) = v
lookupAttr Attr_ah_Inh    ((_,_,_,_,_,_,_),(v,_,_,_)) = v
lookupAttr Attr_aws_Inh   ((_,_,_,_,_,_,_),(_,v,_,_)) = v
lookupAttr Attr_aw_Inh    ((_,_,_,_,_,_,_),(_,_,v,_)) = v
lookupAttr Attr_lines_Syn ((_,_,_,_,_,_,_),(_,_,_,v)) = v

updateAttr :: Att a -> Maybe a -> MemoTable -> MemoTable
updateAttr Attr_n_Syn     v ((_,b,c,d,e,f,g),(h,i,j,k)) = ((v,b,c,d,e,f,g),(h,i,j,k))
updateAttr Attr_ns_Syn    v ((a,_,c,d,e,f,g),(h,i,j,k)) = ((a,v,c,d,e,f,g),(h,i,j,k))
updateAttr Attr_ane_Inh   v ((a,b,_,d,e,f,g),(h,i,j,k)) = ((a,b,v,d,e,f,g),(h,i,j,k))
updateAttr Attr_mh_Syn    v ((a,b,c,_,e,f,g),(h,i,j,k)) = ((a,b,c,v,e,f,g),(h,i,j,k))
updateAttr Attr_mw_Syn    v ((a,b,c,d,_,f,g),(h,i,j,k)) = ((a,b,c,d,v,f,g),(h,i,j,k))
updateAttr Attr_mws_Syn   v ((a,b,c,d,e,_,g),(h,i,j,k)) = ((a,b,c,d,e,v,g),(h,i,j,k))
updateAttr Attr_lmw_Local v ((a,b,c,d,e,f,_),(h,i,j,k)) = ((a,b,c,d,e,f,v),(h,i,j,k))
updateAttr Attr_ah_Inh    v ((a,b,c,d,e,f,g),(_,i,j,k)) = ((a,b,c,d,e,f,g),(v,i,j,k))
updateAttr Attr_aws_Inh   v ((a,b,c,d,e,f,g),(h,_,j,k)) = ((a,b,c,d,e,f,g),(h,v,j,k))
updateAttr Attr_aw_Inh    v ((a,b,c,d,e,f,g),(h,i,_,k)) = ((a,b,c,d,e,f,g),(h,i,v,k))
updateAttr Attr_lines_Syn v ((a,b,c,d,e,f,g),(h,i,j,_)) = ((a,b,c,d,e,f,g),(h,i,j,v))

data Constructors = C_Root
                  | C_RootTable
                  | C_NoRow
                  | C_ConsRow
                  | C_OneRow
                  | C_NoElem
                  | C_ConsElem
                  | C_TableText
                  | C_NestedTable

constructor :: MemoAG Constructors
constructor = do ag <- get
                 case (getHole ag :: Maybe MemoTree ) of
                    Just (MemoRoot        _   _) -> return C_Root
                    Just (MemoRootTable   _   _) -> return C_RootTable
                    Just (MemoNoRow           _) -> return C_NoRow
                    Just (MemoConsRow     _ _ _) -> return C_ConsRow
                    Just (MemoOneRow      _   _) -> return C_OneRow
                    Just (MemoNoElem          _) -> return C_NoElem
                    Just (MemoConsElem    _ _ _) -> return C_ConsElem
                    Just (MemoTableText   _   _) -> return C_TableText
                    Just (MemoNestedTable _   _) -> return C_NestedTable

constructor_parent :: MemoAG Constructors
constructor_parent = do ag <- get
                        case (getHole (parent ag) :: Maybe MemoTree ) of
                           Just (MemoRoot        _   _) -> return C_Root
                           Just (MemoRootTable   _   _) -> return C_RootTable
                           Just (MemoNoRow           _) -> return C_NoRow
                           Just (MemoConsRow     _ _ _) -> return C_ConsRow
                           Just (MemoOneRow      _   _) -> return C_OneRow
                           Just (MemoNoElem          _) -> return C_NoElem
                           Just (MemoConsElem    _ _ _) -> return C_ConsElem
                           Just (MemoTableText   _   _) -> return C_TableText
                           Just (MemoNestedTable _   _) -> return C_NestedTable

lexeme_Elem :: Zipper MemoTree -> String
lexeme_Elem t = case ( getHole t :: Maybe MemoTree) of
               Just (MemoTableText x _) -> x
               _ -> error "You should not be asking for that lexeme_Elem!"

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

---- AG ----
---- Computing the number of elems per row ----
n_Syn :: Dir -> MemoAG Int
n_Syn = memo Attr_n_Syn $ do
    constr <- constructor
    case constr of
       C_Root      -> n_Syn (Child 1)
       C_RootTable -> do res <- ns_Syn (Child 1)
                         return $ maxList res
       C_OneRow    -> n_Syn (Child 1)
       C_NoElem    -> return $ 0
       C_ConsElem  -> do res <- n_Syn (Child 2)
                         return (1 + res)

ns_Syn :: Dir -> MemoAG [Int]
ns_Syn = memo Attr_ns_Syn $ do
    constr <- constructor
    case constr of
      C_NoRow   -> return []
      C_ConsRow -> do res1 <- n_Syn (Child 1)
                      res2 <- ns_Syn (Child 2)
                      return $ res1 : res2

---- Passing down the number of elements per row ----
ane_Inh :: Dir -> MemoAG Int
ane_Inh = memo Attr_ane_Inh $ do
    constr <- constructor
    case constr of
        C_RootTable -> n_Syn Local
        C_NoRow -> do constr1 <- constructor_parent
                      case constr1 of
                         C_RootTable -> n_Syn   Parent
                         C_NoRow     -> ane_Inh Parent
                         C_ConsRow   -> ane_Inh Parent
        C_ConsRow -> do constr1 <- constructor_parent
                        case constr1 of
                          C_RootTable -> n_Syn Parent
                          C_OneRow    -> ane_Inh Parent
                          C_ConsRow   -> ane_Inh Parent
        C_OneRow -> ane_Inh Parent
        C_NoElem -> do constr1 <- constructor_parent
                       case constr1 of
                          C_OneRow   -> ane_Inh Parent
                          C_ConsElem -> do res <- ane_Inh Parent
                                           return $ res - 1
                          C_NoElem   -> do res <- ane_Inh Parent
                                           return $ res - 1
        C_ConsElem -> do constr1 <- constructor_parent
                         case constr1 of
                            C_OneRow   -> ane_Inh Parent
                            C_ConsElem -> do res <- ane_Inh Parent
                                             return $ res - 1
                            C_NoElem   -> do res <- ane_Inh Parent
                                             return $ res - 1

---- Computing the minimal height of each construct ----
mh_Syn :: Dir -> MemoAG Int
mh_Syn = memo Attr_mh_Syn $ do
    constr <- constructor
    case constr of
      C_Root        -> mh_Syn (Child 1)
      C_RootTable   -> mh_Syn (Child 1)
      C_NoRow       -> return 0
      C_ConsRow     -> do res1 <- mh_Syn (Child 1)
                          res2 <- mh_Syn (Child 2)
                          return $ res1 + 1 + res2
      C_OneRow      -> mh_Syn (Child 1)
      C_ConsElem    -> do res1 <- mh_Syn (Child 1)
                          res2 <- mh_Syn (Child 2)
                          return $ max res1 res2
      C_NoElem      -> return 0
      C_TableText   -> return 1
      C_NestedTable -> do res <- mh_Syn (Child 1)
                          return $ res + 1

---- Computing the minimal width of each construct ----
mw_Syn :: Dir -> MemoAG Int
mw_Syn = memo Attr_mw_Syn $ do
    constr <- constructor
    case constr of
      C_Root        -> mw_Syn (Child 1)
      C_RootTable   -> lmw_Local Local -- Local attr, as defined in LRC
      C_TableText   -> do ag <- get
                          return $ length (lexeme_Elem ag)
      C_NestedTable -> do res <- mw_Syn (Child 1)
                          return $ res + 2

mws_Syn :: Dir -> MemoAG [Int]
mws_Syn = memo Attr_mws_Syn $ do
    constr <- constructor
    case constr of
        C_NoRow    -> return []
        C_ConsRow  -> do res1 <- mws_Syn (Child 1)
                         res2 <- mws_Syn (Child 2)
                         return $ eq_zipwith_max res1 res2
        C_OneRow   -> mws_Syn (Child 1)
        C_ConsElem -> do res1 <- mw_Syn (Child 1)
                         res2 <- mws_Syn (Child 2)
                         return $ res1 : res2
        C_NoElem   -> return []

---- LOCAL ATTRIBUTE ----
lmw_Local :: Dir -> MemoAG Int
lmw_Local = memo Attr_lmw_Local $ do
    constr <- constructor
    case constr of
          C_RootTable -> do res1 <- mws_Syn (Child 1)
                            res2 <- mws_Syn (Child 1)
                            return $ (sumList res1) + (lengthList res2) - 1
          C_ConsRow -> do res1 <- aws_Inh Local
                          res2 <- aws_Inh Local
                          return $ (sumList res1) + (lengthList res2) - 1

---- Passing down the available heights and widths ----
ah_Inh :: Dir -> MemoAG Int
ah_Inh = memo Attr_ah_Inh $ do
    constr <- constructor
    case constr of
      C_Root -> mh_Syn Local
      C_RootTable -> do constr1 <- constructor_parent
                        case constr1 of
                          C_Root     -> ah_Inh Parent
                          C_ConsElem -> ah_Inh Parent
      C_ConsElem -> do constr1 <- constructor_parent
                       case constr1 of
                         C_OneRow   -> mh_Syn Local
                         C_ConsElem -> ah_Inh Parent
      C_NoElem -> do constr1 <- constructor_parent
                     case constr1 of
                       C_OneRow   -> mh_Syn Local
                       C_ConsElem -> ah_Inh Parent
      C_TableText   -> ah_Inh Parent
      C_NestedTable -> ah_Inh Parent

aws_Inh :: Dir -> MemoAG [Int]
aws_Inh = memo Attr_aws_Inh $ do
    constr <- constructor
    case constr of
        C_ConsRow -> do constr1 <- constructor_parent
                        case constr1 of
                           C_RootTable -> mws_Syn Local
                           C_ConsRow   -> aws_Inh Parent
        C_NoRow -> do constr1 <- constructor_parent
                      case constr1 of
                         C_RootTable -> mws_Syn Local
                         C_ConsRow   -> aws_Inh Parent
        C_OneRow -> aws_Inh Parent
        C_ConsElem -> do constr1 <- constructor_parent
                         case constr1 of
                            C_OneRow -> aws_Inh Parent
                            C_ConsElem -> do res <- aws_Inh Parent
                                             return $ tailList res
        C_NoElem -> do constr1 <- constructor_parent
                       case constr1 of
                         C_OneRow   -> aws_Inh Parent
                         C_ConsElem -> do res <- aws_Inh Parent
                                          return $ tailList res

aw_Inh :: Dir -> MemoAG Int
aw_Inh = memo Attr_aw_Inh $ do
    constr <- constructor
    case constr of
      C_Root -> mw_Syn Local
      C_RootTable -> do constr1 <- constructor_parent
                        case constr1 of
                          C_Root        -> ah_Inh Parent
                          C_NestedTable -> aw_Inh Parent
      C_TableText   -> do res <- aws_Inh Parent
                          return $ headList res
      C_NestedTable -> do res <- aws_Inh Parent
                          return $ headList res

---- Computing Formatted Table ----
lines_Syn :: Dir -> MemoAG [String]
lines_Syn = memo Attr_lines_Syn $ do
    constr <- constructor
    case constr of
          C_Root        -> lines_Syn (Child 1)
          C_RootTable   -> do res1 <- lines_Syn (Child 1)
                              res2 <- lmw_Local Local
                              res3 <- lmw_Local Local
                              return $ (add_sepline (res2)) ++ (res1) ++ (add_sepline (res3))
          C_NoRow       -> return []
          C_ConsRow     -> do res1 <- lines_Syn (Child 1)
                              res2 <- lines_Syn (Child 2)
                              res3 <- lmw_Local Local
                              return $ add_sep_line (res3) (res1) (res2)
          C_OneRow      -> do res <- lines_Syn (Child 1)
                              return $ add_border_line res
          C_NoElem      -> return []
          C_ConsElem    -> do res1 <- aw_Inh (Child 1)
                              res2 <- mw_Syn (Child 1)
                              res3 <- ah_Inh (Child 1)
                              res4 <- mh_Syn (Child 1)
                              res5 <- lines_Syn (Child 1)
                              res6 <- lines_Syn (Child 2)
                              return $ eq_zipwith_cat (addglue (res1) (res2) (res3) (res4) (res5) ("align")) (res6)
          C_TableText   -> do ag <- get
                              return $ (lexeme_Elem ag) : []
          C_NestedTable -> lines_Syn (Child 1)

---- Semantics Functions ----
sumList = sum

lengthList = length

eq_zeros = []

eq_zipwith_max :: [Int] -> [Int] -> [Int]
eq_zipwith_max [] l2 = l2
eq_zipwith_max l1 [] = l1
eq_zipwith_max (l1:l1s) (l2:l2s) = (max l1 l2) : (eq_zipwith_max l1s l2s)

maxList :: [Int] -> Int
maxList [] = 0
maxList (x:xs) = max x (maxList xs)

headList :: [Int] -> Int
headList [] = 0
headList (x:xs) = x

tailList :: [a] -> [a]
tailList [] = []
tailList (x:xs) = xs

eq_zipwith_cat :: [String] -> [String] -> [String]
eq_zipwith_cat l1 [] = l1
eq_zipwith_cat [] l2 = l2
eq_zipwith_cat (l11:l11s) (l22:l22s) = (l11 ++ "|" ++ l22) : (eq_zipwith_cat l11s l22s)

add_border_line :: [String] -> [String]
add_border_line [] = []
add_border_line (x:xs) = ("|" ++ x ++ "|") : (add_border_line xs)

addglue :: Int -> Int -> Int -> Int -> [String] -> String -> [String]
addglue aw mw ah mh lineS a = (glue_horizontal aw mw lineS a) ++ (glue_vertical_new (ah-mh) (add_vertical aw))

glue_horizontal :: Int -> Int -> [String] -> String -> [String]
glue_horizontal _ _ [] _ = []
glue_horizontal aw mw (l:ls) a = (add_hor l (aw-mw) a) : (glue_horizontal aw mw ls a)

add_hor :: String -> Int -> String -> String
add_hor l aw "left" = l ++ (hor_spaces aw)
add_hor l aw "right" = (hor_spaces aw) ++ l
add_hor l aw "center" = let y = (div aw 2)
            in (hor_spaces y) ++ l ++ (hor_spaces y)
add_hor l aw _ = l ++ (hor_spaces aw)

hor_spaces :: Int -> String
hor_spaces i = if (i <= 0) then "" else (repeatChar ' ' i)

glue_vertical_new :: Int -> [String] -> [String]
glue_vertical_new n l = if (n <= 0) then [] else l ++ (glue_vertical_new (n-1) l)

add_vertical :: Int -> [String]
add_vertical aw = if (aw <= 0) then [] else (repeatChar ' ' aw) : []

add_sepline :: Int -> [String]
add_sepline aw = if (aw <= 0)
        then []
        else ["|" ++ (repeatChar '-' aw) ++ "|"]

add_sep_line :: Int -> [String] -> [String] -> [String]
add_sep_line mw l [] = l
add_sep_line mw l rest = l ++ (add_sepline mw) ++ rest

add_elems :: Int -> Elems
add_elems 0 = NoElem
add_elems n = ConsElem (TableText " ") (add_elems (n-1))

repeatChar :: Char -> Int -> String
repeatChar _ 0 = []
repeatChar c i = c : (repeatChar c (i-1))

semantics :: Root -> String
semantics t = let ag           = toZipper $ buildMemoTree t
              in  printTable . fst $ runState (lines_Syn Local) ag







