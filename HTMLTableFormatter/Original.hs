--  Copyright (C) 2017 Joao Saraiva, Joao P. Fernandes, Pedro Martins,
--                     Alberto Pardo, Marcos Viera
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

{-# LANGUAGE DeriveDataTypeable #-}
module Original where

import Data.Maybe
import Data.Data
import Prelude
import Data.Generics.Zipper
import Language.Grammars.ZipperAG
import Shared

data Constructors = C_Root
                  | C_RootTable
                  | C_NoRow
                  | C_ConsRow
                  | C_OneRow
                  | C_NoElem
                  | C_ConsElem
                  | C_TableText
                  | C_NestedTable

constructor :: Zipper Root -> Constructors
constructor a = case ( getHole a :: Maybe Root ) of
                 Just (Root _) -> C_Root
                 otherwise -> case ( getHole a :: Maybe Table ) of
                                Just (RootTable _) -> C_RootTable
                                otherwise -> case ( getHole a :: Maybe Rows ) of
                                             Just (NoRow) -> C_NoRow
                                             Just (ConsRow _ _) -> C_ConsRow
                                             otherwise -> case ( getHole a :: Maybe Row ) of
                                                          Just (OneRow _) -> C_OneRow
                                                          otherwise -> case ( getHole a :: Maybe Elems ) of
                                                                       Just (NoElem) -> C_NoElem
                                                                       Just (ConsElem _ _) -> C_ConsElem
                                                                       otherwise -> case ( getHole a :: Maybe Elem ) of
                                                                                    Just (TableText _) -> C_TableText
                                                                                    Just (NestedTable _) -> C_NestedTable
                                                                                    otherwise -> error "Naha, that production does not exist!"

lexeme_Elem t = case ( getHole t :: Maybe Elem ) of
                     Just (TableText x) -> x
                     _ -> error "You should not be asking for that lexeme_Elem!"

---- AG ----
---- Computing the number of elems per row ----
n_Syn :: Zipper Root -> Int
n_Syn z = case (constructor z) of
            C_Root -> n_Syn $ z.$1
            C_RootTable -> maxList ( ns_Syn $ z.$1 )
            C_OneRow -> n_Syn $ z.$1
            C_NoElem -> 0
            C_ConsElem -> 1 + (n_Syn $ z.$2)

ns_Syn :: Zipper Root -> [Int]
ns_Syn z = case (constructor z) of
            C_NoRow -> []
            C_ConsRow -> (n_Syn $ z.$1) : (ns_Syn $ z.$2)

---- Passing down the number of elements per row ----
ane_Inh :: Zipper Root -> Int
ane_Inh z = case (constructor z) of
            C_RootTable -> n_Syn z
            C_NoRow -> case (constructor $ parent z) of
                        C_RootTable -> n_Syn $ parent z
                        C_NoRow -> ane_Inh $ parent z
                        C_ConsRow -> ane_Inh $ parent z
            C_ConsRow -> case (constructor $ parent z) of
                            C_RootTable -> n_Syn $ parent z
                            C_OneRow -> ane_Inh $ parent z
                            C_ConsRow -> ane_Inh $ parent z
            C_OneRow -> ane_Inh $ parent z
            C_NoElem -> case (constructor $ parent z) of
                            C_OneRow -> ane_Inh $ parent z
                            C_ConsElem -> (ane_Inh $ parent z) - 1
                            C_NoElem -> (ane_Inh $ parent z) - 1
            C_ConsElem -> case (constructor $ parent z) of
                            C_OneRow -> ane_Inh $ parent z
                            C_ConsElem -> (ane_Inh $ parent z) - 1
                            C_NoElem -> (ane_Inh $ parent z) - 1

---- Computing the minimal height of each construct ----
mh_Syn :: Zipper Root -> Int
mh_Syn z = case (constructor z) of
            C_Root -> mh_Syn $ z.$1
            C_RootTable -> mh_Syn $ z.$1
            C_NoRow -> 0
            C_ConsRow -> (mh_Syn $ z.$1) + 1 + (mh_Syn $ z.$2)
            C_OneRow -> mh_Syn $ z.$1
            C_ConsElem -> max (mh_Syn $ z.$1) (mh_Syn $ z.$2)
            C_NoElem -> 0
            C_TableText -> 1
            C_NestedTable -> (mh_Syn $ z.$1 ) + 1

---- Computing the minimal width of each construct ----
mw_Syn :: Zipper Root -> Int
mw_Syn z = case (constructor z) of
            C_Root -> mw_Syn $ z.$1
            C_RootTable -> lmw_Local z -- Local attr, as defined in LRC
            C_TableText -> length (lexeme_Elem z)
            C_NestedTable -> (mw_Syn $ z.$1) + 2

mws_Syn :: Zipper Root -> [Int]
mws_Syn z = case (constructor z) of
                C_NoRow -> []
                C_ConsRow -> eq_zipwith_max (mws_Syn $ z.$1) (mws_Syn $ z.$2)
                C_OneRow -> mws_Syn $ z.$1
                C_ConsElem -> (mw_Syn $ z.$1) : (mws_Syn $ z.$2)
                C_NoElem -> []

---- LOCAL ATTRIBUTE ----
lmw_Local :: Zipper Root -> Int
lmw_Local z = case (constructor z) of
                    C_RootTable -> (sumList (mws_Syn $ z.$1)) + (lengthList (mws_Syn $ z.$1)) - 1
                    C_ConsRow -> (sumList (aws_Inh z)) + (lengthList (aws_Inh z)) - 1

---- Passing down the available heights and widths ----
ah_Inh :: Zipper Root -> Int
ah_Inh z = case (constructor z) of
            C_Root -> mh_Syn $ z
            C_RootTable -> case (constructor $ parent z) of
                            C_Root -> ah_Inh $ parent z
                            C_ConsElem -> ah_Inh $ parent z
            C_ConsElem ->case (constructor $ parent z) of
                            C_OneRow -> mh_Syn z
                            C_ConsElem -> ah_Inh $ parent z
            C_NoElem -> case (constructor $ parent z) of
                            C_OneRow -> mh_Syn z
                            C_ConsElem -> ah_Inh $ parent z
            C_TableText   -> ah_Inh $ parent z
            C_NestedTable -> ah_Inh $ parent z

aws_Inh :: Zipper Root -> [Int]
aws_Inh z = case (constructor z) of
                C_ConsRow ->case (constructor $ parent z) of
                                C_RootTable -> mws_Syn z
                                C_ConsRow -> aws_Inh $ parent z
                C_NoRow -> case (constructor $ parent z) of
                                C_RootTable -> mws_Syn z
                                C_ConsRow -> aws_Inh $ parent z
                C_OneRow -> aws_Inh $ parent z
                C_ConsElem -> case (constructor $ parent z) of
                                C_OneRow -> aws_Inh $ parent z
                                C_ConsElem -> tailList (aws_Inh $ parent z)
                C_NoElem -> case (constructor $ parent z) of
                                C_OneRow -> aws_Inh $ parent z
                                C_ConsElem -> tailList (aws_Inh $ parent z)

aw_Inh :: Zipper Root -> Int
aw_Inh z = case (constructor z) of
            C_Root -> mw_Syn z
            C_RootTable   -> case (constructor $ parent z) of
                            C_Root -> ah_Inh $ parent z
                            C_NestedTable -> aw_Inh $ parent z
            C_TableText   -> headList (aws_Inh $ parent z)
            C_NestedTable -> headList (aws_Inh $ parent z)

---- Computing Formatted Table ----
lines_Syn :: Zipper Root -> [String]
lines_Syn z = case (constructor z) of
                    C_Root       -> lines_Syn $ z.$1
                    C_RootTable   -> (add_sepline (lmw_Local z)) ++ (lines_Syn $ z.$1) ++ (add_sepline (lmw_Local z))
                    C_NoRow       -> []
                    C_ConsRow     -> add_sep_line (lmw_Local z) (lines_Syn $ z.$1) (lines_Syn $ z.$2)
                    C_OneRow      -> add_border_line (lines_Syn $ z.$1)
                    C_NoElem      -> []
                    C_ConsElem    -> let ag = addglue (aw_Inh $ z.$1) (mw_Syn $ z.$1) (ah_Inh $ z.$1) (mh_Syn $ z.$1) (lines_Syn $ z.$1) ("align")
                                     in eq_zipwith_cat ag (lines_Syn $ z.$2)
                    C_TableText   -> lexeme_Elem z : []
                    C_NestedTable -> lines_Syn $ z.$1

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
semantics t = printTable $ lines_Syn $ (toZipper t)
