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

import Language.Grammars.ZipperAG
import Data.Generics.Zipper
import Data.Maybe
import Data.Data
import Shared

data Constructor = C_ConsIts
                 | C_NilIts
                 | C_Decl
                 | C_Use
                 | C_Block
                 | C_Root

constructor :: (Typeable a) => Zipper a -> Constructor
constructor a = case ( getHole a :: Maybe Its ) of
                 Just (ConsIts _ _) -> C_ConsIts
                 Just (NilIts) -> C_NilIts
                 otherwise -> case ( getHole a :: Maybe It ) of
                                Just (Decl _) -> C_Decl
                                Just (Use _) -> C_Use
                                Just (Block _) -> C_Block
                                otherwise -> case ( getHole a :: Maybe Root) of 
                                                Just (Root _) -> C_Root
                                                otherwise -> error "Naha, that production does not exist!"

lexeme z = case (getHole z :: Maybe It) of
            Just (Use x) -> x
            Just (Decl x) -> x

---- Synthesized Attributes ----
dclo :: Zipper Root -> SymbolTable
dclo z = case (constructor z) of
                    C_ConsIts -> dclo $ z.$2
                    C_NilIts -> dcli z
                    C_Use -> dcli z
                    C_Decl -> (lexeme z,lev z) : (dcli z)
                    C_Block -> dcli z

errs :: Zipper Root -> [String]
errs z = case (constructor z) of
                    C_Root    -> errs $ z.$1    
                    C_NilIts  -> []
                    C_ConsIts -> (errs $ z.$1) ++ (errs $ z.$2)
                    C_Use     -> mBIn (lexeme z) (env z)
                    C_Decl    -> mNBIn (lexeme z,lev z) (dcli z)
                    C_Block   -> errs $ z.$1

---- Inheritted Attributes ----
dcli :: Zipper Root -> SymbolTable 
dcli z = case (constructor z) of
                    C_Root -> []
                    C_NilIts -> case (constructor $ parent z) of
                                    C_ConsIts -> dclo $ (z.$<1)
                                    C_Block -> env $ parent z
                                    C_Root -> []
                    C_ConsIts -> case (constructor $ parent z) of
                                    C_ConsIts -> dclo $ (z.$<1)
                                    C_Block -> env $ parent z
                                    C_Root -> []
                    C_Block -> dcli $ parent z
                    C_Use   -> dcli $ parent z
                    C_Decl  -> dcli $ parent z

lev :: Zipper Root -> Int
lev z = case (constructor z) of
                C_Root -> 0
                C_NilIts -> case (constructor $ parent z) of
                                C_Block -> (lev $ parent z) + 1
                                C_ConsIts -> lev $ parent z
                                C_Root -> 0
                C_ConsIts -> case (constructor $ parent z) of
                                C_Block -> (lev $ parent z) + 1
                                C_ConsIts -> lev $ parent z
                                C_Root -> 0
                C_Block -> lev $ parent z
                C_Use   -> lev $ parent z
                C_Decl  -> lev $ parent z

env :: Zipper Root -> SymbolTable
env z = case (constructor z) of
                    C_NilIts -> case (constructor $ parent z) of
                                    C_Block -> dclo z
                                    C_ConsIts -> env $ parent z
                                    C_Root -> dclo z
                    C_ConsIts -> case (constructor $ parent z) of
                                    C_Block -> dclo z
                                    C_ConsIts -> env $ parent z
                                    C_Root -> dclo z
                    C_Block -> env $ parent z
                    C_Use   -> env $ parent z
                    C_Decl  -> env $ parent z
                    C_Root  -> dclo z

{- Environment lookup functions -}
mBIn name [] = [name]
mBIn name ((n,l):es) = if (n==name) then [] else mBIn name es

mNBIn tuple [] = [] 
mNBIn pair (pl:es) = if pair==pl then [fst pair] else mNBIn pair es

semantics t = errs $ toZipper $ t
