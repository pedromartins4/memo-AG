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

{-# LANGUAGE DeriveDataTypeable #-}
module Shared where

import Language.Grammars.ZipperAG
import Data.Generics.Zipper
import Data.Maybe
import Data.Data

data Root = Root Its
          deriving (Typeable, Data)

data Its = ConsIts It Its
         | NilIts
       deriving (Typeable, Data)

data It = Decl String
        | Use String
        | Block Its
        deriving (Typeable, Data)


instance Show Root where
   show = showRoot

showRoot (Root its) = "[ " ++ showIts its ++ " ]\n"

instance Show Its where
   show = showIts


showIts (ConsIts it NilIts) = show it
showIts (ConsIts it its)    = show it ++ "," ++ showIts its
showIts NilIts              = ""

instance Show It where
  show = showIt

showIt (Decl s) = "Decl " ++ s
showIt (Use s) = "Use " ++ s
showIt (Block its) = "[ " ++ showIts its ++ " ]"




type SymbolTable = [(String, Int)]

type Error       = [String]

(.$>) :: Zipper a -> Int -> Zipper a
zipper .$> n = let current = aux zipper 1
               in  (parent zipper).$(current+n)

(.$<) :: Zipper a -> Int -> Zipper a
zipper .$< n = let current = aux zipper 1
               in  (parent zipper).$(current-n)

aux :: Zipper a -> Int -> Int
aux m n = case left m of
                     Nothing  -> n                     
                     Just m'  -> aux m' (n+1)

--program = [Decl "y", Decl "x", Block [Decl "y", Use "y", Use "w"], Decl "x", Use "y"]
block = Block (ConsIts (Decl "x") (ConsIts (Use "y") (ConsIts (Use "w") (NilIts))))
program = Root $ ConsIts (Decl "y") (ConsIts (Decl "x") (ConsIts (block) (ConsIts (Decl "x") (ConsIts (Use "y") (NilIts)))))
