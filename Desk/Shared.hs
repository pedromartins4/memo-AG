--  Copyright (C) 2017 Joao Saraiva, Joao P. Fernandes, Pedro Martins,
--                Alberto Pardo, Marcos Viera
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
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveDataTypeable #-}
module Shared where

import Language.Grammars.ZipperAG
import Data.Generics.Zipper
import Data.Maybe
import Data.Data

data Root = Root Program
               deriving (Show, Typeable, Data)

data Program = PRINT Expression ConstPart
               deriving (Show, Typeable, Data)

{- Keeping it simple by just having sums -}
data Expression = Add Expression Factor
                | Fact Factor
               deriving (Show, Typeable, Data)

data Factor = Name ConstName
            | Number String
               deriving (Show, Typeable, Data)

data ConstName = Id String
               deriving (Show, Typeable, Data)
{-----------------------------------------}
data ConstPart = EmptyConstPart
               | WHERE ConstDefList
               deriving (Show, Typeable, Data)

data ConstDefList = Comma ConstDefList ConstDef
                  | Def ConstDef
               deriving (Show, Typeable, Data)

data ConstDef = Equal ConstName String
               deriving (Show, Typeable, Data)

type SymbolTable = [(String,String)]

{---------------Tests---------------}
expr = Add (Add (Fact (Name (Id "x"))) (Name (Id "y"))) (Number "1")
deflst = WHERE (Comma (Def (Equal (Id "x") ("2"))) (Equal (Id "y") ("3")))
program = Root (PRINT expr deflst)

--PRINT x + y + 1 WHERE y = 2, x = 3