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