{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Desk
  ( Program(..)
  , Expression(..)
  , Factor(..)
  , ConstName(..)
  , ConstPart(..)
  , ConstDefList(..)
  , ConstDef(..)
  , SymbolTable
  , isInSymbolTable
  , Desk.lookup
  , testData
  ) where

import           Data.Data
import qualified Data.List                     as List
import           Data.Maybe                     ( isJust )
import           GHC.Generics

data Program =
  PRINT Expression
        ConstPart
  deriving (Read, Show, Typeable, Data, Generic)

data Expression
  = Add Expression
        Factor
  | Fact Factor
  deriving (Read, Show, Typeable, Data, Generic)

data Factor
  = Name ConstName
  | Number String
  deriving (Read, Show, Typeable, Data, Generic)

data ConstName =
  Id String
  deriving (Read, Show, Typeable, Data, Generic)

data ConstPart
  = EmptyConstPart
  | WHERE ConstDefList
  deriving (Read, Show, Typeable, Data, Generic)

data ConstDefList
  = Comma ConstDefList
          ConstDef
  | Def ConstDef
  deriving (Read, Show, Typeable, Data, Generic)

data ConstDef =
  Equal ConstName
        String
  deriving (Read, Show, Typeable, Data, Generic)

type SymbolTable = [(String, String)]

isInSymbolTable :: String -> SymbolTable -> Bool
isInSymbolTable symbol table = isJust $ List.lookup symbol table

lookup :: String -> SymbolTable -> String
lookup symbol table = case List.lookup symbol table of
  Just value -> value
  Nothing ->
    error
      $! "Desk.lookup: Bug! Bug! Bug! symbol "
      ++ show symbol
      ++ " is not in the symbol table."

testData :: Program
testData =
  (PRINT (Add (Add (Fact (Name (Id "x"))) (Name (Id "y"))) (Number "1"))
         (WHERE (Comma (Def (Equal (Id "x") ("2"))) (Equal (Id "y") ("3"))))
  )
