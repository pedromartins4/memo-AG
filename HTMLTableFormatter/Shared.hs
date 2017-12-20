{-# LANGUAGE DeriveDataTypeable #-}
module Shared where

import Language.Grammars.ZipperAG
import Control.Monad.State.Lazy
import Data.Generics.Zipper
import Data.Data

---- ABSTRACT SYNTAX GRAMMAR ----
data Root = Root Table
    deriving (Typeable, Show, Data)

data Table = RootTable Rows
    deriving (Typeable, Show, Data)

data Rows = NoRow
          | ConsRow Row Rows
    deriving (Typeable, Show, Data)

data Row = OneRow Elems
    deriving (Typeable, Show, Data)

data Elems = NoElem
           | ConsElem Elem Elems
    deriving (Typeable, Show, Data)

data Elem = TableText String
          | NestedTable Table
    deriving (Typeable, Show, Data)

---- Tests
nestedtable = RootTable (ConsRow (OneRow (ConsElem (TableText "Some more random text!") (NoElem))) (NoRow))
elem1 = TableText "This is some text on a table!"
elem2 = TableText "And even more random text!"
row1 = ConsRow (OneRow (ConsElem (TableText "This is a big phrase etc etc.") NoElem)) (NoRow)
elem3 = ConsElem (TableText "This is a big phrase just to make sure this HTML AG etc etc works.") (NoElem)

table = Root (RootTable (ConsRow (OneRow (ConsElem (elem1) (ConsElem (NestedTable nestedtable) (NoElem)))) (ConsRow (OneRow (ConsElem (elem2) (elem3))) (row1))))

printTable :: [String] -> String
printTable [] = ""
printTable (x:xs) = x ++ "\n" ++ (printTable xs)
