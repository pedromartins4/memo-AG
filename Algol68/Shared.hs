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
