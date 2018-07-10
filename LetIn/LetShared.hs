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
module LetShared where

import Language.Grammars.ZipperAG
import Data.Generics.Zipper
import Data.Dynamic
import Data.Maybe
import Data.Data

{-
instance Data Dynamic where
  toConstr   = error "toConstr"
  gunfold    = error "gunfold" 
  dataTypeOf = error "dataTypeOf"
-}

type SymbolTable = [(String, Int)]

-- Data type
data Root = Root Let
           deriving (Data, Typeable)

data Let = Let List In
          deriving (Data, Typeable)

data In = In A
         deriving (Data, Typeable)

data List = ConsLet    String Let List
          | ConsAssign String A   List
          | EmptyList
          deriving (Data, Typeable)

data A = Plus A A
       | Minus A A
       | Time A A
       | Divide A A
       | Variable String
       | Constant Int
       deriving (Data, Typeable)


sizeLetProg :: Root -> Int
sizeLetProg (Root lt) = sizeLet lt

sizeLet (Let l i) = sizeLetList l

sizeLetList (ConsLet _ lt l) = 1 + (sizeLet lt) + sizeLetList l
sizeLetList (ConsAssign _ _ l) = 1 + sizeLetList l
sizeLetList EmptyList          = 0



depthLetProg :: Root -> Int
depthLetProg (Root lt) = depthLet lt

depthLet (Let l i)  = 1 + depthLetList l

depthLetList (ConsLet _ lt l)   = max (depthLet lt) (depthLetList l)
depthLetList (ConsAssign _ _ l) = depthLetList l
depthLetList EmptyList          = 0



instance Show Root where
   show = showRoot

showRoot (Root l) = showLet l


instance Show Let where
   show = showLet

showLet (Let l i) = "let { " ++ showLL l ++ "} in " ++  (showIn i)

instance Show In where
   show = showIn

showIn (In a)     = showA a


instance Show List where
   show = showLL

showLL (ConsLet s l ll)    = s ++ " = " ++ showLet l ++ ";\n " ++ showLL ll
showLL (ConsAssign s a ll) = s ++ " = " ++ showA a   ++ ";\n " ++ showLL ll 
showLL _                   = ""


instance Show A where
   show = showA

showA (Plus al ar)   = (showA al) ++ " + " ++ (showA ar)
showA (Minus al ar)  = (showA al) ++ " - " ++ (showA ar)
showA (Time al ar)   = (showA al) ++ " * " ++ (showA ar)
showA (Divide al ar) = (showA al) ++ " / " ++ (showA ar)
showA (Variable s)   = s 
showA (Constant i)   = show i



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

---- Examples ----
a1 = let a = b + 3
         c = 8
         w = let  z = a * b
             in   z * b   
         b = (c * 3) - c
     in  (c * w) - a
a = Root (Let
             (ConsAssign "a" (Plus (Variable "b") (Constant 3)) 
             (ConsAssign "c" (Constant 8) 
             (ConsLet    "w" (Let (ConsAssign "z" (Time (Variable "a") (Variable "b")) EmptyList) 
                             (In  (Time (Variable "z") (Variable "b"))))
             (ConsAssign "b" (Minus (Time (Variable "c") (Constant 3)) (Variable "c")) EmptyList))))
         (In (Minus (Time (Variable "c") (Variable "w")) (Variable "a"))))

b1 = let c = 1
         a = let w = let a = 5
                         b = a
                     in  b + c
             in  w
     in  a + c
b = Root (Let 
             (ConsAssign "c" (Constant 1) 
             (ConsLet "a" (Let (ConsLet "w" (Let
                                                (ConsAssign "a" (Constant 5)
                                                (ConsAssign "b" (Variable "a") EmptyList))
                                            (In (Plus (Variable "b") (Variable "c"))))
              EmptyList)
                          (In  (Variable "w"))) EmptyList))
             (In (Plus (Variable "a") (Variable "c"))))

c1 = let a = 5
         b = a
     in  b
c = Root (Let
             (ConsAssign "a" (Constant 5)
             (ConsAssign "b" (Variable "a") EmptyList))
         (In (Variable "b")))

d1 = let a = b+3
         c = 8
         b = c*3 - c
     in  c*5 - a
d = Root (Let (ConsAssign "a" (Plus (Variable "b") (Constant 3))
              (ConsAssign "c" (Constant 8)
              (ConsAssign "b" (Minus (Time (Variable "c") (Constant 3)) (Variable "c")) EmptyList)))
         (In  (Minus (Time (Variable "c") (Constant 5)) (Variable "a"))))

-- Exemplo de circularidade do Paakki
e1 = let x = y
         y = z
         z = 2
     in  x
e = Root (Let (ConsAssign "x" (Variable "y")
              (ConsAssign "y" (Variable "z")
              (ConsAssign "z" (Constant 2) EmptyList)))
         (In  (Variable "x")))

f1 = let a = b + 3
         c = 8
         w = let  z = a * b
             in   z * b   
         b = let  c = 1
             in   c + 4
     in  c * w - a
f = Root (Let (ConsAssign "a" (Plus (Variable "b") (Constant 3))
              (ConsAssign "c" (Constant 8)
              (ConsLet "w" (Let (ConsAssign "z" (Time (Variable "a") (Variable "b")) EmptyList)
                           (In  (Time (Variable "z") (Variable "b"))))
              (ConsLet "b" (Let (ConsAssign "c" (Constant 1) EmptyList)
                           (In  (Plus (Variable "c") (Constant 4)))) EmptyList))))
         (In  (Minus (Time (Variable "c") (Variable "w")) (Variable "a"))))




















