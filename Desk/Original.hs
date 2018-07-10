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
module Original where

import Data.Maybe
import Data.Data
import Prelude
import Data.Generics.Zipper
import Language.Grammars.ZipperAG
import Shared

data Constructors = C_PRINT
                  | C_Add
                  | C_Fact
                  | C_Name
                  | C_Number
                  | C_Id
                  | C_EmptyConstPart
                  | C_WHERE
                  | C_Comma
                  | C_Def
                  | C_Equal
                  | C_Root

constructor :: Zipper Root -> Constructors
constructor a = case ( getHole a :: Maybe Program ) of
                   Just (PRINT _ _) -> C_PRINT
                   otherwise -> case ( getHole a :: Maybe Expression ) of
                                Just (Add _ _) -> C_Add
                                Just (Fact _) -> C_Fact
                                otherwise -> case ( getHole a :: Maybe Factor ) of
                                             Just (Name _) -> C_Name
                                             Just (Number _) -> C_Number
                                             otherwise -> case ( getHole a :: Maybe ConstName ) of
                                                          Just (Id _) -> C_Id
                                                          otherwise -> case ( getHole a :: Maybe ConstPart ) of
                                                                       Just (EmptyConstPart) -> C_EmptyConstPart
                                                                       Just (WHERE _) -> C_WHERE
                                                                       otherwise -> case ( getHole a :: Maybe ConstDefList ) of
                                                                                    Just (Comma _ _) -> C_Comma
                                                                                    Just (Def _) -> C_Def
                                                                                    otherwise -> case ( getHole a :: Maybe ConstDef ) of
                                                                                                 Just (Equal _ _) -> C_Equal
                                                                                                 otherwise -> case ( getHole a :: Maybe Root) of
                                                                                                    Just (Root _) -> C_Root
                                                                                                    _ -> error "That production does not exist!"

lexeme :: Zipper Root -> String
lexeme t = case ( getHole t :: Maybe ConstName ) of
              Just (Id x) -> x
              _ -> case( getHole t :: Maybe ConstDef ) of
                   Just (Equal _ x) -> x
                   _ -> case ( getHole t :: Maybe Factor ) of
                         Just (Number x) -> x

---- AG ----

---- Inherited -----
envi :: Zipper Root -> SymbolTable
envi t = case (constructor t) of
            C_PRINT -> envs ( t.$2 )
            _ -> envi (parent t)

---- Synthesized ----
code :: Zipper Root -> String
code t = case (constructor t) of
            C_Root -> code ( t.$1 )
            C_PRINT -> if ok ( t.$2 )
                        then code ( t.$1 ) ++ "PRINT, 0\n" ++ "HALT,  0\n"
                        else "HALT,  0\n"
            C_Add -> if (ok ( t.$2 ))
                        then code ( t.$1 ) ++ "ADD,   " ++ value ( t.$2 ) ++ "\n"
                        else "HALT,  0\n"
            C_Fact -> if (ok ( t.$1 ))
                       then "LOAD,  " ++ value ( t.$1 ) ++ "\n"
                       else "HALT,  0\n"

value :: Zipper Root -> String
value t = case (constructor t) of
            C_Name -> getValue (name ( t.$1 )) (envi t)
            C_Number -> lexeme t
            C_Equal -> lexeme t

ok :: Zipper Root -> Bool
ok t = case (constructor t) of
        C_Name -> isInST (name ( t.$1 )) (envi t)
        C_Number -> True
        C_EmptyConstPart -> True
        C_WHERE -> ok ( t.$1 )
        C_Comma -> ok ( t.$1 ) && (not (isInST (name ( t.$2 )) (envs ( t.$1 ))) )
        C_Def -> True

name :: Zipper Root -> String
name t = case (constructor t) of
            C_Id -> lexeme t
            C_Equal -> name $ (t.$1)

envs :: Zipper Root -> SymbolTable            
envs t = case (constructor t) of
            C_EmptyConstPart -> []
            C_WHERE -> envs( t.$1 )
            C_Comma -> envs( t.$1 ) ++ [(name ( t.$2 ), value ( t.$2 ))]
            C_Def -> [( name ( t.$1 ), value ( t.$1) )]

{-Semantic Functions-}
isInST :: String -> SymbolTable -> Bool
isInST _ [] = False 
isInST c ((a,b):xs) = if (c==a) then True else isInST c xs

getValue :: String -> SymbolTable -> String
getValue c ((a,b):xs) = if (c==a) then b else (getValue c xs)

--

semantics :: Root -> String
semantics t = code (toZipper t)



