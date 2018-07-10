--  Copyright (C) 2017 Joao Saraiva, Joao P. Fernandes, Pedro Martins,
--  				   Alberto Pardo, Marcos Viera
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
--	along with this program. If not, see <http://www.gnu.org/licenses/>.

module Shared where

-- Tree for ZAG1, ZAG2, ZAG3 and ZAG4

data Tree = Fork Tree Tree | Leaf Int
 deriving Show

t = Fork (Fork (Leaf 1)
               (Leaf 2))
         (Fork (Leaf 3)
               (Leaf 4))
