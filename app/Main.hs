{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module      : Main
-- Description : Repmin example
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : BSD3
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Main where

import Data.Maybe
import Data.Kind
-- import Data.Proxy

import GHC.Generics
-- import GHC.TypeLits

import Zipper

{-
class WhereAmI' p (a :: Symbol) where
  position' :: Proxy a -> p

at :: forall a p. WhereAmI' p a => p
at = position' (Proxy :: Proxy a)
-}

-- User code
--------------------------------------------------------------------------------

-- | A somewhat non-standard binary tree. This is an example of a user-defined
-- data structure.
data Tree
  = Fork Tree
         Tree
  | Leaf Int
  deriving (Show, Read, Generic)

-- | Definition of the "globmin" attribute.
--
globmin :: Zipper (WhereAmI Position) Tree -> Int
globmin z = case up z of
  Nothing -> locmin z
  Just z' -> globmin z'

-- | Definition of the "locmin" attribute.
--
-- @WhereAmI Position@ allows one to pattern match using 'Position' GADT.
locmin :: Zipper (WhereAmI Position) Tree -> Int
locmin z@(Zipper hole _) = case position hole of
  -- The cool thing here is that the type of @'C_Leaf'@ is @'Position Tree'@
  -- which means that after pattern matching on @'C_Leaf'@ GHC knows that
  -- @hole ~ Tree@ which allows us to actualy pattern match on @'Leaf'@.
  C_Leaf -> let (Leaf x) = hole in x
  -- These calls to @'fromJust'@ are kind of ugly... They are safe, because we
  -- know that we're at a Fork which implies that there are exactly two
  -- children, but ugly :)
  C_Fork -> let y = fromJust (down z)
                x = fromJust (left y)
                -- Later on, this should probably become a fold over children...
             in min (locmin x) (locmin y)

-- | Definition of the "replace" attribute (I'm not sure one can actually call
-- it an attribute though).
replace :: Zipper (WhereAmI Position) Tree -> Tree
replace z@(Zipper hole _) = case position hole of
  C_Leaf -> Leaf (globmin z)
  C_Fork -> let y = fromJust (down z)
                x = fromJust (left y)
                -- Later on, this should probably become a map over children...
             in Fork (replace x) (replace y)

-- Library magic
--------------------------------------------------------------------------------

-- | This should definitely be generated using TH or GHC.Generics. I'd prefer
-- the latter, but I'm not sure it's possible.
instance c Tree => Dissectible c Tree where
  dissect (Fork l r) = LOne Fork `LCons` l `LCons` r
  dissect x          = LOne x

-- | Represents the position in our forest of data structures.
data Position :: Type -> Type where
  C_Leaf :: Position Tree
  C_Fork :: Position Tree

-- | Represents the ability to "look around".
class WhereAmI (p :: Type -> Type) (a :: Type) where
  position :: a -> p a

instance WhereAmI Position Tree where
  position (Fork _ _) = C_Fork
  position (Leaf _) = C_Leaf

{-
instance WhereAmI' (Position Tree) "Fork" where
  position' _ = C_Fork

instance WhereAmI' (Position Tree) "Leaf" where
  position' _ = C_Leaf
-}

-- An example
--------------------------------------------------------------------------------

t1 :: Tree
t1 = Fork (Fork (Leaf 123)
                (Leaf 0))
          (Leaf 5)

main :: IO ()
main = print . replace . enter $ t1
