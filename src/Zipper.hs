-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module      : Zipper
-- Description : A generic zipper.
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : GPL-3.0
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Zipper
  ( -- * Core types
    Zipper(..)
  , Context(..)
  , LocalContext(..)
  , Left(..)
  , Right(..)
  , Dissectible(..)
    -- * Entering and leaving
  , enter
  , leave
    -- * Movement
  , up
  , down
  , left
  , right
  ) where

-- import Data.Coerce
import Data.Constraint (Constraint)
import Data.Kind
-- import Data.Maybe
-- import Data.Proxy
-- import GHC.Generics
-- import Unsafe.Coerce

-- | The zipper. It consists of a hole and surrounding context.
--
-- @root@ specifies the type of the value for which the zipper was actually
-- created.
--
-- @c@ specifies some additional user-defined constraints. For example, we can
-- have a @'Zipper' 'Show' Tree@ which implies that each node in out tree is
-- 'Show'able:
--
-- > showCurrent :: 'Zipper' 'Show' a -> 'String'
-- > showCurrent ('Zipper' hole _) = 'show' hole
--
data Zipper (c :: Type -> Constraint) (root :: Type) =
  forall hole. (c hole, Dissectible c hole) =>
    Zipper { _zHole :: hole
           , _zCtxt :: !(Context c hole root)
           }

-- | Context consists of a 'LocalContext' and a path to the @root@ of the zipper.
data Context :: (Type -> Constraint) -> Type -> Type -> Type where
  RootContext :: forall c root. Context c root root
  (:>) :: forall c parent hole root rights. (c parent, Dissectible c parent)
       => !(Context c parent root)
       -> {-# UNPACK #-} !(LocalContext c hole rights parent)
       -> Context c hole root

-- | A dissection of a value into a constructor and some arguments.
data LocalContext c hole rights parent =
  LocalContext !(Left c (hole -> rights)) !(Right c rights parent)

-- | Encoding of arguments to the left of the hole.
data Left c expects where
  LOne  :: b -> Left c b
  LCons :: (c b, Dissectible c b) => Left c (b -> expects) -> b -> Left c expects

-- | Encoding of arguments to the right of the hole.
data Right c provides r where
  RNil  :: Right c r r
  RCons :: (c b, Dissectible c b) => b -> Right c provides r -> Right c (b -> provides) r

-- | A class of types that can can be dissected.
class Dissectible (c :: Type -> Constraint) (a :: Type) where
  dissect :: a -> Left c a

-- | Moves the 'Zipper' down.
down :: forall c root. Zipper c root -> Maybe (Zipper c root)
down (Zipper hole ctxt) = case dissect @c hole of
  LOne _ -> Nothing
  (xs `LCons` x) -> Just $ Zipper x (ctxt :> (LocalContext xs RNil))

localUp :: hole -> LocalContext c hole rights parent -> parent
localUp hole (LocalContext lefts rights) =
  (lefts `applyLefts` hole) `applyRights` rights
  where
    applyLefts :: Left c a -> a
    applyLefts (LOne x) = x
    applyLefts (f `LCons` x) = f `applyLefts` x
    applyRights :: a -> Right c a r -> r
    applyRights x RNil = x
    applyRights f (y `RCons` r) = f y `applyRights` r

-- | Moves the 'Zipper' up.
up :: forall c root. Zipper c root -> Maybe (Zipper c root)
up (Zipper _ RootContext) = Nothing
up (Zipper hole (p :> ctxt)) = Just $ Zipper (localUp hole ctxt) p

-- | Moves the 'Zipper' left.
left :: forall c root. Zipper c root -> Maybe (Zipper c root)
left (Zipper _ RootContext) = Nothing
left (Zipper hole (p :> (LocalContext lefts rights))) =
  case lefts of
    LOne _ -> Nothing
    (xs `LCons` x) ->
      Just $ Zipper x (p :> (LocalContext xs (hole `RCons` rights)))

-- | Moves the 'Zipper' right.
right :: forall c root. Zipper c root -> Maybe (Zipper c root)
right (Zipper _ RootContext) = Nothing
right (Zipper hole (p :> (LocalContext lefts rights))) =
  case rights of
    RNil -> Nothing
    (x `RCons` xs) ->
      Just $ Zipper x (p :> (LocalContext (lefts `LCons` hole) xs))

-- | Enters a 'Zipper'.
enter :: forall c root. (c root, Dissectible c root) => root -> Zipper c root
enter x = Zipper x RootContext

-- | Leaves the 'Zipper'.
leave :: forall c root. Zipper c root -> root
leave z = case top z of
  (Zipper hole RootContext) -> hole
  _ -> error $! "Bug! Bug! Bug!"
  where top x = case up x of
          Nothing -> x
          Just x' -> top x'

