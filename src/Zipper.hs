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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}

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
  , Memo(..)
  , Context(..)
  , LocalContext(..)
  , Left(..)
  , Right(..)
  , Dissectible(..)
  , ToMaybe
    -- * Entering and leaving
  , enter
  , leave
    -- * Movement
  , up
  , down
  , left
  , right
  , child
  , whichChild
   -- * Working with the Memo table
  , aget
  , aput
  ) where

-- import Data.Coerce
import           Control.Monad                  ( (>=>) )
import           Data.Default.Class
import           Data.Constraint                ( Constraint )
import           Data.Kind
import           GHC.TypeLits
-- import           Data.Maybe                     ( fromJust )
-- import Data.Proxy
-- import GHC.Generics
-- import Unsafe.Coerce

import           Data.Vinyl              hiding ( RNil )
import qualified Data.Vinyl.ARec               as Vinyl
import           Data.Vinyl.TypeLevel           ( RIndex
                                                , NatToInt
                                                )
-- import           Data.Vinyl.Functor             ( Thunk )

-- | The AG zipper. It consists of two zippers: @_zHole@ and @_zCxt@ constitute
-- a generic zipper into a user-defined data structure (see the Scrap Your
-- Zippers paper for more info); @_zMemo@ is a zipper into an infinite cache.
--
-- @c@ specifies additional constraints all elements of the user-defined data
-- structures satisfy.
--
-- @attributes@ specifies the memoized attributes.
--
-- @root@ specifies the type of the value for which the zipper was actually
-- created.
data Zipper (c :: Type -> Constraint)
            (attributes :: [(Symbol, Type)])
            (root :: Type) =
  forall hole. (c hole, Dissectible c hole) =>
    Z { _zHole :: hole
      , _zMemo :: !(Memo attributes)
      , _zCxt  :: !(Context c hole root)
      }

type family ToMaybe (xs :: [(Symbol, Type)]) where
  ToMaybe '[] = '[]
  ToMaybe ( '(s,t) ': xs ) = '(s, Maybe t) ': (ToMaybe xs)

data Memo (attributes :: [(Symbol, Type)]) =
  M { _mCurrent :: {-# UNPACK #-}!(AFieldRec (ToMaybe attributes))
    , _mLeft    :: Memo attributes
    , _mRight   :: Memo attributes
    , _mUp      :: Memo attributes
    , _mDown    :: Memo attributes
    }


-- | Returns the specified attribute.
aget :: forall (name :: Symbol)
               (field :: (Symbol, Type))
               (x :: Type)
               c
               attributes
               root.
        ( KnownSymbol name
        , NatToInt (RIndex field (ToMaybe attributes))
        , field ~ '(name, Maybe x)
        ) => Zipper c attributes root -> Maybe x
aget (Z _ m _) = case Vinyl.aget @field (_mCurrent m) of (Field x) -> x

-- | Updates the specified attribute.
aput :: forall (name :: Symbol)
               (field :: (Symbol, Type))
               (x :: Type)
               c
               attributes
               root.
        ( KnownSymbol name
        , NatToInt (RIndex field (ToMaybe attributes))
        , field ~ '(name, Maybe x)
        ) => x -> Zipper c attributes root -> Zipper c attributes root
aput x z@(Z _ m _) = let f = _mCurrent m
                         f' = Vinyl.aput @field (Field (Just x)) f
                      in z { _zMemo = m { _mCurrent = f' } }

-- | Context consists of a 'LocalContext' and a path to the @root@ of the
-- zipper.
data Context :: (Type -> Constraint) -> Type -> Type -> Type where
  RootContext :: forall c root. Context c root root
  (:>) :: forall c parent root hole rights. (c parent, Dissectible c parent)
       => !(Context c parent root)
       -> {-# UNPACK #-} !(LocalContext c hole rights parent)
       -> Context c hole root

-- | A dissection of a value into a constructor and some arguments.
data LocalContext c hole rights parent =
  LocalContext !(Left c (hole -> rights)) !(Right c rights parent)

-- | Encoding of arguments to the left of the hole.
data Left c expects where
  LOne  :: b -> Left c b
  LCons :: (c b, Dissectible c b)
        => Left c (b -> expects) -> b -> Left c expects

-- | Encoding of arguments to the right of the hole.
data Right c provides r where
  RNil  :: Right c r r
  RCons :: (c b, Dissectible c b)
        => b -> Right c provides r -> Right c (b -> provides) r

-- | A class of types that can can be dissected.
class Dissectible (c :: Type -> Constraint) (a :: Type) where
  dissect :: a -> Left c a

-- | Moves the 'Zipper' down.
down :: forall c as root. Zipper c as root -> Maybe (Zipper c as root)
down (Z hole memo ctxt) = case dissect @c hole of
  LOne _         -> Nothing
  (xs `LCons` x) -> Just $ Z x (memoDown memo) (ctxt :> (LocalContext xs RNil))
  where memoDown m = (_mDown m) { _mUp = m }

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
up :: forall c as root. Zipper c as root -> Maybe (Zipper c as root)
up (Z _ _ RootContext) = Nothing
up (Z hole memo (p :> ctxt)) = Just $ Z (localUp hole ctxt) (memoUp memo) p
  where memoUp m = case ctxt of
          (LocalContext _ RNil) -> (_mUp m) { _mDown = m }
          _ -> _mUp m

-- | Moves the 'Zipper' left.
left :: forall c as root. Zipper c as root -> Maybe (Zipper c as root)
left (Z _ _ RootContext) = Nothing
left (Z hole memo (p :> (LocalContext lefts rights))) =
  case lefts of
    LOne _ -> Nothing
    (xs `LCons` x) ->
      Just $ Z x (memoLeft memo) (p :> (LocalContext xs (hole `RCons` rights)))
  where memoLeft m = (_mLeft m) { _mRight = m, _mUp = _mUp m }

-- | Moves the 'Zipper' right.
right :: forall c as root. Zipper c as root -> Maybe (Zipper c as root)
right (Z _ _ RootContext) = Nothing
right (Z hole memo (p :> (LocalContext lefts rights))) =
  case rights of
    RNil -> Nothing
    (x `RCons` xs) ->
      Just $ Z x (memoRight memo) (p :> (LocalContext (lefts `LCons` hole) xs))
  where memoRight m = (_mRight m) { _mLeft = m }

child :: Int -> Zipper c as root -> Maybe (Zipper c as root)
child !n !z | n < 0 = error $! "Expected a natural number, but got: " <> show n
            | otherwise = toNth 0 =<< (theLeftOne <$> down z)
 where
   theLeftOne !x = case left x of
     Nothing -> x
     Just x' -> theLeftOne x'
   toNth !i | i == n    = return
            | otherwise = right >=> toNth (i + 1)

whichChild :: Zipper c as root -> Int
whichChild !x = go x 0
  where
    go !z !n = case left z of
      Nothing -> n
      Just z' -> go z' (n + 1)

-- | Enters a 'Zipper'.
enter :: forall c as root. (c root, Dissectible c root, Default (Memo as))
      => root -> Zipper c as root
enter x = Z x def RootContext

-- | Leaves the 'Zipper'.
leave :: forall c as root. Zipper c as root -> root
leave z = case top z of
  (Z hole _ RootContext) -> hole
  _ -> error $! "Bug! Bug! Bug!"
  where top x = case up x of
          Nothing -> x
          Just x' -> top x'

