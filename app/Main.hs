{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      : Main
-- Description : Repmin example
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : BSD3
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Main where

import Prelude hiding (Either(..))

import Control.Monad.Trans.State.Strict
import Data.Constraint
import Data.Default.Class
import Data.Kind
import Data.Proxy
import Debug.Trace
import GHC.Generics
import GHC.TypeLits

import Data.Vinyl (FieldRec, ElField(..), Rec(..), RecElem(..))
import Data.Vinyl.TypeLevel (RIndex, NatToInt)
import qualified Data.Vinyl as Vinyl
-- import qualified Data.Vinyl.ARec as Vinyl

import qualified Zipper as Internal
import Zipper (Zipper(..), Memo(..), Dissectible(..), aget, aput)


pattern Zipper :: () => (c hole, Dissectible c hole) => hole -> Zipper c attributes root
pattern Zipper h <- (Internal.Z h _ _)

instance Default (Memo Attributes) where
  def = M { _mCurrent = Vinyl.toARec $ Field Nothing :& Field Nothing :& Vinyl.RNil
          , _mLeft = def
          , _mRight = def
          , _mUp = def
          , _mDown = def
          }

memo ::
     forall (name :: Symbol)
            (field :: (Symbol, Type))
            (attributes :: [(Symbol, Type)])
            (x :: Type)
            c
            root.
     ( KnownSymbol name
     , NatToInt (RIndex field (Internal.ToMaybe attributes))
     , field ~ '(name, Maybe x)
     )
  => State (Zipper c attributes root) x
  -> State (Zipper c attributes root) x
memo semantic = (aget @name <$> get) >>= \case
  Nothing ->
    do -- NOTE: The following line is for debugging purposes only and should not
       -- appear in production code
       trace ("computing " <> symbolVal (Proxy @name)) (return ())
       y <- semantic
       modify (aput @name y)
       return y
  (Just y) -> return y

class (a c, b c) => (&&&) (a :: Type -> Constraint) (b :: Type -> Constraint) c
infixl 4 &&&

instance (a c, b c) => (a &&& b) c

checkInvalidMove :: Maybe a -> a
checkInvalidMove (Just x) = x
checkInvalidMove Nothing  = error $! "Invalid move attempted!"

unsafeUp = checkInvalidMove . Internal.up
unsafeDown = checkInvalidMove . Internal.down
unsafeLeft = checkInvalidMove . Internal.left
unsafeRight = checkInvalidMove . Internal.right
unsafeChild i = checkInvalidMove . Internal.child i

data Direction = Left
               | Right
               | Parent
               | Child {-# UNPACK #-}!Int
  deriving (Show)

class WithRollback z d where
  unsafeAt :: Show a => d -> State z a -> State z a

instance WithRollback (Zipper Cxt attributes root) Direction where
  unsafeAt direction action = case direction of
    Left -> go unsafeLeft action unsafeRight
    Right -> go unsafeRight action unsafeLeft
    Parent -> do n <- Internal.whichChild <$> get
                 modify unsafeUp
                 x <- action
                 modify (unsafeChild n)
                 return x
    Child i -> go (unsafeChild i) action unsafeUp
    where
      -- go :: (Zipper Cxt attributes root -> Zipper Cxt attributes root)
      --    -> State (Zipper Cxt attributes root) a
      --    -> (Zipper Cxt attributes root -> Zipper Cxt attributes root)
      --    -> State (Zipper Cxt attributes root) a
      go before now after = do
        modify before
        x <- now
        modify after
        return x


isRoot :: State (Zipper Cxt Attributes Tree) Bool
isRoot = get >>= \case
  (Internal.Z _ _ (_ Internal.:>_)) -> return False
  (Internal.Z _ _ Internal.RootContext)      -> return True


-- User code
--------------------------------------------------------------------------------

-- | A somewhat non-standard binary tree. This is an example of a user-defined
-- data structure.
data Tree = Fork Tree Tree
          | Leaf Int
  deriving (Show, Read, Generic)

type Attributes = '[ '("globmin", Int)
                   , '("replace", Tree)
                   ]

type Cxt = WhereAmI Position &&& Show

-- | Definition of the "globmin" attribute.
--
globmin :: State (Zipper Cxt Attributes Tree) Int
globmin = memo @"globmin" $
  -- NOTE: The following line is for debugging purposes only and should not
  -- appear in production code
  get >>= (\(Zipper h) -> trace ("globmin on " <> show h) (return ())) >>
  isRoot >>= \case
    True  -> gets locmin
    False -> unsafeAt Parent globmin

-- | Definition of the "locmin" attribute.
--
-- @WhereAmI Position@ allows one to pattern match using 'Position' GADT.
locmin :: Zipper Cxt Attributes Tree -> Int
locmin z@(Zipper hole) =
  -- NOTE: The following line is for debugging purposes only and should not
  -- appear in production code
  trace ("locmin on " <> show hole) $
    case whereami hole of
      C_Leaf -> let (Leaf x) = hole in x
      C_Fork -> min (locmin (unsafeChild 0 z)) (locmin (unsafeChild 1 z))

-- | Definition of the "replace" attribute (I'm not sure one can actually call
-- it an attribute though).
replace :: State (Zipper Cxt Attributes Tree) Tree
replace = memo @"replace" $
  get >>= (\(Zipper h) -> trace ("globmin on " <> show h) (return ())) >>
    ( withPosition $ \case
        C_Leaf -> Leaf <$> globmin
        C_Fork -> do x <- unsafeAt (Child 0) replace
                     y <- unsafeAt (Child 1) replace
                     return $! Fork x y
    )

-- Library magic
--------------------------------------------------------------------------------

-- | This should definitely be generated using TH or GHC.Generics. I'd prefer
-- the latter, but I'm not sure it's possible.
instance c Tree => Dissectible c Tree where
  dissect (Fork l r) = Internal.LOne Fork `Internal.LCons` l `Internal.LCons` r
  dissect x          = Internal.LOne x

-- | Represents the position in our forest of data structures.
data Position :: Type -> Type where
  C_Leaf :: Position Tree
  C_Fork :: Position Tree

deriving instance Eq (Position a)
deriving instance Show (Position a)

withPosition ::
  (forall hole. WhereAmI Position hole
     => Position hole -> State (Zipper Cxt attributes root) a)
  -> State (Zipper Cxt attributes root) a
withPosition func = get >>= \(Zipper hole) -> func (whereami hole)

-- | Represents the ability to "look around".
class WhereAmI (p :: Type -> Type) (a :: Type) where
  whereami :: a -> p a

instance WhereAmI Position Tree where
  whereami (Fork _ _) = C_Fork
  whereami (Leaf _)   = C_Leaf

-- An example
--------------------------------------------------------------------------------

t1 :: Tree
t1 = Fork (Fork (Leaf 123)
                (Leaf 0))
          (Leaf 5)

main :: IO ()
main = print $ fst $ runState replace (Internal.enter t1)
  -- print $ fmap showHole $ (Internal.child 0 >=> Internal.right) $ Internal.enter t1
