{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Main
-- Description : Repmin example
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : BSD3
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Main where

import           Prelude                 hiding ( Either(..) )

import           Control.Monad.Trans.State.Strict
import           Data.Constraint
import           Data.Default.Class
import           Data.Kind
import           Debug.Trace
import           GHC.Generics
import           Data.Vinyl                     ( ElField(..)
                                                , Rec((:&))
                                                )
import qualified Data.Vinyl                    as Vinyl

import Zipper

class (a c, b c) => (&&&) (a :: Type -> Constraint) (b :: Type -> Constraint) c
infixl 4 &&&

instance (a c, b c) => (a &&& b) c

-- User code
--------------------------------------------------------------------------------

-- | A somewhat non-standard binary tree. This is an example of a user-defined
-- data structure.
data Tree = Fork Tree Tree
          | Leaf Int
  deriving (Show, Read, Generic)

-- | Specifies which attributes to cache
type Attributes = '[ '("globmin", Int)
                   , '("replace", Tree)
                   ]

-- | Specifies additional constraints on values in the zipper
type Cxt = WhereAmI Position &&& Show

-- | Definition of the "globmin" attribute.
globmin :: State (Zipper Cxt Attributes Tree) Int
globmin = memoize @"globmin" $
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
replace = memoize @"replace" $
  get >>= (\(Zipper h) -> trace ("globmin on " <> show h) (return ())) >>
    ( withPosition $ \case
        C_Leaf -> Leaf <$> globmin
        C_Fork -> do x <- unsafeAt (Child 0) replace
                     y <- unsafeAt (Child 1) replace
                     return $! Fork x y
    )

-- Library magic
--------------------------------------------------------------------------------

instance Default (Memo Attributes) where
  def = M { _mCurrent = Vinyl.toARec $ Field Nothing :& Field Nothing :& Vinyl.RNil
          , _mLeft = def
          , _mRight = def
          , _mUp = def
          , _mDown = def
          }

-- | This should definitely be generated using TH or GHC.Generics. I'd prefer
-- the latter, but I'm not sure it's possible.
instance c Tree => Dissectible c Tree where
  dissect (Fork l r) = LOne Fork `LCons` l `LCons` r
  dissect x          = LOne x

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
main = print $ fst $ runState replace (enter t1)
  -- print $ fmap showHole $ (child 0 >=> right) $ enter t1
