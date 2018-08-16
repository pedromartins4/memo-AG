-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Zipper
-- Description : A generic zipper for embedding attribute grammars.
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : GPL-3
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Zipper
  ( -- ** Motivation
    -- $motivation

    -- ** The Zipper
    Zipper(..)
  , pattern Zipper
  , enter
  , leave
  , up
  , down
  , left
  , right
  , child
  , whichChild
  , isRoot
  , unsafeUp
  , unsafeDown
  , unsafeLeft
  , unsafeRight
  , unsafeChild
  , Direction(..)
  , unsafeAt
    -- * Working with the Memo table
  , Memo(..)
  , memoize
    -- * Implementation details
  , Context(..)
  , LocalContext(..)
  , Left(..)
  , Right(..)
  , Dissectible(..)
  , ToMaybe
  , aget
  , aput
  ) where

import           Prelude                 hiding ( Either(..) )
import           Control.DeepSeq
import           Control.Monad                  ( (>=>) )
import           Control.Monad.Trans.State.Strict
import           Data.Default.Class
import           Data.Constraint                ( Constraint )
import           Data.Kind
import           Data.Proxy
import           Debug.Trace
import           GHC.Generics                   ( Generic )
import           GHC.Stack
import           GHC.TypeLits

import           Data.Vinyl              hiding ( RNil )
import qualified Data.Vinyl.ARec               as Vinyl
import           Data.Vinyl.TypeLevel           ( RIndex
                                                , NatToInt
                                                )
-- import           Data.Vinyl.Functor             ( Thunk )





-- $motivation
--
-- Let's review the current imlementation of the Desk language example. First of
-- all, we construct some new data structure @MemoTree@ which requires a full
-- traversal of our "forest" of data structures starting in @Program@. On one
-- hand, @buildMemoTree@ should be lazy such that the conversion is done on
-- demand. On the other hand, however, all the strictness information from the
-- user-defined data structures is lost!
--
-- Next, we construct a zipper from @MemoTree@ and compute the @code@ attribute
-- on it. The definition of the @code@ function is pretty neat and quite closely
-- follows the AG. However, its type @Dir -> MemoAG a@ removes a lot of
-- expressive power zippers have. What if I want to get the code attribute of
-- the second child of the left sibling of the third child? I could do it with
-- zippers as @child 3 >>> left >>> child 2@. With memoisation added, we can't
-- make arbitrary movements anymore.
--
-- My next concern is the @lexeme@ function. Seeing as it works explicitly with
-- @MemoTree@ it's reasonable to assume that it should be generated
-- automatically. But what's it's type signature in the general case? Or at
-- least the semantics? Say, we stored numbers as 'Integer's rather than
-- 'String's. Now the lexeme function doesn't work anymore. In other words,
-- current approach lacks a general way of extracting the user-defined data and
-- performing some operations on it.
--
-- Now, the conjecture is that modifying a zipper to traverse 2 data structures
-- simultaneously, we can get rid of all the problems described above :)


-- | The AG Zipper.
--
-- It consists of two zippers
--
--   (1) A zipper into the forest of user-defined data structures.
--   (2) A zipper into an infinite graph which is used to store computed
--       attributes.
--
-- Implementation of the first zipper is very similar to the one described in
-- ["Scrap Your Zippers: A Generic Zipper for Heterogeneous
-- Types"](https://michaeldadams.org/papers/scrap_your_zippers/). The only
-- difference is that we rely on the 'Dissectible' type class instead of 'Data'
-- which allows us to propagate down arbitrary user-defined constraints @c@.
-- For example, if our data structure is 'Show'able:
--
-- >>> data Tree = Fork Tree Tree | Leaf Int deriving (Show)
-- >>> deriveDissectible ''Tree -- (not yet implemented)
-- >>> let tree = Fork (Fork (Leaf 123) (Leaf 0)) (Leaf 5)
-- >>> type TreeZ = Zipper Show '[] Tree
-- >>> let showHole :: TreeZ -> String
-- >>>     showHole (Zipper h) = show h
-- >>> print $ showHole <$> (down >=> left >=> down >=> left >=> right) (enter tree :: TreeZ)
-- Just "Leaf 0"
--
-- I don't think there is a clean way to achieve something like this using the
-- generic zipper from the Scrap Your Zippers paper.
--
-- Implementation of the second zipper is quite straightforward: have a look at
-- the 'Memo' data structure for details. The important part is that
-- @attributes@ type allows us to specify which attributes to memoize. Each
-- element in @attributes@ is a promoted tuple of a 'Symbol' describing the name
-- and a 'Type' indicating the type of the attribute.
--
-- Finally, @root@ indicates the root of the tree we want to traverse.
data Zipper (c :: Type -> Constraint) (attributes :: [(Symbol, Type)]) (root :: Type) =
  forall hole. (c hole, Dissectible c hole) =>
    Z { _zHole :: !hole                  -- ^ Value under cursor
      , _zMemo :: !(Memo attributes)     -- ^ Attributes cache
      , _zCxt  :: !(Context c hole root) -- ^ Surrounding context.
      }

-- | You, as an end-user, don't care about the attribute cache and context. This
-- pattern synonym hides these details while still providing access to the hole.
pattern Zipper :: () => (c hole, Dissectible c hole) => hole -> Zipper c attributes root
pattern Zipper h <- (Z h _ _)
{-# COMPLETE Zipper #-}

-- | Given a list associating names of attributes with their types,
-- returns a list associating names of attributes with 'Maybe' their types to
-- account for the fact that some attributes may not yet be known.
--
-- For example:
--
-- >>> :kind! ToMaybe '[ '("foo", Int), '("bar", String)]
-- ToMaybe '[ '("foo", Int), '("bar", String)] :: [(Symbol, *)]
-- = '['("foo", Maybe Int), '("bar", Maybe [Char])]
type family ToMaybe (xs :: [(Symbol, Type)]) where
  ToMaybe '[] = '[]
  ToMaybe ( '(s,t) ': xs ) = '(s, Maybe t) ': (ToMaybe xs)

-- | Attributes cache is an infinite grid of cells where each cell is an
-- extensible record. 'Memo' is a zipper into such a grid.
--
-- /Note/ how all pointers to neighbours are lazy!
data Memo (attributes :: [(Symbol, Type)]) =
  M { _mCurrent :: {-# UNPACK #-}!(AFieldRec (ToMaybe attributes)) -- ^ Cached attributes
    , _mLeft    :: Memo attributes
    , _mRight   :: Memo attributes
    , _mUp      :: Memo attributes
    , _mDown    :: Memo attributes
    }

-- | Returns the specified attribute.
--
-- /Note/ compared to @lookupAttr@ in the original approach, this function is
-- general and requires no TH magic!
aget
  :: forall name field x c attributes root.
     ( KnownSymbol name
     , NatToInt (RIndex field (ToMaybe attributes))
     , field ~ '(name, Maybe x)
     )
  => Zipper c attributes root
  -> Maybe x
aget (Z _ m _) = case Vinyl.aget @field (_mCurrent m) of (Field x) -> x

-- | Updates the specified attribute.
--
-- /Note/ compared to @updateAttr@ in the original approach, this function is
-- general and requires no TH magic!
aput
  :: forall name field x c attributes root.
     ( KnownSymbol name
     , NatToInt (RIndex field (ToMaybe attributes))
     , field ~ '(name, Maybe x)
     )
  => x
  -> Zipper c attributes root
  -> Zipper c attributes root
aput x z@(Z _ m _) = let f = _mCurrent m
                         f' = Vinyl.aput @field (Field (Just x)) f
                      in z { _zMemo = m { _mCurrent = f' } }

-- | Given the @name@ of an attribute and a monadic computation to calculate the
-- attribute, returns a monadic computation of the same type which will use
-- caching to speed-up the calculation.
memoize
  :: forall name field attributes x c root m.
     ( KnownSymbol name
     , NatToInt (RIndex field (ToMaybe attributes))
     , field ~ '(name, Maybe x)
     , Monad m
     )
  => StateT (Zipper c attributes root) m x
  -> StateT (Zipper c attributes root) m x
memoize semantic = (aget @name <$> get) >>= \case
  Nothing -> do
    -- NOTE: The following line is for debugging purposes only and should not
    -- appear in production code
    trace ("computing " <> symbolVal (Proxy @name)) (return ())
    y <- semantic
    modify (aput @name y)
    return y
  (Just y) -> return y

-- | Specifies the position, where to evaluate the attribute. This is an
-- extension of the original @Dir@ data type.
--
-- /Note:/ using "Left" and "Right" is probably not a great idea. Better name
-- suggestions are very welcome.
data Direction = Left
               | Right
               | Parent
               | Child {-# UNPACK #-}!Int
               | Direction :~> Direction
  deriving (Read, Show, Generic, NFData)

-- | Moves the zipper in the given direction, runs the monadic action and then
-- moves back.
--
-- /Note:/ this function is unsafe because it requires Direction to be a valid
-- movement.
unsafeAt
  :: (HasCallStack, Monad m)
  => Direction
  -> StateT (Zipper c as root) m a
  -> StateT (Zipper c as root) m a
unsafeAt d action = do
  !(forth, back) <- mkMovements d <$> get
  modify forth
  x <- action
  modify back
  return x
  where
    mkMovements :: (z ~ Zipper c as root) => Direction -> z -> (z -> z, z -> z)
    mkMovements !Left        _ = (unsafeLeft, unsafeRight)
    mkMovements !Right       _ = (unsafeRight, unsafeLeft)
    mkMovements !Parent (whichChild -> !n) = (unsafeUp, unsafeChild n)
    mkMovements !(Child i)   _ = (unsafeChild i, unsafeUp)
    mkMovements !(d1 :~> d2) z = let !(f1, b1) = mkMovements d1 z
                                     !(f2, b2) = mkMovements d2 (f1 z)
                                  in (f2 . f1, b1 . b2)

-- | A context consists of a 'LocalContext' and a path to the @root@ of the
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

-- | A class of types which can can be dissected.
class Dissectible (c :: Type -> Constraint) (a :: Type) where
  dissect :: a -> Left c a

-- | Returns whether you're at the root of the zipper. This eliminates the need
-- to define a special @Root@ data structure for each AG which preserving the
-- expressive power.
isRoot :: Monad m => StateT (Zipper c attributes root) m Bool
isRoot = get >>= \case
  (Z _ _ (_:>_))      -> return False
  (Z _ _ RootContext) -> return True

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

-- | Returns the child at the specified index. __Counting starts from 0!__
child :: Int -> Zipper c as root -> Maybe (Zipper c as root)
child !n !z | n < 0 = error $! "Expected a natural number, but got: " <> show n
            | otherwise = toNth 0 =<< (theLeftOne <$> down z)
 where
   theLeftOne !x = case left x of
     Nothing -> x
     Just x' -> theLeftOne x'
   toNth !i | i == n    = return
            | otherwise = right >=> toNth (i + 1)

-- | The following relation holds: @Just x = up x >>= child (whichChild x)@
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

checkInvalidMove :: HasCallStack => Maybe a -> a
checkInvalidMove (Just x) = x
checkInvalidMove Nothing  = error $! "Invalid move attempted!"

unsafeUp :: HasCallStack => Zipper c as root -> Zipper c as root
unsafeUp = checkInvalidMove . up

unsafeDown :: HasCallStack => Zipper c as root -> Zipper c as root
unsafeDown = checkInvalidMove . down

unsafeLeft :: HasCallStack => Zipper c as root -> Zipper c as root
unsafeLeft = checkInvalidMove . left

unsafeRight :: HasCallStack => Zipper c as root -> Zipper c as root
unsafeRight = checkInvalidMove . right

unsafeChild :: HasCallStack => Int -> Zipper c as root -> Zipper c as root
unsafeChild !i = checkInvalidMove . child i


