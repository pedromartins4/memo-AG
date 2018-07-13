{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-}

-- {-# LANGUAGE DataKinds #-}
module Main where

import Data.Constraint
import GHC.Generics
import Data.Kind

import Data.Proxy
import Unsafe.Coerce
import Data.Coerce

data Tree a
  = Fork { _left :: Tree a
         , _right :: Tree a }
  | Leaf a
  deriving (Show, Read, Generic)


-- instance Dissectible (Tree a) where




data Zipper (c :: Type -> Constraint) (root :: Type) =
  forall hole. (c hole, Recursive c, Dissectible hole) =>
    Zipper hole (Context c hole root)

data HList :: [Type] -> Type where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr 7 `HCons`

-- deriving instance All Show as => Show (HList c as)

{-
data SList :: [k] -> Type where
  SNil  :: SList '[]
  SCons :: SListI xs => SList (x ': xs)

class SListI (xs :: [k]) where
  -- | Get hold of the explicit singleton (that one can then
  -- pattern match on).
  sList :: SList xs

instance SListI '[] where
  sList = SNil

instance SListI xs => SListI (x ': xs) where
  sList = SCons
-}


-- | Transforms a list @[a1, a2, a3]@ and a type @b@ into a function
-- @a1 -> a2 -> a3 -> b@.
type family Unpack2Function (a :: [Type]) b where
  Unpack2Function '[]       b = b
  Unpack2Function (a ': as) b = a -> Unpack2Function as b

-- | Concatenates two type lists.
type family
  AppendList (a :: [Type]) (b :: [Type]) :: [Type] where
    AppendList '[]       b = b
    AppendList (a ': as) b = a ': AppendList as b

type (++) a b = AppendList a b

type family
  AllR (c :: Type -> Constraint) (as :: [Type]) :: Constraint where
    AllR c '[]       = ()
    AllR c (a ': as) = (c a, AllR c as)

type All c as = AllR c as



type family
  ReverseListHelper (acc :: [Type]) (xs :: [Type]) :: [Type] where
    ReverseListHelper acc '[]       = acc
    ReverseListHelper acc (x ': xs) = ReverseListHelper (x ': acc) xs

type ReverseList xs = ReverseListHelper '[] xs

hReverse :: HList a -> HList (ReverseList a)
hReverse = go HNil
  where go :: HList acc -> HList xs -> HList (ReverseListHelper acc xs)
        go acc HNil = acc
        go acc (x `HCons` xs) = go (x `HCons` acc) xs


type family InitR (a :: [Type]) :: [Type] where
  InitR (a ': '[])     = '[]
  InitR (a ': b ': bs) = a ': InitR (b ': bs)

hInit :: HList (a ': as) -> HList (InitR (a ': as))
hInit (x `HCons` HNil) = HNil
hInit (x `HCons` (y `HCons` ys)) = x `HCons` hInit (y `HCons` ys)

initPreservesConstraints :: forall c a as. Proxy c -> Proxy (a ': as) -> (All c as :- All c (InitR as))
initPreservesConstraints _ _ = Sub axiom
  where axiom :: All c as => Dict (All c (InitR as))
        axiom = unsafeCoerce (Dict :: Dict ())

type family LastR (a :: [Type]) :: Type where
  LastR (a ': '[])     = a
  LastR (a ': b ': bs) = LastR (b ': bs)

hLast :: HList (a ': as) -> LastR (a ': as)
hLast = go
  where
    go :: HList (a ': as) -> LastR (a ': as)
    go (x `HCons` HNil) = x
    go (x `HCons` (y `HCons` ys)) = hLast (y `HCons` ys)

lastPreservesConstraints :: forall c a as. Proxy c -> Proxy (a ': as) -> (All c (a ': as) :- c (LastR (a ': as)))
lastPreservesConstraints _ _ = Sub axiom
  where axiom :: All c (a ': as) => Dict (c (LastR (a ': as)))
        axiom = unsafeCoerce (Dict :: Dict ())

type family PushBackR (a :: [k]) (b :: k) :: [k] where
  PushBackR '[]       b = '[b]
  PushBackR (a ': as) b = a ': PushBackR as b

{-
hPushBack :: (c b, All c as) => HList as -> b -> HList (PushBackR as b)
hPushBack = go
  where
    go :: HList as -> b -> HList (PushBackR as b)
    go HNil y = y `HCons` HNil
    go (x `HCons` xs) y = x `HCons` (go xs y)
-}

data LocalContext (lefts :: [Type])
                  (hole :: Type)
                  (rights :: [Type])
                  (parent :: Type) =
    LocalContext
      { _constructor :: !(Unpack2Function (ReverseList lefts) (hole -> Unpack2Function rights parent))
      , _lefts  :: HList lefts
      , _rights :: HList rights
      }

hUncurry :: (Unpack2Function as r) -> HList as -> r
hUncurry = go
  where
    go :: (Unpack2Function as r) -> HList as -> r
    go f HNil = f
    go f (x `HCons` xs) = go (f x) xs

localUp :: hole -> LocalContext lefts hole rights parent -> parent
localUp hole (LocalContext f lefts rights) =
  hUncurry (hUncurry f (hReverse lefts) hole) rights

zUp :: forall c root. Zipper c root -> Maybe (Zipper c root)
zUp (Zipper _ RootContext) = Nothing
zUp (Zipper hole (p :> ctxt)) = Just $ Zipper (localUp hole ctxt) p

localLeft :: forall l ls hole rights parent.
             hole
          -> LocalContext (l ': ls) hole rights parent
          -> (l, LocalContext ls l (hole ': rights) parent)
localLeft hole (LocalContext f (l `HCons` ls) rights) =
  -- TODO: Fix unsafeCoerce here
  (l, LocalContext (unsafeCoerce f) ls (hole `HCons` rights))

zLeft :: forall c root. Zipper c root -> Maybe (Zipper c root)
zLeft (Zipper _ RootContext) = Nothing
zLeft (Zipper hole (p :> ctxt@(LocalContext _ lefts _))) =
  case lefts of
    HNil -> Nothing
    (_ `HCons` _) -> let (hole', ctxt') = localLeft hole ctxt
                      in Just $ Zipper hole' (p :> ctxt')

{-
localDown :: forall c x xs hole.
             (Unpack2Function (x ': xs) hole, HList (x ': xs))
          -> (x, LocalContext xs x '[] hole)
localDown (f, (x `HCons` xs)) p =
  (x, LocalContext f xs HNil)
-}

zDown :: forall c root. Zipper c root -> Maybe (Zipper c root)
zDown (Zipper hole ctxt) =
  case dissect hole of
    (_, HNil) -> Nothing
    (f, (x `HCons` xs)) -> Just $ Zipper x (ctxt :> (LocalContext f HNil xs))
                                    \\     isRecursive @c (toProxy hole)
                                       *** isRecursive @Dissectible (toProxy hole)
  where toProxy :: a -> Proxy a
        toProxy _ = Proxy

{-
localRight :: (c hole, All c lefts, All c rights)
           => hole
           -> LocalContext c lefts hole (r ': rs) parent
           -> (r, LocalContext c (PushBackR lefts hole) r rs parent)
localRight hole (LocalContext f lefts (r `HCons` rs)) =
  let lefts' = hPushBack lefts hole
      hole' = r
      rights' = rs
   in (hole', LocalContext (unsafeCoerce f) lefts' rights')
-}


class Dissectible a where
  type family Children a :: [Type]
  dissect :: a -> (Unpack2Function (Children a) a, HList (Children a))

class Recursive (c :: Type -> Constraint) where
  isRecursive :: forall a proxy. Dissectible a => proxy a -> c a :- All c (Children a)

instance Recursive Dissectible where
  isRecursive = Sub . axiom
    where axiom :: proxy a -> Dict (All Dissectible (Children a))
          axiom = unsafeCoerce (Dict :: Dict ())

data Context (c :: Type -> Constraint) hole root where
  RootContext :: forall c root. (c root, Dissectible root) => Context c root root
  (:>) :: forall lefts rights c parent hole root.
          (All c lefts, All c rights, c hole, c parent,
           All Dissectible lefts, All Dissectible rights, Dissectible hole, Dissectible parent,
           Recursive c)
       => Context c parent root
       -> {-# UNPACK #-} !(LocalContext lefts hole rights parent)
       -> Context c hole root


showLast :: forall a as. All Show (a ': as) => HList (a ': as) -> IO ()
showLast xs = print (hLast xs) \\ lastPreservesConstraints (Proxy :: Proxy Show)
                                                           (Proxy :: Proxy (a ': as))
  where go :: Dict (Show (LastR (a ': as))) -> HList (a ': as) -> IO ()
        go Dict xs = print $ hLast xs

asProxy :: HList a -> Proxy a
asProxy _ = Proxy





main :: IO ()
main = print "Hi"
