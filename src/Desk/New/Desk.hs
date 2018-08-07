{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Desk.New.Desk
-- Description : New implementation of the Desk example
-- Copyright   : (c) Tom Westerhout, 2018
-- License     : BSD3
-- Maintainer  : t.westerhout@student.ru.nl
-- Stability   : experimental
module Desk.New.Desk
  ( main
  ) where


import           Prelude                 hiding ( lookup, Either(..) )

import           Control.Monad.Trans.State.Strict
import           Data.Default.Class
import           Data.Kind
import           Data.Vinyl                     ( ElField(..)
                                                , Rec((:&))
                                                )
import qualified Data.Vinyl                    as Vinyl

import           Desk
import           Zipper

{-
class (a c, b c) => (&&&) (a :: Type -> Constraint) (b :: Type -> Constraint) c
infixl 4 &&&

instance (a c, b c) => (a &&& b) c
-}

-- User code
-- -----------------------------------------------------------------------------


type Attributes = '[ '("envi", SymbolTable)
                   , '("envs", SymbolTable)
                   , '("name", String)
                   , '("value", String)
                   , '("ok", Bool)
                   , '("code", String)
                   ]

type Cxt = WhereAmI C_Position
type AGZipper = State (Zipper Cxt Attributes Program)

envi :: AGZipper SymbolTable
envi = memoize @"envi" $ get >>= \(Zipper hole) -> case whereami hole of
  C_PRINT -> unsafeAt (Child 1) envs
  _       -> unsafeAt Parent envi

name :: AGZipper String
name = memoize @"name" $ get >>= \(Zipper hole) -> case whereami hole of
  C_Id    -> let (Id x) = hole in return x
  C_Equal -> unsafeAt (Child 0) name
  _       -> error $! "'name' attribute is not defined for " <> show
    (whereami @C_Position hole)

value :: AGZipper String
value = memoize @"value" $ get >>= \(Zipper hole) -> case whereami hole of
  C_Name   -> lookup <$> (unsafeAt (Child 0) name) <*> envi
  C_Number -> let (Number x) = hole in return x
  C_Equal  -> let (Equal _ x) = hole in return x
  _        -> error $! "'value' attribute is not defined for " <> show
    (whereami @C_Position hole)

envs :: AGZipper SymbolTable
envs = memoize @"envs" $ get >>= \(Zipper hole) -> case whereami hole of
  C_EmptyConstPart -> return []
  C_WHERE          -> unsafeAt (Child 0) envs
  C_Comma          -> do
    xs <- unsafeAt (Child 0) envs
    x  <- unsafeAt (Child 1) ((,) <$> name <*> value)
    return $ xs ++ [x]
  C_Def -> (: []) <$> unsafeAt (Child 0) ((,) <$> name <*> value)
  _     -> error $! "'envs' attribute is not defined for " <> show
    (whereami @C_Position hole)

ok :: AGZipper Bool
ok = memoize @"ok" $ get >>= \(Zipper hole) -> case whereami hole of
  C_Name           -> isInSymbolTable <$> (unsafeAt (Child 0) name) <*> envi
  C_Number         -> return True
  C_EmptyConstPart -> return True
  C_WHERE          -> unsafeAt (Child 0) ok
  C_Comma          -> do
    b  <- unsafeAt (Child 0) ok
    s  <- unsafeAt (Child 1) name
    st <- unsafeAt (Child 0) envs
    return $ b && not (isInSymbolTable s st)
  C_Def -> return True
  _     -> error $! "'ok' attribute is not defined for " <> show
    (whereami @C_Position hole)

code :: AGZipper String
code = memoize @"code" $ get >>= \(Zipper hole) -> case whereami hole of
  C_PRINT -> do
    unsafeAt (Child 1) ok >>= \b -> if b
      then concat <$> sequence
        [unsafeAt (Child 0) code, return "PRINT, 0\n", return "HALT,  0\n"]
      else return "HALT,  0\n"
  C_Add -> do
    unsafeAt (Child 1) ok >>= \b -> if b
      then concat <$> sequence
        [ unsafeAt (Child 0) code
        , return "ADD,   "
        , unsafeAt (Child 1) value
        , return "\n"
        ]
      else return $ "HALT,  0\n"
  C_Fact -> do
    unsafeAt (Child 0) ok >>= \b -> if b
      then concat
        <$> sequence [return "LOAD,  ", unsafeAt (Child 0) value, return "\n"]
      else return $ "HALT,  0\n"
  _     -> error $! "'code' attribute is not defined for " <> show
    (whereami @C_Position hole)

main :: IO ()
main = putStrLn $ fst $ runState code (enter testData)

-- Library magic
-- -----------------------------------------------------------------------------

instance Default (Memo Attributes) where
  def = M { _mCurrent = Vinyl.toARec $ Field Nothing
                                    :& Field Nothing
                                    :& Field Nothing
                                    :& Field Nothing
                                    :& Field Nothing
                                    :& Field Nothing
                                    :& Vinyl.RNil
          , _mLeft = def
          , _mRight = def
          , _mUp = def
          , _mDown = def
          }

instance ( c Expression
         , c ConstPart
         , c Factor
         , c ConstDefList
         , c ConstDef
         , c ConstName
         , c String
         ) =>
         Dissectible c Program where
  dissect (PRINT x y) = LOne PRINT `LCons` x `LCons` y

instance (c Expression, c Factor, c ConstName, c String) =>
         Dissectible c Expression where
  dissect (Add x y) = LOne Add `LCons` x `LCons` y
  dissect (Fact x) = LOne Fact `LCons` x

instance (c ConstName, c String) => Dissectible c Factor where
  dissect (Name x) = LOne Name `LCons` x
  dissect x@(Number _) = LOne x

instance Dissectible c ConstName where
  dissect x = LOne x

instance (c ConstPart, c ConstDefList, c ConstDef, c ConstName, c String) =>
         Dissectible c ConstPart where
  dissect EmptyConstPart = LOne EmptyConstPart
  dissect (WHERE x) = LOne WHERE `LCons` x

instance (c ConstDefList, c ConstDef, c ConstName, c String) =>
         Dissectible c ConstDefList where
  dissect (Comma xs x) = LOne Comma `LCons` xs `LCons` x
  dissect (Def x) = LOne Def `LCons` x

instance (c ConstName, c String) => Dissectible c ConstDef where
  dissect (Equal x y) = LOne Equal `LCons` x `LCons` y

instance Dissectible c String where
  dissect x = LOne x

-- | Represents the ability to "look around".
class WhereAmI (p :: Type -> Type) (a :: Type) where
  whereami :: a -> p a

{-
data T_Position :: Type -> Type where
  T_Program      :: T_Position Program
  T_Expression   :: T_Position Expression
  T_Factor       :: T_Position Factor
  T_ConstName    :: T_Position ConstName
  T_ConstPart    :: T_Position ConstPart
  T_ConstDefList :: T_Position ConstDefList
  T_ConstDef     :: T_Position ConstDef
  T_String       :: T_Position String
-}

data C_Position :: Type -> Type where
  C_PRINT          :: C_Position Program
  C_Add            :: C_Position Expression
  C_Fact           :: C_Position Expression
  C_Name           :: C_Position Factor
  C_Number         :: C_Position Factor
  C_Id             :: C_Position ConstName
  C_EmptyConstPart :: C_Position ConstPart
  C_WHERE          :: C_Position ConstPart
  C_Comma          :: C_Position ConstDefList
  C_Def            :: C_Position ConstDefList
  C_Equal          :: C_Position ConstDef
  C_String         :: C_Position String

deriving instance Show (C_Position a)

{-
instance WhereAmI T_Position Program      where whereami _ = T_Program
instance WhereAmI T_Position Expression   where whereami _ = T_Expression
instance WhereAmI T_Position Factor       where whereami _ = T_Factor
instance WhereAmI T_Position ConstName    where whereami _ = T_ConstName
instance WhereAmI T_Position ConstPart    where whereami _ = T_ConstPart
instance WhereAmI T_Position ConstDefList where whereami _ = T_ConstDefList
instance WhereAmI T_Position ConstDef     where whereami _ = T_ConstDef
instance WhereAmI T_Position String       where whereami _ = T_String
-}

instance WhereAmI C_Position Program where whereami _ = C_PRINT
instance WhereAmI C_Position Expression where
  whereami (Add _ _) = C_Add
  whereami (Fact _)  = C_Fact
instance WhereAmI C_Position Factor where
  whereami (Name _)  = C_Name
  whereami (Number _)  = C_Number
instance WhereAmI C_Position ConstName where whereami _ = C_Id
instance WhereAmI C_Position ConstPart where
  whereami EmptyConstPart = C_EmptyConstPart
  whereami (WHERE _)      = C_WHERE
instance WhereAmI C_Position ConstDefList where
  whereami (Comma _ _)  = C_Comma
  whereami (Def _)      = C_Def
instance WhereAmI C_Position ConstDef where whereami _ = C_Equal
instance WhereAmI C_Position String where whereami _ = C_String

