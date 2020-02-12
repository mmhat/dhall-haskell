{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Dhall.TypeLevel
    ( type (||)
    , type (&&)
    , Not
    , Id
    , Contains
    , Inhabits
    , Recursive
    ) where

import Data.Functor.Identity (Identity)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Generics

{- | Type-level OR
-}
type family (a :: Bool) || (b :: Bool) :: Bool where
    a      || a      = a
    'False || b      = b
    'True  || _      = 'True
    a      || 'False = a
    _      || 'True  = 'True

{- | Type-level AND
-}
type family (a :: Bool) && (b :: Bool) :: Bool where
    a      && a      = a
    'False && _      = 'False
    'True  && b      = b
    _      && 'False = 'False
    a      && 'True  = a

{- | Type-level NOT
-}
type family Not (a :: Bool) :: Bool where
    Not 'False = 'True
    Not 'True  = 'False

{- | Type identity
-}
type family Id (a :: *) (b :: *) :: Bool where
    Id a a = 'True
    Id _ _ = 'False

{- | Check if type `b` occurs in type `a`, e.g. `a` occurs in `Foo a`.
-}
type family Contains (a :: k) (b :: *) :: Bool where
    Contains (f x) y = Id x y || Contains f y
    Contains _ _     = 'False

{- | A type family to extract the types contained in a type definition.
     `Inhabits a b` means that type `a` occurs in the the type definition of `b`.
-}
type family Inhabits (a :: *) (b :: k) :: Bool where
    Inhabits a     (f :+: g)  = Inhabits a f || Inhabits a g
    Inhabits a     (f :*: g)  = Inhabits a f || Inhabits a g
    Inhabits a     (M1 i c f) = Inhabits a f
    Inhabits a     (K1 R b)   = (Id a b || Contains b a) && Not (Contains a b)
    Inhabits _ _              = 'False

{- | A type family that unifies with `True` for recursive types.
-}
type family Recursive (a :: *) :: Bool where
    Recursive a = Inhabits a (Rep a)



{- Some tests.
-}

id1 :: Id Bool Bool :~: 'True
id1 = Refl

id2 :: Id Bool () :~: 'False
id2 = Refl

id3 :: Id a a :~: 'True
id3 = Refl

id4 :: Id a b :~: Id a b
id4 = Refl

id5 :: Id (a, b) (a, b) :~: 'True
id5 = Refl

id6 :: Id (a, b) (a, c) :~: Id (a, b) (a, c)
id6 = Refl

id7 :: Id (a, b) a && Not (Contains (a, b) a) :~: 'False
id7 = Refl

id8 :: Id (a, b) b && Not (Contains (a, b) b) :~: 'False
id8 = Refl

contains1 :: Contains Bool Bool :~: 'False
contains1 = Refl

contains2 :: Contains [Bool] Bool :~: 'True
contains2 = Refl

contains3 :: Contains [Bool] () :~: 'False
contains3 = Refl

contains4 :: Contains (a, b) a :~: 'True
contains4 = Refl

contains5 :: Contains (a, b) b :~: 'True
contains5 = Refl

contains6 :: Contains (a, b) (c, d) :~: Id b (c, d) || Id a (c, d)
contains6 = Refl

data NoCons
    deriving Generic

data SingletonCons = SingletonCons
    deriving Generic

data Monomorphic = Monomorphic Int
    deriving Generic

data Polymorphic a = Polymorphic a
    deriving Generic

data MonoRec = MonoRec MonoRec
    deriving Generic

data SumConsL = SumConsL SumConsL | SumNilL
    deriving Generic

data SumConsR = SumNilR | SumConsR SumConsR
    deriving Generic

data PolyRec a = PolyRec (PolyRec a)
    deriving Generic

data PolyRec2 a = PolyRec2 (Identity (PolyRec2 a))
    deriving Generic

recursive1 :: Recursive NoCons :~: 'False
recursive1 = Refl

recursive2 :: Recursive SingletonCons :~: 'False
recursive2 = Refl

recursive3 :: Recursive Monomorphic :~: 'False
recursive3 = Refl

recursive4 :: Recursive (Polymorphic Bool) :~: 'False
recursive4 = Refl

recursive5 :: Recursive (Polymorphic a) :~: 'False
recursive5 = Refl

recursive6 :: Recursive MonoRec :~: 'True
recursive6 = Refl

recursive7 :: Recursive SumConsL :~: 'True
recursive7 = Refl

recursive8 :: Recursive SumConsR :~: 'True
recursive8 = Refl

recursive9 :: Recursive (PolyRec Bool) :~: 'True
recursive9 = Refl

--recursive10 :: Recursive (PolyRec a) :~: 'True
--recursive10 = Refl

recursive11 :: Recursive (PolyRec2 Bool) :~: 'True
recursive11 = Refl

--recursive12 :: Recursive (PolyRec2 a) :~: 'True
--recursive12 = Refl
