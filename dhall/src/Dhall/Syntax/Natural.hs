{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This module contains the core syntax types of the builtin Natural type.
-}

module Dhall.Syntax.Natural
    ( NaturalExpr(..)
    , subExpressions
    , reservedIdentifiers
    ) where

import                Control.DeepSeq            (NFData)
import                Data.Bifunctor             (Bifunctor (..))
import                Data.Data                  (Data)
import                Data.HashSet               (HashSet)
import                Data.Text                  (Text)
import {-# SOURCE #-} Dhall.Syntax               (Expr)
import                GHC.Generics               (Generic)
import                Language.Haskell.TH.Syntax (Lift)
import                Numeric.Natural            (Natural)

import qualified Data.HashSet
import qualified Lens.Family  as Lens

{-| Syntax tree for Natural expressions.
 - For the documentation of the type parameters @s@ and @a@ see the `Expr`.
-}
data NaturalExpr s a
    -- | > Natural                                  ~  Natural
    = Natural
    -- | > NaturalLit n                             ~  n
    | NaturalLit Natural
    -- | > NaturalFold                              ~  Natural/fold
    | NaturalFold
    -- | > NaturalBuild                             ~  Natural/build
    | NaturalBuild
    -- | > NaturalIsZero                            ~  Natural/isZero
    | NaturalIsZero
    -- | > NaturalEven                              ~  Natural/even
    | NaturalEven
    -- | > NaturalOdd                               ~  Natural/odd
    | NaturalOdd
    -- | > NaturalToInteger                         ~  Natural/toInteger
    | NaturalToInteger
    -- | > NaturalShow                              ~  Natural/show
    | NaturalShow
    -- | > NaturalSubtract                          ~  Natural/subtract
    | NaturalSubtract
    -- | > NaturalPlus x y                          ~  x + y
    | NaturalPlus (Expr s a) (Expr s a)
    -- | > NaturalTimes x y                         ~  x * y
    | NaturalTimes (Expr s a) (Expr s a)
    deriving (Data, Eq, Foldable, Generic, Lift, NFData, Ord, Show, Traversable)

-- This instance is hand-written due to the fact that deriving
-- it does not give us an INLINABLE pragma. We annotate this fmap
-- implementation with this pragma below to allow GHC to, possibly,
-- inline the implementation for performance improvements.
instance Functor (NaturalExpr s) where
  fmap f = Lens.over subExpressions (fmap f)
  {-# INLINABLE fmap #-}

instance Bifunctor NaturalExpr where
    first k = Lens.over subExpressions (first k)

    second = fmap

subExpressions
    :: Applicative f => (Expr s a -> f (Expr t b)) -> NaturalExpr s a -> f (NaturalExpr t b)
subExpressions _ Natural = pure Natural
subExpressions _ (NaturalLit n) = pure (NaturalLit n)
subExpressions _ NaturalFold = pure NaturalFold
subExpressions _ NaturalBuild = pure NaturalBuild
subExpressions _ NaturalIsZero = pure NaturalIsZero
subExpressions _ NaturalEven = pure NaturalEven
subExpressions _ NaturalOdd = pure NaturalOdd
subExpressions _ NaturalToInteger = pure NaturalToInteger
subExpressions _ NaturalShow = pure NaturalShow
subExpressions _ NaturalSubtract = pure NaturalSubtract
subExpressions f (NaturalPlus a b) = NaturalPlus <$> f a <*> f b
subExpressions f (NaturalTimes a b) = NaturalTimes <$> f a <*> f b
{-# INLINABLE subExpressions #-}

reservedIdentifiers :: HashSet Text
reservedIdentifiers = Data.HashSet.fromList
    [ "Natural/fold"
    , "Natural/build"
    , "Natural/isZero"
    , "Natural/even"
    , "Natural/odd"
    , "Natural/toInteger"
    , "Natural/show"
    , "Natural/subtract"
    , "Natural"
    ]
