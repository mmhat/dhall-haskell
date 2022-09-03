{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dhall.Syntax.Bool
    ( BoolExpr(..)
    , subExpressions
    , reservedIdentifiers
    ) where

import                Control.DeepSeq            (NFData)
import                Data.Bifunctor             (Bifunctor (..))
import                Data.Data                  (Data)
import                Data.HashSet               (HashSet)
import                Data.Text                  (Text)
import {-# SOURCE #-} Dhall.Syntax                  (Expr)
import                GHC.Generics               (Generic)
import                Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashSet
import qualified Lens.Family          as Lens

data BoolExpr s a
    -- | > Bool                                     ~  Bool
    = Bool
    -- | > BoolLit b                                ~  b
    | BoolLit Bool
    -- | > BoolAnd x y                              ~  x && y
    | BoolAnd (Expr s a) (Expr s a)
    -- | > BoolOr  x y                              ~  x || y
    | BoolOr  (Expr s a) (Expr s a)
    -- | > BoolEQ  x y                              ~  x == y
    | BoolEQ  (Expr s a) (Expr s a)
    -- | > BoolNE  x y                              ~  x != y
    | BoolNE  (Expr s a) (Expr s a)
    -- | > BoolIf x y z                             ~  if x then y else z
    | BoolIf (Expr s a) (Expr s a) (Expr s a)
    deriving (Data, Eq, Foldable, Generic, Lift, NFData, Ord, Show, Traversable)

-- This instance is hand-written due to the fact that deriving
-- it does not give us an INLINABLE pragma. We annotate this fmap
-- implementation with this pragma below to allow GHC to, possibly,
-- inline the implementation for performance improvements.
instance Functor (BoolExpr s) where
  fmap f = Lens.over subExpressions (fmap f)
  {-# INLINABLE fmap #-}

instance Bifunctor BoolExpr where
    first k = Lens.over subExpressions (first k)

    second = fmap

subExpressions
    :: Applicative f => (Expr s a -> f (Expr t b)) -> BoolExpr s a -> f (BoolExpr t b)
subExpressions _ Bool = pure Bool
subExpressions _ (BoolLit b) = pure (BoolLit b)
subExpressions f (BoolAnd a b) = BoolAnd <$> f a <*> f b
subExpressions f (BoolOr a b) = BoolOr <$> f a <*> f b
subExpressions f (BoolEQ a b) = BoolEQ <$> f a <*> f b
subExpressions f (BoolNE a b) = BoolNE <$> f a <*> f b
subExpressions f (BoolIf a b c) = BoolIf <$> f a <*> f b <*> f c
{-# INLINABLE subExpressions #-}

-- | The set of reserved identifiers for the Dhall language
-- | Contains also all keywords from "reservedKeywords"
reservedIdentifiers :: HashSet Text
reservedIdentifiers = Data.HashSet.fromList
    [ "Bool"
    , "True"
    , "False"
    ]
