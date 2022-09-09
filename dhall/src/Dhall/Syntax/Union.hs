{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dhall.Syntax.Union
    ( UnionExpr(..)
    , subExpressions
    , reservedIdentifiers
    ) where

import                Control.DeepSeq            (NFData)
import                Data.Bifunctor             (Bifunctor (..))
import                Data.Data                  (Data)
import                Data.HashSet               (HashSet)
import                Data.Text                  (Text)
import                Dhall.Map                  (Map)
import {-# SOURCE #-} Dhall.Syntax               (Expr)
import                GHC.Generics               (Generic)
import                Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashSet
import qualified Lens.Family          as Lens

data UnionExpr s a
    -- | > Optional                                 ~  Optional
    = Optional
    -- | > Some e                                   ~  Some e
    | Some (Expr s a)
    -- | > None                                     ~  None
    | None
    -- | > Union        [(k1, Just t1), (k2, Nothing)] ~  < k1 : t1 | k2 >
    | Union     (Map Text (Maybe (Expr s a)))
    -- | > Merge x y (Just t )                      ~  merge x y : t
    --   > Merge x y  Nothing                       ~  merge x y
    | Merge (Expr s a) (Expr s a) (Maybe (Expr s a))
    -- | > ShowConstructor x                        ~  showConstructor x
    | ShowConstructor (Expr s a)
    deriving (Data, Eq, Foldable, Generic, Lift, NFData, Ord, Show, Traversable)

-- This instance is hand-written due to the fact that deriving
-- it does not give us an INLINABLE pragma. We annotate this fmap
-- implementation with this pragma below to allow GHC to, possibly,
-- inline the implementation for performance improvements.
instance Functor (UnionExpr s) where
  fmap f = Lens.over subExpressions (fmap f)
  {-# INLINABLE fmap #-}

instance Bifunctor UnionExpr where
    first k = Lens.over subExpressions (first k)

    second = fmap

-- | A traversal over the immediate sub-expressions of an expression.
subExpressions
    :: Applicative f => (Expr s a -> f (Expr t b)) -> UnionExpr s a -> f (UnionExpr t b)
subExpressions _ Optional = pure Optional
subExpressions f (Some a) = Some <$> f a
subExpressions _ None = pure None
subExpressions f (Union a) = Union <$> traverse (traverse f) a
subExpressions f (Merge a b t) = Merge <$> f a <*> f b <*> traverse f t
subExpressions f (ShowConstructor a) = ShowConstructor <$> f a
{-# INLINABLE subExpressions #-}

reservedIdentifiers :: HashSet Text
reservedIdentifiers = Data.HashSet.fromList
    [ "Optional"
    , "None"
    , "Some"
    ]
