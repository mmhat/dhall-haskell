{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This module contains the core syntax types of the builtin List type.
-}

module Dhall.Syntax.List
    ( ListExpr(..)
    , subExpressions
    , reservedIdentifiers
    ) where

import                Control.DeepSeq            (NFData)
import                Data.Bifunctor             (Bifunctor (..))
import                Data.Data                  (Data)
import                Data.HashSet               (HashSet)
import                Data.Sequence              (Seq)
import                Data.Text                  (Text)
import {-# SOURCE #-} Dhall.Syntax               (Expr)
import                GHC.Generics               (Generic)
import                Instances.TH.Lift          ()
import                Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashSet
import qualified Lens.Family  as Lens

{-| Syntax tree for List expressions.
 - For the documentation of the type parameters @s@ and @a@ see the `Expr`.
-}
data ListExpr s a
    -- | > List                                     ~  List
    = List
    -- | > ListLit (Just t ) []                     ~  [] : t
    --   > ListLit  Nothing  [x, y, z]              ~  [x, y, z]
    --
    --   Invariant: A non-empty list literal is always represented as
    --   @ListLit Nothing xs@.
    --
    --   When an annotated, non-empty list literal is parsed, it is represented
    --   as
    --
    --   > Annot (ListLit Nothing [x, y, z]) t      ~ [x, y, z] : t

    -- Eventually we should have separate constructors for empty and non-empty
    -- list literals. For now it's easier to check the invariant in @infer@.
    -- See https://github.com/dhall-lang/dhall-haskell/issues/1359#issuecomment-537087234.
    | ListLit (Maybe (Expr s a)) (Seq (Expr s a))
    -- | > ListAppend x y                           ~  x # y
    | ListAppend (Expr s a) (Expr s a)
    -- | > ListBuild                                ~  List/build
    | ListBuild
    -- | > ListFold                                 ~  List/fold
    | ListFold
    -- | > ListLength                               ~  List/length
    | ListLength
    -- | > ListHead                                 ~  List/head
    | ListHead
    -- | > ListLast                                 ~  List/last
    | ListLast
    -- | > ListIndexed                              ~  List/indexed
    | ListIndexed
    -- | > ListReverse                              ~  List/reverse
    | ListReverse
    deriving (Data, Eq, Foldable, Generic, Lift, NFData, Ord, Show, Traversable)

-- This instance is hand-written due to the fact that deriving
-- it does not give us an INLINABLE pragma. We annotate this fmap
-- implementation with this pragma below to allow GHC to, possibly,
-- inline the implementation for performance improvements.
instance Functor (ListExpr s) where
  fmap f = Lens.over subExpressions (fmap f)
  {-# INLINABLE fmap #-}

instance Bifunctor ListExpr where
    first k = Lens.over subExpressions (first k)

    second = fmap

-- | A traversal over the immediate sub-expressions of an `ListExpr`.
subExpressions
    :: Applicative f => (Expr s a -> f (Expr t b)) -> ListExpr s a -> f (ListExpr t b)
subExpressions _ List = pure List
subExpressions f (ListLit a b) = ListLit <$> traverse f a <*> traverse f b
subExpressions f (ListAppend a b) = ListAppend <$> f a <*> f b
subExpressions _ ListBuild = pure ListBuild
subExpressions _ ListFold = pure ListFold
subExpressions _ ListLength = pure ListLength
subExpressions _ ListHead = pure ListHead
subExpressions _ ListLast = pure ListLast
subExpressions _ ListIndexed = pure ListIndexed
subExpressions _ ListReverse = pure ListReverse
{-# INLINABLE subExpressions #-}

-- | Reserved identifiers of the List builtin.
reservedIdentifiers :: HashSet Text
reservedIdentifiers = Data.HashSet.fromList
    [ "List/build"
    , "List/fold"
    , "List/length"
    , "List/head"
    , "List/last"
    , "List/indexed"
    , "List/reverse"
    , "List"
    ]
