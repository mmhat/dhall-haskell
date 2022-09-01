{-# LANGUAGE PatternSynonyms #-}

{-| This module contains pattern synonyms useful to maintain some backwards
 - compatibilty.
-}

module Dhall.Syntax.Patterns (
    Expr
        ( List
        , ListLit
        , ListAppend
        , ListBuild
        , ListFold
        , ListLength
        , ListHead
        , ListLast
        , ListIndexed
        , ListReverse
        , Text
        , TextLit
        , TextAppend
        , TextReplace
        , TextShow
        )
    ) where

import Data.Sequence     (Seq)
import Dhall.Syntax      (Expr (..))
import Dhall.Syntax.Text (Chunks)

import qualified Dhall.Syntax.List
import qualified Dhall.Syntax.Text

--------------------------------------------------------------------------------
-- List builtin
--------------------------------------------------------------------------------

-- | > List  ~  List
pattern List :: Expr s a
pattern List = ListExpr Dhall.Syntax.List.List

-- | > ListLit (Just t ) []         ~  [] : t
--   > ListLit  Nothing  [x, y, z]  ~  [x, y, z]
--
--   Invariant: A non-empty list literal is always represented as
--   @ListLit Nothing xs@.
--
--   When an annotated, non-empty list literal is parsed, it is represented
--   as
--
--   > Annot (ListLit Nothing [x, y, z]) t  ~  [x, y, z] : t

-- Eventually we should have separate constructors for empty and non-empty
-- list literals. For now it's easier to check the invariant in @infer@.
-- See https://github.com/dhall-lang/dhall-haskell/issues/1359#issuecomment-537087234.
pattern ListLit :: Maybe (Expr s a) -> Seq (Expr s a) -> Expr s a
pattern ListLit mtype list = ListExpr (Dhall.Syntax.List.ListLit mtype list)

-- | > ListAppend x y  ~  x # y
pattern ListAppend :: Expr s a -> Expr s a -> Expr s a
pattern ListAppend list1 list2 = ListExpr (Dhall.Syntax.List.ListAppend list1 list2)

-- | > ListBuild  ~  List/build
pattern ListBuild :: Expr s a
pattern ListBuild = ListExpr Dhall.Syntax.List.ListBuild

-- | > ListFold  ~  List/fold
pattern ListFold :: Expr s a
pattern ListFold = ListExpr Dhall.Syntax.List.ListFold

-- | > ListLength  ~  List/length
pattern ListLength :: Expr s a
pattern ListLength = ListExpr Dhall.Syntax.List.ListLength

-- | > ListHead  ~  List/head
pattern ListHead :: Expr s a
pattern ListHead = ListExpr Dhall.Syntax.List.ListHead

-- | > ListLast  ~  List/last
pattern ListLast :: Expr s a
pattern ListLast = ListExpr Dhall.Syntax.List.ListLast

-- | > ListIndexed  ~  List/indexed
pattern ListIndexed :: Expr s a
pattern ListIndexed = ListExpr Dhall.Syntax.List.ListIndexed

-- | > ListReverse  ~  List/reverse
pattern ListReverse :: Expr s a
pattern ListReverse = ListExpr Dhall.Syntax.List.ListReverse

--------------------------------------------------------------------------------
-- Text builtin
--------------------------------------------------------------------------------

-- | > Text  ~  Text
pattern Text :: Expr s a
pattern Text = TextExpr Dhall.Syntax.Text.Text

-- | > TextLit (Chunks [(t1, e1), (t2, e2)] t3)  ~  "t1${e1}t2${e2}t3"
pattern TextLit :: Chunks s a -> Expr s a
pattern TextLit chunks = TextExpr (Dhall.Syntax.Text.TextLit chunks)

-- | > TextAppend x y  ~  x ++ y
pattern TextAppend :: Expr s a -> Expr s a -> Expr s a
pattern TextAppend text1 text2 = TextExpr (Dhall.Syntax.Text.TextAppend text1 text2)

-- | > TextReplace  ~  Text/replace
pattern TextReplace :: Expr s a
pattern TextReplace = TextExpr Dhall.Syntax.Text.TextReplace

-- | > TextShow  ~  Text/show
pattern TextShow :: Expr s a
pattern TextShow = TextExpr Dhall.Syntax.Text.TextShow
