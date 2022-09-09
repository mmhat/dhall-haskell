{-# LANGUAGE PatternSynonyms #-}

{-| This module contains pattern synonyms useful to maintain some backwards
 - compatibilty.
-}

module Dhall.Syntax.Patterns (
    Expr
        ( Bool
        , BoolLit
        , BoolAnd
        , BoolEQ
        , BoolIf
        , BoolNE
        , BoolOr
        , Date
        , DateLiteral
        , Time
        , TimeLiteral
        , TimeZone
        , TimeZoneLiteral
        , Double
        , DoubleLit
        , DoubleShow
        , Integer
        , IntegerLit
        , IntegerClamp
        , IntegerNegate
        , IntegerShow
        , IntegerToDouble
        , List
        , ListLit
        , ListAppend
        , ListBuild
        , ListFold
        , ListLength
        , ListHead
        , ListLast
        , ListIndexed
        , ListReverse
        , Natural
        , NaturalLit
        , NaturalFold
        , NaturalBuild
        , NaturalIsZero
        , NaturalEven
        , NaturalOdd
        , NaturalToInteger
        , NaturalShow
        , NaturalSubtract
        , NaturalPlus
        , NaturalTimes
        , Record
        , RecordLit
        , Combine
        , CombineTypes
        , Prefer
        , Project
        , RecordCompletion
        , ToMap
        , With
        , Text
        , TextLit
        , TextAppend
        , TextReplace
        , TextShow
        , Optional
        , Some
        , None
        , Union
        , Merge
        , ShowConstructor
        )
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Sequence     (Seq)
import Data.Text         (Text)
import Data.Time         (Day, TimeOfDay, TimeZone)
import Dhall.Map (Map)
import {-# SOURCE #-} Dhall.Pretty.Internal (CharacterSet)
import Dhall.Syntax      (Expr (..))
import Dhall.Syntax.Double (DhallDouble)
import Dhall.Syntax.Record (PreferAnnotation, RecordField, WithComponent)
import Dhall.Syntax.Text (Chunks)
import Numeric.Natural   (Natural)

import qualified Dhall.Syntax.Bool
import qualified Dhall.Syntax.DateTime
import qualified Dhall.Syntax.Double
import qualified Dhall.Syntax.Integer
import qualified Dhall.Syntax.List
import qualified Dhall.Syntax.Natural
import qualified Dhall.Syntax.Record
import qualified Dhall.Syntax.Text
import qualified Dhall.Syntax.Union

--------------------------------------------------------------------------------
-- Bool builtin
--------------------------------------------------------------------------------

-- | > Bool  ~  Bool
pattern Bool :: Expr s a
pattern Bool = BoolExpr Dhall.Syntax.Bool.Bool

-- | > BoolLit b  ~  b
pattern BoolLit :: Bool -> Expr s a
pattern BoolLit b = BoolExpr (Dhall.Syntax.Bool.BoolLit b)

-- | > BoolAnd x y  ~  x && y
pattern BoolAnd :: Expr s a -> Expr s a -> Expr s a
pattern BoolAnd x y = BoolExpr (Dhall.Syntax.Bool.BoolAnd x y)

-- | > BoolOr x y  ~  x || y
pattern BoolOr :: Expr s a -> Expr s a -> Expr s a
pattern BoolOr x y = BoolExpr (Dhall.Syntax.Bool.BoolOr x y)

-- | > BoolEQ x y  ~  x == y
pattern BoolEQ :: Expr s a -> Expr s a -> Expr s a
pattern BoolEQ x y = BoolExpr (Dhall.Syntax.Bool.BoolEQ x y)

-- | > BoolNE x y  ~  x != y
pattern BoolNE :: Expr s a -> Expr s a -> Expr s a
pattern BoolNE x y = BoolExpr (Dhall.Syntax.Bool.BoolNE x y)

-- | > BoolIf x y z  ~  if x then y else z
pattern BoolIf :: Expr s a -> Expr s a -> Expr s a -> Expr s a
pattern BoolIf b t f = BoolExpr (Dhall.Syntax.Bool.BoolIf b t f)

--------------------------------------------------------------------------------
-- DateTime builtin
--------------------------------------------------------------------------------

-- | > Date  ~  Date
pattern Date :: Expr s a
pattern Date = DateTimeExpr Dhall.Syntax.DateTime.Date

-- | > DateLiteral (fromGregorian _YYYY _MM _DD)  ~  YYYY-MM-DD
pattern DateLiteral :: Day -> Expr s a
pattern DateLiteral day = DateTimeExpr (Dhall.Syntax.DateTime.DateLiteral day)

-- | > Time  ~  Time
pattern Time :: Expr s a
pattern Time = DateTimeExpr Dhall.Syntax.DateTime.Time

-- | > TimeLiteral (TimeOfDay hh mm ss) _  ~  hh:mm:ss
pattern TimeLiteral
    :: TimeOfDay
    -> Word -- ^ Precision
    -> Expr s a
pattern TimeLiteral tod prec = DateTimeExpr (Dhall.Syntax.DateTime.TimeLiteral tod prec)

-- | > TimeZone  ~  TimeZone
pattern TimeZone :: Expr s a
pattern TimeZone = DateTimeExpr Dhall.Syntax.DateTime.TimeZone

-- | > TimeZoneLiteral (TimeZone ( 60 * _HH + _MM) _ _)  ~  +HH:MM
-- | > TimeZoneLiteral (TimeZone (-60 * _HH + _MM) _ _)  ~  -HH:MM
pattern TimeZoneLiteral :: TimeZone -> Expr s a
pattern TimeZoneLiteral tz = DateTimeExpr (Dhall.Syntax.DateTime.TimeZoneLiteral tz)

--------------------------------------------------------------------------------
-- Double builtin
--------------------------------------------------------------------------------

-- | > Double  ~  Double
pattern Double :: Expr s a
pattern Double = DoubleExpr Dhall.Syntax.Double.Double

-- | > DoubleLit n  ~  n
pattern DoubleLit :: DhallDouble -> Expr s a
pattern DoubleLit n = DoubleExpr (Dhall.Syntax.Double.DoubleLit n)

-- | > DoubleShow  ~  Double/show
pattern DoubleShow :: Expr s a
pattern DoubleShow = DoubleExpr Dhall.Syntax.Double.DoubleShow

--------------------------------------------------------------------------------
-- Integer builtin
--------------------------------------------------------------------------------

-- | > Integer  ~  Integer
pattern Integer :: Expr s a
pattern Integer = IntegerExpr Dhall.Syntax.Integer.Integer

-- | > IntegerLit n  ~  ±n
pattern IntegerLit :: Integer -> Expr s a
pattern IntegerLit i = IntegerExpr (Dhall.Syntax.Integer.IntegerLit i)

-- | > IntegerClamp  ~  Integer/clamp
pattern IntegerClamp :: Expr s a
pattern IntegerClamp = IntegerExpr Dhall.Syntax.Integer.IntegerClamp

-- | > IntegerNegate  ~  Integer/negate
pattern IntegerNegate :: Expr s a
pattern IntegerNegate = IntegerExpr Dhall.Syntax.Integer.IntegerNegate

-- | > IntegerShow  ~  Integer/show
pattern IntegerShow :: Expr s a
pattern IntegerShow = IntegerExpr Dhall.Syntax.Integer.IntegerShow

-- | > IntegerToDouble  ~  Integer/toDouble
pattern IntegerToDouble :: Expr s a
pattern IntegerToDouble = IntegerExpr Dhall.Syntax.Integer.IntegerToDouble

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
-- Natural builtin
--------------------------------------------------------------------------------

-- | > Natural  ~  Natural
pattern Natural :: Expr s a
pattern Natural = NaturalExpr Dhall.Syntax.Natural.Natural

-- | > NaturalLit n  ~  n
pattern NaturalLit :: Natural -> Expr s a
pattern NaturalLit n = NaturalExpr (Dhall.Syntax.Natural.NaturalLit n)

-- | > NaturalFold  ~  Natural/fold
pattern NaturalFold :: Expr s a
pattern NaturalFold = NaturalExpr Dhall.Syntax.Natural.NaturalFold

-- | > NaturalBuild  ~  Natural/build
pattern NaturalBuild :: Expr s a
pattern NaturalBuild = NaturalExpr Dhall.Syntax.Natural.NaturalBuild

-- | > NaturalIsZero  ~  Natural/isZero
pattern NaturalIsZero :: Expr s a
pattern NaturalIsZero = NaturalExpr Dhall.Syntax.Natural.NaturalIsZero

-- | > NaturalEven  ~  Natural/even
pattern NaturalEven :: Expr s a
pattern NaturalEven = NaturalExpr Dhall.Syntax.Natural.NaturalEven

-- | > NaturalOdd  ~  Natural/odd
pattern NaturalOdd :: Expr s a
pattern NaturalOdd = NaturalExpr Dhall.Syntax.Natural.NaturalOdd

-- | > NaturalToInteger  ~  Natural/toInteger
pattern NaturalToInteger :: Expr s a
pattern NaturalToInteger = NaturalExpr Dhall.Syntax.Natural.NaturalToInteger

-- | > NaturalShow  ~  Natural/show
pattern NaturalShow :: Expr s a
pattern NaturalShow = NaturalExpr Dhall.Syntax.Natural.NaturalShow

-- | > NaturalSubtract  ~  Natural/subtract
pattern NaturalSubtract :: Expr s a
pattern NaturalSubtract = NaturalExpr Dhall.Syntax.Natural.NaturalSubtract

-- | > NaturalPlus x y  ~  x + y
pattern NaturalPlus :: Expr s a -> Expr s a -> Expr s a
pattern NaturalPlus x y = NaturalExpr (Dhall.Syntax.Natural.NaturalPlus x y)

-- | > NaturalTimes x y  ~  x * y
pattern NaturalTimes :: Expr s a -> Expr s a -> Expr s a
pattern NaturalTimes x y = NaturalExpr (Dhall.Syntax.Natural.NaturalTimes x y)

--------------------------------------------------------------------------------
-- Record builtin
--------------------------------------------------------------------------------

-- | > Record [ (k1, RecordField _ t1)  ~  { k1 : t1, k2 : t1 }
--   >        , (k2, RecordField _ t2)
--   >        ]
pattern Record :: Map Text (RecordField s a) -> Expr s a
pattern Record m = RecordExpr (Dhall.Syntax.Record.Record m)

-- | > RecordLit [ (k1, RecordField _ v1)  ~  { k1 = v1, k2 = v2 }
--   >           , (k2, RecordField _ v2)
--   >           ]
pattern RecordLit :: Map Text (RecordField s a) -> Expr s a
pattern RecordLit m = RecordExpr (Dhall.Syntax.Record.RecordLit m)

-- | > Combine _ Nothing x y  ~  x ∧ y
--
-- The second field is a `Just` when the `Combine` operator is introduced
-- as a result of desugaring duplicate record fields:
--
--   > RecordLit [ (k, RecordField _ (Combine (Just k) x y))  ~  { k = x, k = y }
--   >           ]
pattern Combine :: Maybe CharacterSet -> Maybe Text -> Expr s a -> Expr s a -> Expr s a
pattern Combine charset combine x y = RecordExpr (Dhall.Syntax.Record.Combine charset combine x y)

-- | > CombineTypes _ x y  ~  x ⩓ y
pattern CombineTypes :: Maybe CharacterSet -> Expr s a -> Expr s a -> Expr s a
pattern CombineTypes charset x y = RecordExpr (Dhall.Syntax.Record.CombineTypes charset x y)

-- | > Prefer _ _ x y  ~  x ⫽ y
pattern Prefer :: Maybe CharacterSet -> PreferAnnotation -> Expr s a -> Expr s a -> Expr s a
pattern Prefer charset prefer x y = RecordExpr (Dhall.Syntax.Record.Prefer charset prefer x y)

-- | > RecordCompletion x y  ~  x::y
pattern RecordCompletion :: Expr s a -> Expr s a -> Expr s a
pattern RecordCompletion x y = RecordExpr (Dhall.Syntax.Record.RecordCompletion x y)

-- | > ToMap x (Just t)  ~  toMap x : t
--   > ToMap x  Nothing  ~  toMap x
pattern ToMap :: Expr s a -> Maybe (Expr s a) -> Expr s a
pattern ToMap x type_ = RecordExpr (Dhall.Syntax.Record.ToMap x type_)

-- | > Project e (Left xs)  ~  e.{ xs }
--   > Project e (Right t)  ~  e.(t)
pattern Project :: Expr s a -> Either [Text] (Expr s a) -> Expr s a
pattern Project x proj = RecordExpr (Dhall.Syntax.Record.Project x proj)

-- | > With x y e  ~  x with y = e
pattern With :: Expr s a -> NonEmpty WithComponent -> Expr s a -> Expr s a
pattern With x withs y = RecordExpr (Dhall.Syntax.Record.With x withs y)

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

--------------------------------------------------------------------------------
-- Text builtin
--------------------------------------------------------------------------------

-- | > Optional  ~  Optional
pattern Optional :: Expr s a
pattern Optional = UnionExpr Dhall.Syntax.Union.Optional

-- | > Some e  ~  Some e
pattern Some :: Expr s a -> Expr s a
pattern Some expr = UnionExpr (Dhall.Syntax.Union.Some expr)

-- | > None  ~  None
pattern None :: Expr s a
pattern None = UnionExpr Dhall.Syntax.Union.None

-- | > Union [(k1, Just t1), (k2, Nothing)]  ~  < k1 : t1 | k2 >
pattern Union :: Map Text (Maybe (Expr s a)) -> Expr s a
pattern Union m = UnionExpr (Dhall.Syntax.Union.Union m)

-- | > Merge x y (Just t )  ~  merge x y : t
--   > Merge x y  Nothing   ~  merge x y
pattern Merge :: Expr s a -> Expr s a -> Maybe (Expr s a) -> Expr s a
pattern Merge x y type_ = UnionExpr (Dhall.Syntax.Union.Merge x y type_)

-- | > ShowConstructor x  ~  showConstructor x
pattern ShowConstructor :: Expr s a -> Expr s a
pattern ShowConstructor expr = UnionExpr (Dhall.Syntax.Union.ShowConstructor expr)
