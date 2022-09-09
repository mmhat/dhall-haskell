{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Record
    ( -- * 'Expr'
      RecordExpr(..)
    , RecordField(..)
    , makeRecordField
    , PreferAnnotation(..)
    , WithComponent(..)

    -- ** Optics
    , subExpressions
    , unsafeSubExpressions
    , recordFieldExprs
    ) where

import                Control.DeepSeq            (NFData)
import                Data.Bifunctor             (Bifunctor (..))
import                Data.Data                  (Data)
--import                Data.HashSet               (HashSet)
import                Data.List.NonEmpty         (NonEmpty (..))
--import                Data.String                (IsString (..))
import                Data.Text                  (Text)
--import                Data.Traversable           ()
--import                Data.Void                  (Void)
import                Dhall.Map                  (Map)
import {-# SOURCE #-} Dhall.Pretty.Internal
import {-# SOURCE #-} Dhall.Syntax                  (Expr)
import                GHC.Generics               (Generic)
--import                Instances.TH.Lift          ()
import                Language.Haskell.TH.Syntax (Lift)
--import                Prettyprinter              (Pretty)
--import                Unsafe.Coerce              (unsafeCoerce)

--import qualified Control.Monad
--import qualified Data.HashSet
--import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Text
--import qualified Dhall.Syntax.Bool
--import qualified Dhall.Syntax.DateTime
--import qualified Dhall.Syntax.Double
--import qualified Dhall.Syntax.Import
--import qualified Dhall.Syntax.Integer
--import qualified Dhall.Syntax.List
--import qualified Dhall.Syntax.Natural
--import qualified Dhall.Syntax.Text
import qualified Lens.Family          as Lens
--import qualified Prettyprinter        as Pretty

-- | Used to record the origin of a @//@ operator (i.e. from source code or a
-- product of desugaring)
data PreferAnnotation
    = PreferFromSource
    | PreferFromCompletion
    deriving (Data, Eq, Generic, Lift, NFData, Ord, Show)

-- | Record the field of a record-type and record-literal expression.
-- The reason why we use the same ADT for both of them is because they store
-- the same information.
--
-- For example,
--
-- > { {- A -} x {- B -} : {- C -} T }
--
-- ... or
--
-- > { {- A -} x {- B -} = {- C -} T }
--
-- will be instantiated as follows:
--
-- * @recordFieldSrc0@ corresponds to the @A@ comment.
-- * @recordFieldValue@ is @"T"@
-- * @recordFieldSrc1@ corresponds to the @B@ comment.
-- * @recordFieldSrc2@ corresponds to the @C@ comment.
--
-- Although the @A@ comment isn't annotating the @"T"@ Record Field,
-- this is the best place to keep these comments.
--
-- Note that @recordFieldSrc2@ is always 'Nothing' when the 'RecordField' is for
-- a punned entry, because there is no @=@ sign. For example,
--
-- > { {- A -} x {- B -} }
--
-- will be instantiated as follows:
--
-- * @recordFieldSrc0@ corresponds to the @A@ comment.
-- * @recordFieldValue@ corresponds to @(Var "x")@
-- * @recordFieldSrc1@ corresponds to the @B@ comment.
-- * @recordFieldSrc2@ will be 'Nothing'
--
-- The labels involved in a record using dot-syntax like in this example:
--
-- > { {- A -} a {- B -} . {- C -} b {- D -} . {- E -} c {- F -} = {- G -} e }
--
-- will be instantiated as follows:
--
-- * For both the @a@ and @b@ field, @recordfieldSrc2@ is 'Nothing'
-- * For the @a@ field:
--   * @recordFieldSrc0@ corresponds to the @A@ comment
--   * @recordFieldSrc1@ corresponds to the @B@ comment
-- * For the @b@ field:
--   * @recordFieldSrc0@ corresponds to the @C@ comment
--   * @recordFieldSrc1@ corresponds to the @D@ comment
-- * For the @c@ field:
--   * @recordFieldSrc0@ corresponds to the @E@ comment
--   * @recordFieldSrc1@ corresponds to the @F@ comment
--   * @recordFieldSrc2@ corresponds to the @G@ comment
--
-- That is, for every label except the last one the semantics of
-- @recordFieldSrc0@ and @recordFieldSrc1@ are the same from a regular record
-- label but @recordFieldSrc2@ is always 'Nothing'. For the last keyword, all
-- srcs are 'Just'
data RecordField s a = RecordField
    { recordFieldSrc0  :: Maybe s
    , recordFieldValue :: Expr s a
    , recordFieldSrc1  :: Maybe s
    , recordFieldSrc2  :: Maybe s
    } deriving (Data, Eq, Foldable, Functor, Generic, Lift, NFData, Ord, Show, Traversable)

-- | Construct a 'RecordField' with no src information
makeRecordField :: Expr s a -> RecordField s a
makeRecordField e = RecordField Nothing e Nothing Nothing

instance Bifunctor RecordField where
    first k (RecordField s0 value s1 s2) =
        RecordField (k <$> s0) (first k value) (k <$> s1) (k <$> s2)
    second = fmap

-- | A path component for a @with@ expression
data WithComponent = WithLabel Text | WithQuestion
    deriving (Data, Eq, Generic, Lift, NFData, Ord, Show)

data RecordExpr s a
    -- | > Record [ (k1, RecordField _ t1)          ~  { k1 : t1, k2 : t1 }
    --   >        , (k2, RecordField _ t2)
    --   >        ]
    = Record (Map Text (RecordField s a))
    -- | > RecordLit [ (k1, RecordField _ v1)       ~  { k1 = v1, k2 = v2 }
    --   >           , (k2, RecordField _ v2)
    --   >           ]
    | RecordLit (Map Text (RecordField s a))
    -- | > Combine _ Nothing x y                    ~  x ∧ y
    --
    -- The first field is a `Just` when the `Combine` operator is introduced
    -- as a result of desugaring duplicate record fields:
    --
    --   > RecordLit [ ( k                          ~ { k = x, k = y }
    --   >           , RecordField
    --   >              _
    --   >              (Combine (Just k) x y)
    --   >            )]
    | Combine (Maybe CharacterSet) (Maybe Text) (Expr s a) (Expr s a)
    -- | > CombineTypes _ x y                       ~  x ⩓ y
    | CombineTypes (Maybe CharacterSet) (Expr s a) (Expr s a)
    -- | > Prefer _ _ x y                           ~  x ⫽ y
    | Prefer (Maybe CharacterSet) PreferAnnotation (Expr s a) (Expr s a)
    -- | > RecordCompletion x y                     ~  x::y
    | RecordCompletion (Expr s a) (Expr s a)
    -- | > ToMap x (Just t)                         ~  toMap x : t
    --   > ToMap x  Nothing                         ~  toMap x
    | ToMap (Expr s a) (Maybe (Expr s a))
    -- | > Project e (Left xs)                      ~  e.{ xs }
    --   > Project e (Right t)                      ~  e.(t)
    | Project (Expr s a) (Either [Text] (Expr s a))
    -- | > With x y e                               ~  x with y = e
    | With (Expr s a) (NonEmpty WithComponent) (Expr s a)
    deriving (Data, Eq, Foldable, Generic, Lift, NFData, Ord, Show, Traversable)

-- This instance is hand-written due to the fact that deriving
-- it does not give us an INLINABLE pragma. We annotate this fmap
-- implementation with this pragma below to allow GHC to, possibly,
-- inline the implementation for performance improvements.
instance Functor (RecordExpr s) where
  fmap f (Record a) = Record $ fmap f <$> a
  fmap f (RecordLit a) = RecordLit $ fmap f <$> a
  fmap f expression = Lens.over unsafeSubExpressions (fmap f) expression
  {-# INLINABLE fmap #-}

instance Bifunctor RecordExpr where
    first k (Record a   ) = Record $ first k <$> a
    first k (RecordLit a) = RecordLit $ first k <$> a
    first k  expression  = Lens.over unsafeSubExpressions (first k) expression

    second = fmap

-- | A traversal over the immediate sub-expressions of an expression.
subExpressions
    :: Applicative f => (Expr s a -> f (Expr s b)) -> RecordExpr s a -> f (RecordExpr s b)
subExpressions f (Record a) = Record <$> traverse (recordFieldExprs f) a
subExpressions f (RecordLit a) = RecordLit <$> traverse (recordFieldExprs f) a
subExpressions f expression = unsafeSubExpressions f expression
{-# INLINABLE subExpressions #-}

{-| An internal utility used to implement transformations that require changing
    one of the type variables of the `Expr` type

    This utility only works because the implementation is partial, not
    handling the `Let`, `Note`, or `Embed` cases, which need to be handled by
    the caller.
-}
unsafeSubExpressions
    :: Applicative f => (Expr s a -> f (Expr t b)) -> RecordExpr s a -> f (RecordExpr t b)
unsafeSubExpressions f (Combine cs a b c) = Combine cs a <$> f b <*> f c
unsafeSubExpressions f (CombineTypes cs a b) = CombineTypes cs <$> f a <*> f b
unsafeSubExpressions f (Prefer cs a b c) = Prefer cs <$> pure a <*> f b <*> f c
unsafeSubExpressions f (RecordCompletion a b) = RecordCompletion <$> f a <*> f b
unsafeSubExpressions f (ToMap a t) = ToMap <$> f a <*> traverse f t
unsafeSubExpressions f (Project a b) = Project <$> f a <*> traverse f b
unsafeSubExpressions f (With a b c) = With <$> f a <*> pure b <*> f c
unsafeSubExpressions _ (Record {}) = unhandledConstructor "Record"
unsafeSubExpressions _ (RecordLit {}) = unhandledConstructor "RecordLit"
{-# INLINABLE unsafeSubExpressions #-}

unhandledConstructor :: Text -> a
unhandledConstructor constructor =
    internalError
        (   "Dhall.Syntax.Record.unsafeSubExpressions: Unhandled "
        <>  constructor
        <>  " construtor"
        )

{-| Traverse over the immediate 'Expr' children in a 'RecordField'.
-}
recordFieldExprs
    :: Applicative f
    => (Expr s a -> f (Expr s b))
    -> RecordField s a -> f (RecordField s b)
recordFieldExprs f (RecordField s0 e s1 s2) =
    RecordField
        <$> pure s0
        <*> f e
        <*> pure s1
        <*> pure s2
{-# INLINABLE recordFieldExprs #-}

_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"

{-| Utility function used to throw internal errors that should never happen
    (in theory) but that are not enforced by the type system
-}
internalError :: Data.Text.Text -> forall b . b
internalError text = error (unlines
    [ _ERROR <> ": Compiler bug                                                        "
    , "                                                                                "
    , "Explanation: This error message means that there is a bug in the Dhall compiler."
    , "You didn't do anything wrong, but if you would like to see this problem fixed   "
    , "then you should report the bug at:                                              "
    , "                                                                                "
    , "https://github.com/dhall-lang/dhall-haskell/issues                              "
    , "                                                                                "
    , "Please include the following text in your bug report:                           "
    , "                                                                                "
    , "```                                                                             "
    , Data.Text.unpack text <> "                                                       "
    , "```                                                                             "
    ] )
