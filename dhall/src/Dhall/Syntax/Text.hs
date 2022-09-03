{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This module contains the core syntax types of the builtin List type.
-}

module Dhall.Syntax.Text
    ( TextExpr(..)
    , Chunks(..)

    , subExpressions
    , chunkExprs

    , reservedIdentifiers

    -- * `Data.Text.Text` manipulation
    , toDoubleQuoted
    , longestSharedWhitespacePrefix
    , linesLiteral
    , unlinesLiteral
    ) where

import                Control.DeepSeq            (NFData)
import                Data.Data                  (Data)
import                Data.HashSet               (HashSet)
import                Data.List.NonEmpty         (NonEmpty (..))
import                Data.String                (IsString (..))
import                Data.Text                  (Text)
import                Dhall.Src                  (Src (..))
import {-# SOURCE #-} Dhall.Syntax               (Expr)
import                GHC.Generics               (Generic)
import                Language.Haskell.TH.Syntax (Lift)

import qualified Data.Foldable
import qualified Data.HashSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text
import qualified Lens.Family        as Lens

-- | The body of an interpolated @Text@ literal
data Chunks s a = Chunks [(Text, Expr s a)] Text
    deriving (Functor, Foldable, Generic, Traversable, Show, Eq, Ord, Data, Lift, NFData)

instance Semigroup (Chunks s a) where
    Chunks xysL zL <> Chunks         []    zR =
        Chunks xysL (zL <> zR)
    Chunks xysL zL <> Chunks ((x, y):xysR) zR =
        Chunks (xysL ++ (zL <> x, y):xysR) zR

instance Monoid (Chunks s a) where
    mempty = Chunks [] mempty

instance IsString (Chunks s a) where
    fromString str = Chunks [] (fromString str)

{-| Syntax tree for Text expressions.
 - For the documentation of the type parameters @s@ and @a@ see the `Expr`.
-}
data TextExpr s a
    -- | > Text                                     ~  Text
    = Text
    -- | > TextLit (Chunks [(t1, e1), (t2, e2)] t3) ~  "t1${e1}t2${e2}t3"
    | TextLit (Chunks s a)
    -- | > TextAppend x y                           ~  x ++ y
    | TextAppend (Expr s a) (Expr s a)
    -- | > TextReplace                              ~ Text/replace
    | TextReplace
    -- | > TextShow                                 ~  Text/show
    | TextShow
    deriving (Data, Eq, Foldable, Generic, Lift, NFData, Ord, Show, Traversable)

-- This instance is hand-written due to the fact that deriving
-- it does not give us an INLINABLE pragma. We annotate this fmap
-- implementation with this pragma below to allow GHC to, possibly,
-- inline the implementation for performance improvements.
instance Functor (TextExpr s) where
  fmap f = Lens.over subExpressions (fmap f)
  {-# INLINABLE fmap #-}

-- | Generates a syntactically valid Dhall program
--instance Pretty a => Pretty (Expr s a) where
--    pretty = Pretty.unAnnotate . prettyExpr

-- | A traversal over the immediate sub-expressions of an `ListExpr`.
subExpressions
    :: Applicative f => (Expr s a -> f (Expr t b)) -> TextExpr s a -> f (TextExpr t b)
subExpressions _ Text = pure Text
subExpressions f (TextLit chunks) = TextLit <$> chunkExprs f chunks
subExpressions f (TextAppend a b) = TextAppend <$> f a <*> f b
subExpressions _ TextReplace = pure TextReplace
subExpressions _ TextShow = pure TextShow
{-# INLINABLE subExpressions #-}

-- | A traversal over the immediate sub-expressions in 'Chunks'.
chunkExprs
  :: Applicative f
  => (Expr s a -> f (Expr t b))
  -> Chunks s a -> f (Chunks t b)
chunkExprs f (Chunks chunks final) =
  flip Chunks final <$> traverse (traverse f) chunks
{-# INLINABLE chunkExprs #-}

-- | Reserved identifiers of the List builtin.
reservedIdentifiers :: HashSet Text
reservedIdentifiers =
    Data.HashSet.fromList
        [ "Text/replace"
        , "Text/show"
        , "Text"
        ]

-- | Same as @Data.Text.splitOn@, except always returning a `NonEmpty` result
splitOn :: Text -> Text -> NonEmpty Text
splitOn needle haystack =
    case Data.Text.splitOn needle haystack of
        []     -> "" :| []
        t : ts -> t  :| ts

-- | Split `Chunks` by lines
linesLiteral :: Chunks s a -> NonEmpty (Chunks s a)
linesLiteral (Chunks [] suffix) =
    fmap (Chunks []) (splitOn "\n" suffix)
linesLiteral (Chunks ((prefix, interpolation) : pairs₀) suffix₀) =
    foldr
        NonEmpty.cons
        (Chunks ((lastLine, interpolation) : pairs₁) suffix₁ :| chunks)
        (fmap (Chunks []) initLines)
  where
    splitLines = splitOn "\n" prefix

    initLines = NonEmpty.init splitLines
    lastLine  = NonEmpty.last splitLines

    Chunks pairs₁ suffix₁ :| chunks = linesLiteral (Chunks pairs₀ suffix₀)

-- | Flatten several `Chunks` back into a single `Chunks` by inserting newlines
unlinesLiteral :: NonEmpty (Chunks s a) -> Chunks s a
unlinesLiteral chunks =
    Data.Foldable.fold (NonEmpty.intersperse "\n" chunks)

-- | Returns `True` if the `Chunks` represents a blank line
emptyLine :: Chunks s a -> Bool
emptyLine (Chunks [] ""  ) = True
emptyLine (Chunks [] "\r") = True  -- So that `\r\n` is treated as a blank line
emptyLine  _               = False

-- | Return the leading whitespace for a `Chunks` literal
leadingSpaces :: Chunks s a -> Text
leadingSpaces chunks = Data.Text.takeWhile isSpace firstText
  where
    isSpace c = c == ' ' || c == '\t'

    firstText =
        case chunks of
            Chunks                []  suffix -> suffix
            Chunks ((prefix, _) : _ ) _      -> prefix

{-| Compute the longest shared whitespace prefix for the purposes of stripping
    leading indentation
-}
longestSharedWhitespacePrefix :: NonEmpty (Chunks s a) -> Text
longestSharedWhitespacePrefix literals =
    case fmap leadingSpaces filteredLines of
        l : ls -> Data.Foldable.foldl' sharedPrefix l ls
        []     -> ""
  where
    sharedPrefix ab ac =
        case Data.Text.commonPrefixes ab ac of
            Just (a, _b, _c) -> a
            Nothing          -> ""

    -- The standard specifies to filter out blank lines for all lines *except*
    -- for the last line
    filteredLines = newInit <> pure oldLast
      where
        oldInit = NonEmpty.init literals

        oldLast = NonEmpty.last literals

        newInit = filter (not . emptyLine) oldInit

-- | Drop the first @n@ characters for a `Chunks` literal
dropLiteral :: Int -> Chunks s a -> Chunks s a
dropLiteral n (Chunks [] suffix) =
    Chunks [] (Data.Text.drop n suffix)
dropLiteral n (Chunks ((prefix, interpolation) : rest) suffix) =
    Chunks ((Data.Text.drop n prefix, interpolation) : rest) suffix

{-| Convert a single-quoted `Chunks` literal to the equivalent double-quoted
    `Chunks` literal
-}
toDoubleQuoted :: Chunks Src a -> Chunks Src a
toDoubleQuoted literal =
    unlinesLiteral (fmap (dropLiteral indent) literals)
  where
    literals = linesLiteral literal

    longestSharedPrefix = longestSharedWhitespacePrefix literals

    indent = Data.Text.length longestSharedPrefix
