{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dhall.Syntax.Double
    ( DoubleExpr(..)
    , DhallDouble(..)
    , reservedIdentifiers
    ) where

import                Control.DeepSeq            (NFData)
import                Data.Bits                  (xor)
import                Data.Data                  (Data)
import                Data.HashSet               (HashSet)
import                Data.Text                  (Text)
import                GHC.Generics               (Generic)
import                Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashSet

-- | This wrapper around 'Prelude.Double' exists for its 'Eq' instance which is
-- defined via the binary encoding of Dhall @Double@s.
newtype DhallDouble = DhallDouble { getDhallDouble :: Double }
    deriving (Data, Generic, Lift, NFData, Show)

-- | This instance satisfies all the customary 'Eq' laws except substitutivity.
--
-- In particular:
--
-- >>> nan = DhallDouble (0/0)
-- >>> nan == nan
-- True
--
-- This instance is also consistent with with the binary encoding of Dhall @Double@s:
--
-- >>> toBytes n = Dhall.Binary.encodeExpression (DoubleLit n :: Expr Void Import)
--
-- prop> \a b -> (a == b) == (toBytes a == toBytes b)
instance Eq DhallDouble where
    DhallDouble a == DhallDouble b
        | isNaN a && isNaN b                      = True
        | isNegativeZero a `xor` isNegativeZero b = False
        | otherwise                               = a == b

-- | This instance relies on the 'Eq' instance for 'DhallDouble' but cannot
-- satisfy the customary 'Ord' laws when @NaN@ is involved.
instance Ord DhallDouble where
    compare a@(DhallDouble a') b@(DhallDouble b') =
        if a == b
            then EQ
            else compare a' b'

data DoubleExpr
    -- | > Double                                   ~  Double
    = Double
    -- | > DoubleLit n                              ~  n
    | DoubleLit DhallDouble
    -- | > DoubleShow                               ~  Double/show
    | DoubleShow
    deriving (Data, Eq, Generic, Lift, NFData, Ord, Show)

reservedIdentifiers :: HashSet Text
reservedIdentifiers = Data.HashSet.fromList
    [ "Double/show"
    , "Double"
    ]
