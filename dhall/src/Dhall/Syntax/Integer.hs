{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dhall.Syntax.Integer
    ( IntegerExpr(..)
    , reservedIdentifiers
    ) where

import                Control.DeepSeq            (NFData)
import                Data.Data                  (Data)
import                Data.HashSet               (HashSet)
import                Data.Text                  (Text)
import                GHC.Generics               (Generic)
import                Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashSet

data IntegerExpr
    -- | > Integer                                  ~  Integer
    = Integer
    -- | > IntegerLit n                             ~  ±n
    | IntegerLit Integer
    -- | > IntegerClamp                             ~  Integer/clamp
    | IntegerClamp
    -- | > IntegerNegate                            ~  Integer/negate
    | IntegerNegate
    -- | > IntegerShow                              ~  Integer/show
    | IntegerShow
    -- | > IntegerToDouble                          ~  Integer/toDouble
    | IntegerToDouble
    deriving (Data, Eq, Generic, Lift, NFData, Ord, Show)

reservedIdentifiers :: HashSet Text
reservedIdentifiers = Data.HashSet.fromList
        [ "Integer"
        , "Integer/clamp"
        , "Integer/negate"
        , "Integer/show"
        , "Integer/toDouble"
        , "Integer/show"
        ]
