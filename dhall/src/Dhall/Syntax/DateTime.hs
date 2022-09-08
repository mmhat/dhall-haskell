{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.DateTime
    ( DateTimeExpr(..)
    , reservedIdentifiers
    ) where

import                Control.DeepSeq            (NFData)
import                Data.Data                  (Data)
import                Data.HashSet               (HashSet)
import                Data.Text                  (Text)
import                GHC.Generics               (Generic)
import                Language.Haskell.TH.Syntax (Lift)

import qualified Data.Fixed           as Fixed
import qualified Data.HashSet
import qualified Data.Time            as Time

deriving instance Lift Time.Day
deriving instance Lift Time.TimeOfDay
deriving instance Lift Time.TimeZone
deriving instance Lift (Fixed.Fixed a)

{-| Syntax tree for date an time expressions.
-}
data DateTimeExpr
    -- | > Date                                     ~  Date
    = Date
    -- | > DateLiteral (fromGregorian _YYYY _MM _DD) ~ YYYY-MM-DD
    | DateLiteral Time.Day
    -- | > Time                                     ~  Time
    | Time
    -- | > TimeLiteral (TimeOfDay hh mm ss) _       ~  hh:mm:ss
    | TimeLiteral
        Time.TimeOfDay
        Word -- ^ Precision
    -- | > TimeZone                                 ~  TimeZone
    | TimeZone
    -- | > TimeZoneLiteral (TimeZone ( 60 * _HH + _MM) _ _) ~ +HH:MM
    -- | > TimeZoneLiteral (TimeZone (-60 * _HH + _MM) _ _) ~ -HH:MM
    | TimeZoneLiteral Time.TimeZone
    deriving (Data, Eq, Generic, Lift, NFData, Ord, Show)

reservedIdentifiers :: HashSet Text
reservedIdentifiers = Data.HashSet.fromList
    [ "Date"
    , "Time"
    , "TimeZone"
    ]
