module Dhall.Syntax where

import Control.DeepSeq            (NFData)
import Data.Bifunctor             (Bifunctor)
import Data.Data                  (Data)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter              (Pretty)

data Var

data Const

data Expr s a

instance Bifunctor Expr
instance (Data s, Data a) => Data (Expr s a)
instance (Eq s, Eq a) => Eq (Expr s a)
instance Foldable (Expr s)
instance Functor (Expr s)
instance (Lift s, Lift a) => Lift (Expr s a)
instance (NFData s, NFData a) => NFData (Expr s a)
instance (Ord s, Ord a) => Ord (Expr s a)
instance Pretty a => Pretty (Expr s a)
instance (Show s, Show a) => Show (Expr s a)
instance Traversable (Expr s)
