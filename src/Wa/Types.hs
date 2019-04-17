{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}

module Wa.Types
  ( Universe(..)
  , Value(..)
  , Expr(..)
  , ExprM
  ) where

import GHC.Natural
import Data.Kind
import Wa.Dual

-- | The universe of types.
data Universe
  = Unit
  | Nat
  | F64
  | Dual
  | Function Universe Universe

-- | Values in our universe of types.
data Value :: Universe -> Type where
  ValueUnit :: () -- ^ the unit type
    => Value 'Unit 
  ValueNat :: () -- ^ a natural number
    => Natural
    -> Value 'Nat
  ValueF64 :: () -- ^ IEEE floating point 64-bit number
    => Double
    -> Value 'F64
  ValueDual :: () -- ^ a dual number
    => D
    -> Value 'Dual
  ValueFunction :: () -- ^ functions
    => (Value u -> Value u')
    -> Value ('Function u u')

data Expr :: (Universe -> Type) -> Universe -> Type where
  Lit :: () -- ^ A literval value, e.g. '1'
    => Value u
    -> Expr f u
  Plus :: () -- ^ dual addition
    => Expr f 'Dual
    -> Expr f 'Dual
    -> Expr f 'Dual
  Times :: () -- ^ dual multiplication
    => Expr f 'Dual
    -> Expr f 'Dual
    -> Expr f 'Dual
  Minus :: () -- ^ dual subtraction
    => Expr f 'Dual
    -> Expr f 'Dual
    -> Expr f 'Dual
  Negate :: () -- ^ dual negation
    => Expr f 'Dual
    -> Expr f 'Dual
  Abs :: () -- ^ dual absolute value
    => Expr f 'Dual
    -> Expr f 'Dual
  Signum :: () -- ^ dual signum
    => Expr f 'Dual
    -> Expr f 'Dual
  FracDiv :: () -- ^ dual division
    => Expr f 'Dual
    -> Expr f 'Dual
    -> Expr f 'Dual
  Exp :: () -- ^ dual base-e exponentiation
    => Expr f 'Dual
    -> Expr f 'Dual
  Log :: () -- ^ dual base-e logarithm
    => Expr f 'Dual
    -> Expr f 'Dual
  Sin :: () -- ^ dual sin
    => Expr f 'Dual
    -> Expr f 'Dual
  Cos :: () -- ^ dual cos
    => Expr f 'Dual
    -> Expr f 'Dual
  Tan :: () -- ^ dual tan
    => Expr f 'Dual
    -> Expr f 'Dual
  Asin :: () -- ^ dual asin
    => Expr f 'Dual
    -> Expr f 'Dual
  Acos :: () -- ^ dual acos
    => Expr f 'Dual
    -> Expr f 'Dual
  Atan :: () -- ^ dual atan
    => Expr f 'Dual
    -> Expr f 'Dual
  Sinh :: () -- ^ dual sinh
    => Expr f 'Dual
    -> Expr f 'Dual
  Cosh :: () -- ^ dual cosh
    => Expr f 'Dual
    -> Expr f 'Dual
  Tanh :: () -- ^ dual tanh
    => Expr f 'Dual
    -> Expr f 'Dual
  Asinh :: () -- ^ dual asinh
    => Expr f 'Dual
    -> Expr f 'Dual
  Acosh :: () -- ^ dual acosh
    => Expr f 'Dual
    -> Expr f 'Dual
  Atanh :: () -- ^ dual atanh
    => Expr f 'Dual
    -> Expr f 'Dual
  Apply :: () -- ^ function application
    => Expr f ('Function u u')
    -> Expr f u
    -> Expr f u'
  Lambda :: () -- ^ A (not necessarily anonymous) function
    => (f u -> Expr f u')
    -> Expr f ('Function u u')
  Var :: () -- ^ variable assignment
    => String -- ^ the name of the variable
    -> f u
    -> Expr f u
  Let :: () -- ^ assign a value inside of an Expr
    => Expr f u
    -> (f u -> Expr f u')
    -> Expr f u'
  Diff :: () -- ^ function differentation (forward-mode ad tower)
    => Expr f 'Nat -- ^ nth derivative
    -> Expr f 'F64 -- ^ value supplied to function
    -> Expr f ('Function 'Dual 'Dual) -- ^ function
    -> Expr f 'F64 -- ^ derivative at that point in time

instance forall (f :: Universe -> Type) u. (u ~ 'Dual) => Num (Expr f u) where
  (+) = Plus
  (*) = Times
  (-) = Minus
  abs = Abs
  signum = Signum
  fromInteger = Lit . ValueDual . fromInteger
  negate = Negate

instance forall (f :: Universe -> Type) u. (u ~ 'Dual) => Fractional (Expr f u) where
  fromRational = Lit . ValueDual . fromRational
  (/) = FracDiv

instance forall (f :: Universe -> Type) u. (u ~ 'Dual) => Floating (Expr f u) where
  pi = Lit . ValueDual $ pi
  exp = Exp
  log = Log
  sin = Sin
  cos = Cos
  tan = Tan
  asin = Asin
  acos = Acos
  atan = Atan
  sinh = Sinh
  cosh = Cosh
  asinh = Asinh
  acosh = Acosh
  atanh = Atanh

type ExprM f (p :: Universe) = forall u u'. (Expr f u -> Expr f u') -> Expr f p
