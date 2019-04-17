{-# language DataKinds, KindSignatures, LambdaCase, GADTs, RankNTypes #-}

module Wa.Evaluate
  ( evaluate
  
  , unNat
  , unF64
  , unDual
  , unFunction
  ) where

import Wa.Dual
import Wa.Types
import Data.Kind (Type)
import GHC.Natural (Natural, naturalToInt)

evaluate :: forall (u :: Universe). ()
  => (forall (f :: Universe -> Type). Expr f u)
  -> Value u
evaluate e0 = go e0 where
  go :: forall v. Expr Value v -> Value v
  go = \case
    Lit v -> v
    Plus x y -> ValueDual (unDual (go x) + unDual (go y))
    Times x y -> ValueDual (unDual (go x) * unDual (go y))
    Minus x y -> ValueDual (unDual (go x) - unDual (go y))
    Abs x -> ValueDual (abs (unDual (go x)))
    Signum x -> ValueDual (signum (unDual (go x)))
    Negate x -> ValueDual (negate (unDual (go x)))
    FracDiv x y -> ValueDual (unDual (go x) / unDual (go y))
    Exp x -> ValueDual (exp (unDual (go x)))
    Log x -> ValueDual (log (unDual (go x)))
    Sin x -> ValueDual (sin (unDual (go x)))
    Cos x -> ValueDual (cos (unDual (go x)))
    Tan x -> ValueDual (tan (unDual (go x)))
    Asin x -> ValueDual (asin (unDual (go x)))
    Acos x -> ValueDual (acos (unDual (go x)))
    Atan x -> ValueDual (atan (unDual (go x)))
    Sinh x -> ValueDual (sinh (unDual (go x)))
    Cosh x -> ValueDual (cosh (unDual (go x)))
    Tanh x -> ValueDual (tanh (unDual (go x)))
    Asinh x -> ValueDual (asinh (unDual (go x)))
    Acosh x -> ValueDual (acosh (unDual (go x)))
    Atanh x -> ValueDual (atanh (unDual (go x)))
    Apply g x -> unFunction (go g) (go x)
    Var _ x -> x
    Lambda g -> ValueFunction (go . g)
    Let x g -> go (g (go x))
    Diff n x g ->
      let h f = \d -> unDual (f (ValueDual d))
       in ValueF64 $ diff
         (naturalToInt (unNat (go n)))
         (unF64 (go x))
         (h (unFunction (go g)))

unNat :: Value 'Nat -> Natural
unNat (ValueNat n) = n

unF64 :: Value 'F64 -> Double
unF64 (ValueF64 d) = d

unDual :: Value 'Dual -> D
unDual (ValueDual d) = d

unFunction :: Value ('Function u u') -> Value u -> Value u'
unFunction (ValueFunction f) = f
