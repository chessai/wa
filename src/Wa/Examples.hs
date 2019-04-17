{-# language DataKinds #-}

module Wa.Examples where

import Wa.Types
import Wa.Api

-- | Compute f'(x) = sin(log(log(log(x)))) at x = 3.5
d1 :: ExprM f 'F64
d1 = do
  n <- nat_ 1
  x <- f64_ 3.5
  pure $ diff_ n x (lambda_ "f" (\d -> sin . log . log . log $ d))

-- | succ x = x + 1
succ :: Expr f ('Function 'Dual 'Dual)
succ = lambda_ "succ" (+1)

-- | pred x = x - 1
pred :: Expr f ('Function 'Dual 'Dual)
pred = lambda_ "pred" (\x -> x - 1)
