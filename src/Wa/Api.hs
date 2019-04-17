{-# language DataKinds, KindSignatures, RankNTypes #-}

module Wa.Api
  ( diff_
  , let_
  , lit_
  , nat_
  , f64_
  , apply_
  , lambda_
  , runExprM
  ) where

import Data.Coerce
import Data.Kind
import GHC.Natural
import Wa.Evaluate
import Wa.Types

diff_ :: Expr f 'Nat -> Expr f 'F64 -> Expr f ('Function 'Dual 'Dual) -> Expr f 'F64
diff_ = Diff

let_ :: Expr f u -> (Expr f u -> Expr f u') -> Expr f u'
let_ e f = Let e (coerce f . Var "bad name in let_")

lit_ :: Value u -> Expr f u
lit_ = Lit

nat_ :: Natural -> ExprM f 'Nat
nat_ n _ = Lit . ValueNat $ n

f64_ :: Double -> ExprM f 'F64
f64_ d _ = Lit . ValueF64 $ d

apply_ :: Expr f ('Function u u') -> Expr f u -> Expr f u'
apply_ = Apply

lambda_ :: String -> (Expr f u -> Expr f u') -> Expr f ('Function u u')
lambda_ name f = Lambda (coerce f . Var name)

runExprM :: (forall (f :: Universe -> Type). ExprM f u) -> Value u
runExprM e = evaluate (e id)
