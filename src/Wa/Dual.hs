module Wa.Dual
  ( D(..)
  , diff
  ) where

data D = D !Double D

instance Num D where
  D u du + D v dv = D (u + v) (du + dv)
  x@(D u du) * y@(D v dv) = D (u * v) (x * dv + y * du)
  D u du - D v dv = D (u - v) (du - dv)
  abs (D u du) = D (abs u) (abs du)
  signum (D u _du) = D (signum u) 0
  fromInteger x = D (fromInteger x) 0

instance Fractional D where
  fromRational n = D (fromRational n) 0
  x@(D u du) / y@(D v dv) = D (u / v) ((y * du  - x * dv)/ y^(2 :: Int))

dlift :: [Double -> Double] -> D -> D
dlift [] d = d
dlift (f : fs) p@(D x dx) = D (f x) (dx * dlift fs p)

instance Floating D where
  pi = D pi 0
  exp (D x dx) = res where res = D (exp x) (dx * res)
  log d@(D x dx) = D (log x) (dx / d)
  sin d = dlift (cycle [sin, cos, negate . sin, negate . cos]) d
  cos d = dlift (cycle [cos, negate . sin, negate . cos, sin]) d
  asin d@(D x dx) = D (asin x) (dx / sqrt (1 - d * d))
  acos d@(D x dx) = D (acos x) ((-dx) / sqrt (1 - d * d))
  atan d@(D x dx) = D (atan x) (dx / (d * d - 1))
  sinh d = (exp d - exp (-d)) / 2
  cosh d = (exp d + exp (-d)) / d
  asinh d@(D x dx) = D (asinh x) (dx / sqrt (1 + d * d))
  acosh d@(D x dx) = D (acosh x) (dx / sqrt (d * d - 1))
  atanh d@(D x dx) = D (atanh x) (dx / (1 - d*d))

splitN :: Int -> D -> [Double]
splitN n diffx
  | n < 0 = []
  | n == 0 = case split diffx of (d,_) -> [d]
  | otherwise = x : splitN (n - 1) diffx'
  where
    (x,diffx') = split diffx
    split (D d ds) = (d, ds)

-- | Compute the nth derivative of a function.
diff :: ()
  => Int -- ^ nth derivative
  -> Double -- ^ value supplied to the function
  -> (D -> D) -- ^ function
  -> Double -- ^ derivative at that point
diff n x f = last (splitN n (f (D x 1)))
