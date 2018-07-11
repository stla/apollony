module Soddy.SoddyCircle2
  where
import Data.Complex
import Soddy.SoddyCircle (Point2)

soddyRadius :: Double -> Double -> Double -> Double -> Double
soddyRadius r1 r2 r3 sign =
  1/(1/r1+1/r2+1/r3 + sign*2*sqrt(1/r1/r2+1/r2/r3+1/r3/r1))

soddyCircle' :: (Point2, Double) -> (Point2, Double) -> (Point2, Double)
             -> Double -> (Point2, Double)
soddyCircle' (p1,r1) (p2,r2) (p3,r3) rother = sol
  where
  r0 = soddyRadius r1 r2 (-r3) 1
  (r,other) = if abs((r0-rother)/r0) < 0.001
    then (soddyRadius r1 r2 (-r3) (-1), True)
    else (r0, False)
  z1 = uncurry (:+) p1
  z2 = uncurry (:+) p2
  z3 = uncurry (:+) p3
  toCplx x = x :+ 0.0
  term1 = toCplx(1/r1) * z1 + toCplx(1/r2) * z2 - toCplx(1/r3) * z3
  term2 = 2*sqrt(toCplx(1/r1/r2) * z1*z2 - toCplx(1/r2/r3) * z2*z3 -
                 toCplx(1/r1/r3) * z1*z3)
  center1 = toCplx r * (term1-term2)
  center2 = toCplx r * (term1+term2)
  center1_x = realPart center1
  center1_y = imagPart center1
  center2_x = realPart center2
  center2_y = imagPart center2
  c3_x = fst p3
  c3_y = snd p3
  d1 = (center1_x - c3_x)*(center1_x - c3_x) +
       (center1_y - c3_y)*(center1_y - c3_y)
  d2 = (center2_x - c3_x)*(center2_x - c3_x) +
       (center2_y - c3_y)*(center2_y - c3_y)
  sol = if other
    then if d1 > d2
      then ((center1_x, center1_y),r)
      else ((center2_x, center2_y),r)
    else if d1 < d2
      then ((center1_x, center1_y),r)
      else ((center2_x, center2_y),r)
