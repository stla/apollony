module Soddy.SoddyCircle
  where
import Data.Complex

type Point2 = (Double,Double)

soddyRadius :: Double -> Double -> Double -> Double
soddyRadius r1 r2 r3 = 1/(1/r1+1/r2+1/r3+2*sqrt(1/r1/r2+1/r2/r3+1/r3/r1))

soddyCenter :: Point2 -> Point2 -> Point2 -> Point2
soddyCenter p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) =
  (k1*x1+k2*x2+k3*x3, k1*y1+k2*y2+k3*y3)
  where
  measure (x,y) (x',y') = sqrt((x-x')*(x-x')+(y-y')*(y-y'))
  a = measure p2 p3
  b = measure p1 p3
  c = measure p1 p2
  u1 = x2-x1
  u2 = y2-y1
  v1 = x3-x1
  v2 = y3-y1
  delta = 0.5*abs(u1*v2-u2*v1)
  tc1 = 1 + 2*delta/a/(b+c-a)
  tc2 = 1 + 2*delta/b/(c+a-b)
  tc3 = 1 + 2*delta/c/(a+b-c)
  den = a*tc1 + b*tc2 + c*tc3
  k1 = a*tc1/den
  k2 = b*tc2/den
  k3 = c*tc3/den

soddyCircle :: (Point2, Double) -> (Point2, Double) -> (Point2, Double)
            -> (Point2, Double)
soddyCircle (p1,r1) (p2,r2) (p3,r3) =
  (soddyCenter p1 p2 p3, soddyRadius r1 r2 r3)

-- case of an exterior circle c3
soddyCircle' :: (Point2, Double) -> (Point2, Double) -> (Point2, Double)
             -> (Point2, Double)
soddyCircle' (p1,r1) (p2,r2) (p3,r3) = sol
  where
  r = soddyRadius r1 r2 (-r3)
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
  sol = if d1 > d2
    then ((center1_x, center1_y),r)
    else ((center2_x, center2_y),r)
