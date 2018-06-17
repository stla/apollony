module Soddy.SoddyCircle
  where

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
