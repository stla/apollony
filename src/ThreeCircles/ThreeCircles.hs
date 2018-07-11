module ThreeCircles.ThreeCircles
  (threeCircles)
  where
import ThreeCircles.Circumcircle (circumcircle)

type Circle = ((Double,Double),Double)

inversion :: Double -> (Double,Double) -> Double -> (Double,Double)
          -> (Double, Double)
inversion phi (mx,my) radius (cx,cy) = (mmx,mmy)
  where
  invphi = 1/phi
  ix = invphi*radius
  k = radius*radius*(invphi*invphi-1)
  imx = mx - ix - cx
  imy = my - cy
  im2 = imx*imx + imy*imy
  mmx = ix + cx - k/im2*mx
  mmy = cy - k/im2*my

oneCircle :: Double -> (Double,Double) -> Double -> Double -> Circle
oneCircle phi center@(cx,cy) radius beta = ((ccx',ccy),rr)
  where
  sine = sin(pi/3)
  coef = 1/(1+sine)
  ptx = cx + coef * radius * cos beta
  pty = cy + coef * radius * sin beta
  r = coef * radius * sine
  p1 = (ptx + r, pty)
  p2 = (ptx, pty + r)
  p3 = (ptx - r, pty)
  q1 = inversion phi p1 radius center
  q2 = inversion phi p2 radius center
  q3 = inversion phi p3 radius center
  ((ccx,ccy), rr) = circumcircle q1 q2 q3
  ccx' = ccx - 2/phi*radius

threeCircles :: Double -> Circle -> Double -> (Circle, Circle, Circle)
threeCircles phi cbig shift = (c1,c2,c3)
  where
  c1 = oneCircle phi (fst cbig) (snd cbig) shift
  c2 = oneCircle phi (fst cbig) (snd cbig) (shift + 2*pi/3)
  c3 = oneCircle phi (fst cbig) (snd cbig) (shift + 4*pi/3)
