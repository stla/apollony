module ThreeCircles.Circumcircle
  where

det3x3 :: [[Double]] -> Double
det3x3 m =
  - m!!0!!2 * m!!1!!1 * m!!2!!0
  + m!!0!!1 * m!!1!!2 * m!!2!!0
  + m!!0!!2 * m!!1!!0 * m!!2!!1
  - m!!0!!0 * m!!1!!2 * m!!2!!1
  - m!!0!!1 * m!!1!!0 * m!!2!!2
  + m!!0!!0 * m!!1!!1 * m!!2!!2


circumcircle :: (Double,Double) -> (Double,Double) -> (Double,Double)
             -> ((Double,Double), Double)
circumcircle (x1,y1) (x2,y2) (x3,y3) = (center, radius)
  where
  a = det3x3 [[x1,x2,x3],[y1,y2,y3],[1,1,1]]
  q1 = x1*x1 + y1*y1
  q2 = x2*x2 + y2*y2
  q3 = x3*x3 + y3*y3
  dx = det3x3 [[q1,q2,q3],[y1,y2,y3],[1,1,1]]
  dy = det3x3 [[q1,q2,q3],[x1,x2,x3],[1,1,1]]
  cx = 0.5/a*dx
  cy = -0.5/a*dy
  center = (cx, cy)
  radius = sqrt((x1-cx)*(x1-cx) + (y1-cy)*(y1-cy))
