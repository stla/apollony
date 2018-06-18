module Apollony.Apollony1
  where
import Soddy.SoddyCircle (Point2, soddyCircle)

apollony :: (Point2, Double) -> (Point2, Double) -> (Point2, Double) -> Int
         -> [(Point2, Double)]
apollony c1 c2 c3 n =
  if n==1
    then [circle]
  else
    concat [ apollony c1 c2 circle (n-1)
           , apollony c1 circle c3 (n-1)
           , apollony circle c2 c3 (n-1) ]
  where
  circle = soddyCircle c1 c2 c3

fractal :: Int -> [(Point2, Double)]
fractal n = concatMap (apollony c1 c2 c3) [1..n]
  where
  c1 = ((1, -1/sqrt 3), 1)
  c2 = ((-1, -1/sqrt 3), 1)
  c3 = ((0, sqrt 3 - 1/sqrt 3), 1)
