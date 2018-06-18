module Apollony.Apollony2
  where
import Soddy.SoddyCircle (Point2, soddyCircle, soddyCircle')

apollony :: (Point2, Double) -> (Point2, Double) -> (Point2, Double) -> Bool -> Int
         -> [(Point2, Double)]
apollony c1 c2 c3 exterior n =
  if n==1
    then [circle]
  else
    concat [ apollony c1 c2 circle False (n-1)
           , apollony c1 circle c3 exterior (n-1)
           , apollony circle c2 c3 exterior (n-1) ]
  where
  circle = if exterior
    then soddyCircle' c1 c2 c3
    else soddyCircle c1 c2 c3

fractal :: Int -> [(Point2, Double)]
fractal n = concatMap (apollony c1 c2 c3 False) [1..n] ++
            concatMap (apollony c1 c2 c4 True) [1..n] ++
            concatMap (apollony c1 c3 c4 True) [1..n] ++
            concatMap (apollony c2 c3 c4 True) [1..n]
  where
  c1 = ((1, -1/sqrt 3), 1)
  c2 = ((-1, -1/sqrt 3), 1)
  c3 = ((0, sqrt 3 - 1/sqrt 3), 1)
  c4 = ((0,0), 2 + 1/(3+2*sqrt 3))
