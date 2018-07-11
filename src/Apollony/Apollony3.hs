module Apollony.Apollony3
  where
import Soddy.SoddyCircle2 (soddyCircle')
import Soddy.SoddyCircle (Point2, soddyCircle)
import Data.Maybe
import ThreeCircles.ThreeCircles (threeCircles)

apollony :: (Point2, Double) -> (Point2, Double) -> (Point2, Double)
         -> Maybe (Point2, Double) -> Bool -> Int -> [(Point2, Double)]
apollony c1 c2 c3 cother exterior n =
  if n==1
    then [circle]
  else
    concat [ apollony c1 c2 circle Nothing False (n-1)
           , apollony c1 circle c3 cother exterior (n-1)
           , apollony circle c2 c3 cother exterior (n-1) ]
  where
  circle = if exterior
    then soddyCircle' c1 c2 c3 (snd (fromJust cother))
    else soddyCircle c1 c2 c3

fractal :: Int -> Double -> Double -> [(Point2, Double)]
fractal n phi beta = concatMap (apollony c1 c2 c3 Nothing False) [1..n] ++
                     concatMap (apollony c1 c2 c4 (Just c3) True) [1..n] ++
                     concatMap (apollony c1 c3 c4 (Just c2) True) [1..n] ++
                     concatMap (apollony c2 c3 c4 (Just c1) True) [1..n]
  where
  c4 = ((0,0), 1)
  (c1,c2,c3) = threeCircles phi c4 beta
