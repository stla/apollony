module Apollony.Apollony1
  where
import Soddy.SoddyCircle (Point2, soddyRadius, soddyCenter)

apollony :: (Point2, Double) -> (Point2, Double) -> (Point2, Double) -> Int
         -> [(Point2, Double)]
apollony c1@(p1,r1) c2@(p2,r2) c3@(p3,r3) n =
  if n==1
    then [circle]
  else
    concat [ apollony c1 c2 circle (n-1)
           , apollony c1 circle c3 (n-1)
           , apollony circle c2 c3 (n-1) ]
  where
  circle = (soddyCenter p1 p2 p3, soddyRadius r1 r2 r3)

-- apollony :: (Point2, Double) -> (Point2, Double) -> (Point2, Double) -> Int
--          -> [(Point2, Double)]
-- apollony c1@(p1,r1) c2@(p2,r2) c3@(p3,r3) n =
--   if n==1
--     then [(soddyCenter p1 p2 p3, soddyRadius r1 r2 r3)]
--   else
--     concat [ apollony c1 c2 (soddyCenter p1 p2 p3, soddyRadius r1 r2 r3) (n-1)
--            , apollony c1 (soddyCenter p1 p2 p3, soddyRadius r1 r2 r3) c3 (n-1)
--            , apollony (soddyCenter p1 p2 p3, soddyRadius r1 r2 r3) c2 c3 (n-1) ]

fractal :: Int -> [(Point2, Double)]
fractal n = concatMap (apollony c1 c2 c3) [1..n]
  where
  c1 = ((1, -1/sqrt 3), 1)
  c2 = ((-1, -1/sqrt 3), 1)
  c3 = ((0, sqrt 3 - 1/sqrt 3), 1)
