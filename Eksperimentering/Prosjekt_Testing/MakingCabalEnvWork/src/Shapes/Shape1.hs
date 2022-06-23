module Shapes.Shape1 (Shape (..), dimensions, add) where

data (Num a) => Shape a
  = Rect {width::a, length::a} 
  | Circle {radius::a}
  | Square {width::a}
  deriving (Show, Eq) 

dimensions :: (Num a) => Shape a -> [a]
dimensions (Rect w l) = [w, l]
dimensions (Circle r) = [r]
dimensions (Square w) = [w]

add :: (Num a) => Shape a -> Shape a
add (Rect w l) = Rect (w + 1) (l + 1)
add (Circle r) = Circle (r+1)
add (Square w) = Square (w+1)
