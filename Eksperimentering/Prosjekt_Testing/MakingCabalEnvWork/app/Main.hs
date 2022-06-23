module Main where

import qualified MyLib (otherfunc, someFunc)
import qualified Shapes.Shape1 as Shape1 (Shape (..), add, dimensions)
import qualified SomeOtherLib as OLib (someOtherFunc)

main :: IO ()
main = do
  putStrLn $ (++ "\n") $ OLib.someOtherFunc "String" 6

  let square = Shape1.Square 3
  let square2 = Shape1.Square 3
  let circle = Shape1.Circle 2
  let rectangle = Shape1.Rect 4 2

  print $ Shape1.add square
  print $ Shape1.add circle
  print $ Shape1.add rectangle

  print $ Shape1.dimensions square
  print $ Shape1.dimensions circle
  print $ Shape1.dimensions rectangle

  print $ square == square2