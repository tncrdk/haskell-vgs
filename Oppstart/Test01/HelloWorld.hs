module Main where

main :: IO ()

f x y = 2 * x + 2 * y

tst a b =
  if a > b
    then a - b
    else a + b

main = do
  print "Hello World"
  print (f 1 2)