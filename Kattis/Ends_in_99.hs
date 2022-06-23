import Data.Maybe (fromJust)

main :: IO ()
main = do
  line <- getLine
  print $ solve $ read line

solve :: Integer -> Integer
solve price
  | minPrice <= 49 = maxPrice
  | maxPrice - price <= price - minPrice = maxPrice
  | otherwise = minPrice
  where
    maxPrice = maxLimit price
    minPrice = lowerLimit price

maxLimit :: Integer -> Integer
maxLimit number = 100 * ceil - 1
  where
    ceil = ceiling $ fromIntegral number / 100

lowerLimit :: Integer -> Integer
lowerLimit number = 100 * (number `div` 100) - 1
