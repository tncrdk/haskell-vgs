import Data.List (sort)

solve :: [Integer] -> String -> [Integer]
solve numbers (y : ys)
  | y == 'A' = minimum numbers : solve numbers ys
  | y == 'B' = sort numbers !! 1 : solve numbers ys
  | y == 'C' = maximum numbers : solve numbers ys
solve _ _ = []

format :: [Integer] -> String
format numbers = unwords $ map show numbers

getNumbers :: String -> [Integer]
getNumbers input = map (read :: String -> Integer) $ init $ words input

main :: IO ()
main = interact $ format . returnOutput
  where
    returnOutput input = solve (getNumbers input) (last $ words input)
