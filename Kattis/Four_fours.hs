import Control.Monad (replicateM)
import Data.Char (isDigit)
import Data.Text (pack, replace, unpack)

solve :: [String] -> String
solve =
  foldr
    (\x -> (++) (checkNum x possibilities (getValues possibilities)))
    ""

checkNum :: String -> [String] -> [Int] -> String
checkNum numToCheck (e : exprs) (v : values)
  | read numToCheck == v = e ++ " = " ++ show v ++ "\n"
  | otherwise = checkNum numToCheck exprs values
checkNum _ _ _ = "no solution\n"

possibilities :: [String]
possibilities = newPos
  where
    newPos = [unpack $ replace (pack "`div`") (pack "/") (pack x) | x <- possibleExpr ["+", "-", "*", "`div`"] 0]

possibleExpr :: [String] -> Int -> [String]
possibleExpr os acc
  | acc >= 3 = ["4"]
  | otherwise = addEveryOperator os (possibleExpr os (acc + 1))

cat :: String -> String -> String
cat o elem = "4" ++ " " ++ o ++ " " ++ elem

addOperator :: String -> [String] -> [String]
addOperator o = map (cat o)

addEveryOperator :: [String] -> [String] -> [String]
addEveryOperator (o : os) expressionList =
  addOperator o expressionList
    ++ addEveryOperator os expressionList
addEveryOperator [] _ = []

getValues :: [String] -> [Int]
getValues exprs = [eval [4, 4, 4, 4] (words $ filter (not . isDigit) expr) | expr <- exprs]

eval :: [Int] -> [String] -> Int
eval (x1 : x2 : xs) (o1 : os)
  | o1 == "*" = eval ((x1 * x2) : xs) os
  | o1 == "/" = eval ((x1 `quot` x2) : xs) os -- `div` blir feil avrunding pga (-1)//4 er ikke det samme som -1//4
  | o1 == "+" = x1 + eval (x2 : xs) os
  | o1 == "-" = x1 + eval (- x2 : xs) os
eval [x] _ = x
eval _ _ = error "Her mangler det noe"

main :: IO ()
main = do
  line <- getLine
  let count :: Int
      count = read line
  input <- replicateM count $ do
    getLine
  putStr $ solve input

-- main :: IO ()
-- main = interact $ solve . tail . words