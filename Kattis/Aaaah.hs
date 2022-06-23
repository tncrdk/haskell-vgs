solve :: [String] -> String
solve (first : second : _)
  | length first >= length second = "go"
  | length first < length second = "no"
solve _ = error "Hvor er input?"

main :: IO ()
main = interact $ solve . words