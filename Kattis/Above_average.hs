import qualified Text.Printf as Printf

solve :: [String] -> String
solve = concatMap $ Printf.printf "%.3f%%\n" . getResult . words

getResult :: [String] -> Double
getResult (pop_size : pop) = 100 * fromIntegral (length [student | student <- students, student > avg]) / num_students
  where
    num_students = read pop_size
    students = map read pop
    avg = sum students / num_students
getResult _ = error "her er det en feil"

main :: IO ()
main = interact $ solve . tail . lines
