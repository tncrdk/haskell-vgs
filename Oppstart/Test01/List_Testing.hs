elements :: (Eq a) => a -> [a] -> Bool
elements x [] = False
elements x (a : as)
  | x == a = True
  | otherwise = elements x as

sas :: Eq t => t -> [t] -> Bool
sas x [] = False
sas x (a : as) = (x == a) || sas x as

summary :: [Int] -> Int
summary [] = 0
summary (x : xs)
  | even x = summary xs
  | otherwise = x + summary xs

remove :: (Eq a) => [a] -> [a]
remove [] = []
remove (x : xs)
  | elements x xs = remove xs
  | otherwise = x : remove xs

ascending :: [Int] -> Bool
ascending [] = True
ascending [x] = True
ascending (x : y : xs) =
  x <= y && ascending (y : xs)

directedgraphs :: [(Int, Int)] -> Int -> Int -> Bool
directedgraphs [] a b = True
directedgraphs (x : xs) a b
  | a == b = True
  | a < b = elements (a, a + 1) (x : xs) && directedgraphs (x : xs) (a + 1) b
  | otherwise = elements (a, a -1) (x : xs) && directedgraphs (x : xs) (a -1) b

linkedGraph :: [(Int, Int)] -> Int -> Int -> Bool
linkedGraph xs a b
  | a == b = True
  | otherwise = or [linkedGraph xs' y b | (x, y) <- from_a]
  where
    from_a = [(n, m) | (n, m) <- xs, n == a && m /= a]
    xs' = [(n, m) | (n, m) <- xs, n /= a]

multiples :: Integer
multiples = (sum [3, 6 .. 999] + sum [5, 10 .. 999]) - sum [15, 30 .. 999]

daada :: IO ()
daada = do
  putStrLn "Hello"
  print multiples
  putStrLn "Bye"

egfoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
egfoldr k z [] = z
egfoldr k z (x : xs) = x `k` egfoldr k z xs

egenfoldr :: (t -> p -> p) -> p -> [t] -> p
egenfoldr k z = go
  where
    go [] = z
    go (y : ys) = y `k` go ys

{- [(0, 2), (0, 4), (0, 5), (1, 4), (1, 5), (2, 3), (2, 4), (4, 5), (0, 3), (3, 2)] -}
